package PIL::Nodes;
# This module provides ->as_js methods for the objects returned by PIL::Parser.

use warnings;
use strict;

# State:
use constant {
  SUBTHUNK   => 1,
  SUBPRIM    => 2,
  SUBBLOCK   => 3,
  SUBPOINTY  => 4,
  SUBROUTINE => 5,
  SUBMETHOD  => 6,
};
our $IN_SUBLIKE = undef;
our $CUR_SUBNAME;
our $CUR_POS  = bless [ "<unknown>", (0) x 4 ] => "PIL::MkPos";
our %UNDECLARED_VARS;
our $PROCESSING_HAS_STARTED;

our $FAIL     = sub { die "*** $_[0]\n    at $CUR_POS\n" };

sub generic_catch {
  my ($level, $body) = @_;

  $body = add_indent(1, $body);

  return sprintf <<EOF, $body; }
try {
%s
} catch(err) {
  PIL2JS.call_chain.pop();
  if(err instanceof PIL2JS.ControlException.ret && $level >= err.level) {
    return err.return_value;
  } else {
    throw err;
  }
}
EOF

sub as_js {
  my $self = shift;
  die unless $self->{pilMain}->isa("PIL::PStmts");
  die unless ref($self->{pilGlob}) eq "ARRAY";

  die "PIL::Nodes::as_js is not reentrant, sorry...\n"
    if $PROCESSING_HAS_STARTED;
  local $PROCESSING_HAS_STARTED = 1;

  local $_;
  my @glob_js = map { $_->as_js } @{ $self->{"pilGlob"} };
  my $main_js = sprintf <<EOF, add_indent(1, $self->{pilMain}->as_js);
try {
%s
} catch(err) {
  alert(err);
}
EOF

  my $decl_js =
    "// Declaration of undeclared vars:\n" .
    join("\n", map {
      sprintf "var %s = new PIL2JS.Box(undefined);",
        PIL::Nodes::name_mangle($_);
    } keys %UNDECLARED_VARS) .
    "\n// End declaration of undeclared vars.\n";
  %UNDECLARED_VARS = ();

  my $init_js =
    "// Initialization of global vars and exportation of subs:\n" .
    join("\n", map {
      my $name = $_->[0];
      $name =~ /^(?:__init_|__export_)/
        ? sprintf("%s.GET()([PIL2JS.Context.Void]);", PIL::Nodes::name_mangle $name)
        : ();
    } @{ $self->{"pilGlob" } })  .
    "\n// End of initialization of global vars and exportation of subs.\n";

  return join "\n", $decl_js, @glob_js, $init_js, $main_js;
}

# Possible contexts:
{
  package PIL::TCxt;
  
  sub cxt  { $_[0] }
  sub type { $_[0]->cxt->[0]->[0] }

  sub as_js {
    return sprintf "new PIL2JS.Box.Constant(new PIL2JS.Context({ main: %s, type: %s }))",
      PIL::Nodes::doublequote($_[0]->main),
      defined $_[0]->type
        ? PIL::Nodes::doublequote($_[0]->type)
        : "undefined";
  }
}

{ package PIL::TCxtVoid;   our @ISA = qw<PIL::TCxt>; sub main { "void" } }
{ package PIL::TCxtLValue; our @ISA = qw<PIL::TCxt>; sub main { "lvalue" } }
{ package PIL::TCxtItem;   our @ISA = qw<PIL::TCxt>; sub main { "item" } }
{ package PIL::TCxtSlurpy; our @ISA = qw<PIL::TCxt>; sub main { "slurpy" } }
{
  package PIL::TTailCall;
  
  our @ISA = qw<PIL::TCxt>;

  sub cxt  { $_[0]->[0]->cxt  }
  sub main { $_[0]->cxt->main }
}

# Possible subroutine types:
{ package PIL::SubType }
{ package PIL::SubRoutine; our @ISA = qw<PIL::SubType>; sub as_constant { PIL::Nodes::SUBROUTINE } }
{ package PIL::SubPrim;    our @ISA = qw<PIL::SubType>; sub as_constant { PIL::Nodes::SUBPRIM } }
{ package PIL::SubBlock;   our @ISA = qw<PIL::SubType>; sub as_constant { PIL::Nodes::SUBBLOCK } }
{ package PIL::SubPointy;  our @ISA = qw<PIL::SubType>; sub as_constant { PIL::Nodes::SUBPOINTY } }
{ package PIL::SubMethod;  our @ISA = qw<PIL::SubType>; sub as_constant { PIL::Nodes::SUBMETHOD } }

# Doublequotes an input string, e.g. foo""bar -> foo\"\"bar
sub doublequote($) {
  my $str = shift;

  $str =~ s/((?:[^\w0-9_,.=:; ()\[\]{}+\*\/~\-]|\n))/sprintf "\\x%02x", ord $1/eg;
  return "\"$str\"";
}

# Mangles a subroutine or variable name.
# XXX: Hacky, will need a rewrite later.
sub name_mangle($) {
  my $str = shift;

  # ::JS::Root:: ::= ::*::;
  if($str =~ /^&\*?JS::Root::(.+)$/) {
    $str = "&main::$1";
  # ::JS::native_js_function
  } elsif($str =~ /^[\&\$\@\+\%\:]\*?JS::(.+)$/) {
    return $1;
  # No qualification? Use "main" as package name. XXX! Lexical variables?
  } elsif($str !~ /::/) {
    $str = 
      substr($str, 0, 1) .
      "main::" .
      substr($str, 1);
  }

  # Finally, escape special chars.
  $str =~ s/([^\w0-9])/sprintf "_%02x", ord $1/eg;
  return $str;
}

# Add indentation to input text $text.
sub add_indent {
  my ($i, $text) = @_;
  local $_;

  my $INDENT = 2;
  return join "\n", map { " " x ($i * $INDENT) . $_ } split "\n", $text;
}

# The lines like
#   die unless $self->[...]->isa(...)
# make sure that PIL::Parser gave us correct nodes.
{
  package PIL::PStmts;

  sub as_js {
    my $self = shift;
    die unless @$self == 2;

    # Update $?POSITION.
    my $pos =
      sprintf "_24main_3a_3a_3fPOSITION.STORE(new PIL2JS.Box.Constant(%s))",
      PIL::Nodes::doublequote $CUR_POS;

    # Add a &return() to the last statement of a sub.
    if($IN_SUBLIKE and $self->[1]->isa("PIL::PNil")) {
      my $js = $self->[0]->as_js;
      # Note: Purely cosmetical hacking on the generated JS! (else it would be
      # eevil).
      $js =~ s/\n$//;
      if($IN_SUBLIKE >= PIL::Nodes::SUBROUTINE) {
        return "$pos;\n_26main_3a_3areturn.GET()([PIL2JS.Context.ItemAny, $js]);";
      } elsif($IN_SUBLIKE >= PIL::Nodes::SUBBLOCK) {
        return "$pos;\n_26main_3a_3aleave.GET()([PIL2JS.Context.ItemAny, $js]);";
      } else {
        return "$pos;\nreturn($js);";
      }
    } else {
      my @js = ($self->[0]->as_js, $self->[1]->as_js);
      $js[0] =~ s/\n$//;
      return "$pos;\n$js[0];\n$js[1]";
    }
  }
}

{
  package PIL::PNoop;

  sub as_js {
    die unless @{$_[0]} == 0;
    return "new PIL2JS.Box.Constant(undefined)";
  }
}

# PPos -- simply ignore the position and ->as_js the real contents.
{
  package PIL::PPos;

  sub as_js {
    my $self = shift;
    die unless @$self == 3;
    die unless $self->[0]->isa("PIL::MkPos");

    local $CUR_POS = $self->[0];
    return $self->[2]->as_js;
  }
}

{
  package PIL::MkPos;

  use overload '""' => \&as_string;

  sub as_string {
    my $self = shift;
    die unless @$self == 5;

    my ($file, $line_start, $column_start, $line_end, $column_end) = @$self;
    return "$file line $line_start-$line_end, column $column_start-$column_end";
  }
}

{
  package PIL::PStmt;

  sub as_js {
    my $self = shift;
    die unless @$self == 1;

    return $self->[0]->as_js . "\n";
  }
}

{
  package PIL::PNil;

  sub as_js {
    my $self = shift;
    die unless @$self == 0;

    return "" unless $IN_SUBLIKE;
    return "_26main_3a_3areturn.GET()([PIL2JS.Context.ItemAny, new PIL2JS.Box.Constant(undefined)]);"
      if $IN_SUBLIKE >= PIL::Nodes::SUBROUTINE;
    return "_26main_3a_3aleave.GET()([PIL2JS.Context.ItemAny, new PIL2JS.Box.Constant(undefined)]);"
      if $IN_SUBLIKE >= PIL::Nodes::SUBBLOCK;
    return "return(new PIL2JS.Box.Constant(undefined));"
      if $IN_SUBLIKE >= PIL::Nodes::SUBTHUNK;
  }
}

{
  package PIL::PExp;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    return $self->[0]->as_js;
  }
}

{
  package PIL::PApp;

  sub as_js {
    my $self = shift;

    die unless @$self == 4;
    die unless $self->[0]->isa("PIL::TCxt");
    # die unless $self->[1]->isa("PIL::PExp") or $self->[1]->isa("PIL::PCode");
    die unless ref($self->[3]) eq "ARRAY";

    local $_;

    my $subname;
    if($self->[1]->[0]->isa("PIL::PVar") and not ref $self->[1]->[0]->[0]) {
      $subname = $self->[1]->[0]->[0];
    }

    my $obj;
    if(
      $self->[2]->isa("PIL::Just")           and
      $self->[2]->[0]->isa("PIL::PExp")      and
      $self->[2]->[0]->[0]->isa("PIL::PVar") and
      not ref $self->[2]->[0]->[0]->[0]      and
      1
    ) {
      $obj = $self->[2]->[0]->[0]->[0];
    }

    # XXX HACK! Support for &JS::inline.
    if(
      defined $subname                                 and
      $subname eq "&JS::inline"                        and
      $self->[3]->[0]->isa("PIL::PPos")                and
      $self->[3]->[0]->[2]->isa("PIL::PLit")           and
      $self->[3]->[0]->[2]->[0]->isa("PIL::PVal")      and
      $self->[3]->[0]->[2]->[0]->[0]->isa("PIL::VStr") and
      1
    ) {
      return $self->[3]->[0]->[2]->[0]->[0]->[0];
    } elsif(defined $subname and $subname eq "&JS::inline") {
      $FAIL->("Invalid use of &JS::inline!");
    }

    my $native;
    # true              ==> It's a call to a native JavaScript func.
    # false but defined ==> It's a call to a Perl 6 func.
    # undefined         ==> We don't know at compile-time.

    # Call to JS::foo? ==> It's a native call.
    $native = $subname =~ /^[\&\$\@\+\%\:]\*?JS::/
      if defined $subname;

    # We have an invocant? ==> We can't say for sure at compile-time.
    $native = undef
      if $self->[2]->isa("PIL::Just");

    # We have an invocant *and* the invocant is in the JS:: namespace? ==> It's
    # a native call.
    $native++ if
      defined $obj and $obj =~ /^[\&\$\@\+\%\:]\*?JS::/;

    # The sub is a reference? ==> We can't know at compile-time.
    $native = undef
      if not defined $subname or $subname =~ /^\$/;

    # Sanitize $subname.
    $FAIL->("When calling a method, the method name must be a simple string!")
      if $self->[2]->isa("PIL::Just") and (not defined $subname or $subname !~ /^&/);
    $subname = "&$1" if $self->[2]->isa("PIL::Just") and $subname =~ /^&(.+)$/;
    $subname =~ s/^(.)\*?JS::/$1/ if defined $subname;

    # Go!
    my $inv = $self->[2]->isa("PIL::Just") ? $self->[2]->[0]->as_js : "";
    my $sub = $inv || $native ? substr($subname, 1) : $self->[1]->as_js;
    my @arg = map { $_->as_js } @{ $self->[3] };
    @arg    = map { "($_).toNative()" } @arg if $native;
    my $arg = PIL::Nodes::add_indent(1, join ",\n", @arg);
    my $cxt = PIL::Nodes::add_indent(1, $self->[0]->as_js);

    # XXX Context handling!
    if($inv) {
      return "new PIL2JS.Box.Constant($inv.$sub(\n$arg\n))" if $native;
      return sprintf "%s.perl_methods[%s]([\n%s,\n%s\n])",
        $inv, PIL::Nodes::doublequote($sub), $cxt, $arg
        if defined $native;
      return sprintf "PIL2JS.call(%s, %s, [\n%s,\n%s\n])",
        $inv, PIL::Nodes::doublequote($sub), $cxt, $arg;
    } else {
      return "new PIL2JS.Box.Constant($sub(\n$arg\n))" if $native;
      return "$sub.GET()([\n$cxt,\n$arg\n])"           if defined $native;
      return sprintf "PIL2JS.call(undefined, %s, [\n%s,\n%s\n])", $sub, $cxt, $arg;
    }
  }
}

{
  package PIL::PVar;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    die if     ref $self->[0];

    return PIL::Nodes::name_mangle($self->[0]);
    # return "__pil2js_lookup(" . PIL::Nodes::doublequote($self->[0]) . ")";
  }
}

{
  package PIL::PLit;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    die unless $self->[0]->isa("PIL::PVal");
    return $self->[0]->as_js;
  }
}

{
  package PIL::PVal;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    die unless $self->[0]->isa("PIL::PVal");
    return $self->[0]->as_js;
  }
}

# Basic datatypes (VInt, VRat, VStr, VUndef). Important: Box the things with
# __pil2js_box!
{
  package PIL::VInt;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    die if     ref $self->[0];
    return "new PIL2JS.Box.Constant($self->[0])";
  }
}

{
  package PIL::VRat;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    die if     ref $self->[0];
    return sprintf "new PIL2JS.Box.Constant(%s)", join "/", split "%", $self->[0];
  }
}

{
  package PIL::VStr;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    die if     ref $self->[0];
    return sprintf "new PIL2JS.Box.Constant(%s)", PIL::Nodes::doublequote $self->[0];
  }
}

{
  package PIL::VBool;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;
    die unless @$self == 1;

    return sprintf "new PIL2JS.Box.Constant(%s)",
      $self->[0]->isa("PIL::True")  ? "true"  :
      $self->[0]->isa("PIL::False") ? "false" : die;
  }
}

{
  package PIL::VList;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;
    die unless @$self == 1;

    local $_;
    return sprintf "new PIL2JS.Box.Constant([%s])",
      join ", ", map { $_->as_js } @{ $self->[0] };
  }
}

{
  package PIL::VUndef;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    die unless @$self == 0;
    return "new PIL2JS.Box.Constant(undefined)";
  }
}

# Real subroutines, blocks, thunks.
# Back on days 1 and 2 of PIL2JS, the eval()s were needed to emulate Perl's
# "the return value of a sub is the value of the last expression evaluated":
#   (function () { 42 })()                 # undefined
#   (function () { return eval "42" })()   # 42
# Now PIL::PStmts wraps a return() around the last statement, but only if we're
# $IN_SUBLIKE. :)
{
  package PIL::PSub;

  sub as_js {
    my $self = shift;
    die unless @$self == 4;
    die if     ref $self->[0];
    die unless $self->[1]->isa("PIL::SubType");
    die unless ref($self->[2]) eq "ARRAY" or $self->[2]->isa("PIL::Params");
    bless $self->[2] => "PIL::Params";

    local $IN_SUBLIKE  = $self->[1]->as_constant;
    local $CUR_SUBNAME = $self->[0];

    # Subbody
    local $_;
    my $body = sprintf "PIL2JS.call_chain.push(%s);\n%s;\n%s;",
      PIL::Nodes::name_mangle($self->[0]),
      $self->[2]->as_js,
      $self->[3]->as_js;

    # Sub declaration
    my $js = sprintf
      "var %s = new PIL2JS.Box.Constant(function (args) {\n%s\n});\n",
      PIL::Nodes::name_mangle($self->[0]),
      PIL::Nodes::add_indent 1, PIL::Nodes::generic_catch($IN_SUBLIKE, $body);
    $js .= sprintf
      "%s.perl_name = %s;\n",
      PIL::Nodes::name_mangle($self->[0]),
      PIL::Nodes::doublequote($self->[0]);
    $js .= sprintf
      "%s.arity = %d;\n",
      PIL::Nodes::name_mangle($self->[0]),
      $self->[2]->arity;

    # Special magic for methods.
    if($self->[1]->isa("PIL::SubMethod")) {
      my $methname = $self->[0];
      $methname = ($methname =~ /^&.*::(.+)$/)[0] or
        $FAIL->("Method names must be simple strings!");
      $js .= sprintf
        "PIL2JS.Box.prototype.perl_methods[%s] = %s;\n",
        PIL::Nodes::doublequote($methname),
        PIL::Nodes::name_mangle($self->[0]);
    }

    return $js;
  }
}

{
  package PIL::PCode;

  sub as_js {
    my $self = shift;

    die unless @$self == 3;
    die unless $self->[0]->isa("PIL::SubType");
    die unless ref($self->[1]) eq "ARRAY" or $self->[1]->isa("PIL::Params");
    bless $self->[1] => "PIL::Params";

    local $IN_SUBLIKE  = $self->[0]->as_constant;
    local $CUR_SUBNAME = "<anonymous@{[$CUR_SUBNAME ? ' in ' . $CUR_SUBNAME : '']}>";

    # Subbody
    local $_;
    my $body = sprintf "%s;\n%s;",
      $self->[1]->as_js,
      $self->[2]->as_js;

    # Sub declaration
    return sprintf "PIL2JS.Box.constant_func(%d, function (args) {\n%s\n})",
      $self->[1]->arity,
      PIL::Nodes::add_indent 1, PIL::Nodes::generic_catch($IN_SUBLIKE, $body);
  }
}

{
  package PIL::PThunk;

  sub as_js {
    my $self = shift;
    local $IN_SUBLIKE  = PIL::Nodes::SUBTHUNK;
    local $CUR_SUBNAME = "<thunk@{[$CUR_SUBNAME ? ' in ' . $CUR_SUBNAME : '']}>";

    die unless @$self == 1;

    local $_;
    return sprintf "PIL2JS.Box.constant_func(0, function (args) { var cxt = args.shift(); return(%s); })",
      $self->[0]->as_js;
  }
}

# Parameters
{
  package PIL::Params;

  sub as_js {
    my $self = shift;
    local $_;

    # The parameter extracting thing is 3-pass.
    # Firstly, in as_js1, possible named args are immediately extracted and
    # removed from args.
    # Then, in as_js2, remaing positional args are picked up.
    # Finally,in as_js3, the actual checking is done.
    my $js;
    $js .= "var cxt   = args.shift();\n";
    $js .= "args      = PIL2JS.possibly_flatten(args);\n";
    $js .= "var pairs = PIL2JS.grep_for_pairs(args);\n";
    $js .= $_->as_js1() . "\n" for @$self;
    $js .= $_->as_js2() . "\n" for @$self;
    $js .= $_->as_js3() . "\n" for @$self;
    $js .= <<EOF;
if(args.length != 0)
  PIL2JS.die(
    "" +
    args.length +
    " more parameters passed to sub " +
    @{[PIL::Nodes::doublequote $CUR_SUBNAME]} +
    " than expected (@{[scalar @$self]})!"
  );
EOF

    return $js;
  }

  sub arity {
    local $_;
    my $arity = 0;
    $_->is_required and $arity++ for @{ $_[0] };
    return $arity;
  }
}

{
  package PIL::MkTParam;

  sub is_required { $_[0]->{tpParam}{isOptional}->isa("PIL::False") }

  sub as_js1 {
    my $self = shift;
    my $name = $self->{tpParam}{paramName};
    die unless defined $name and not ref $name;
    warn "Skipping \%_ parameter.\n" and return "" if $name eq "%_";

    my $jsname   = PIL::Nodes::name_mangle $name;
    my $pairname = PIL::Nodes::doublequote(substr $name, 1);
    return <<EOF;
var $jsname = undefined;
if(pairs[$pairname] != undefined) {
  $jsname = pairs[$pairname];
  args = PIL2JS.delete_pair_from_args(args, $pairname);
}
EOF
  }

  sub as_js2 {
    my $self = shift;
    my $name = $self->{tpParam}{paramName};
    die unless defined $name and not ref $name;
    warn "Skipping \%_ parameter.\n" and return "" if $name eq "%_";

    # If we're a name-only arg, skip as_js2.
    #return "" if
    #  !$self->{tpParam}{isNamed}->isa("PIL::True") and not
    #  $name eq '$_';

    # It's a slurpy parameter? Flatten args so we can .shift() one item at a
    # time.
    my @js;
    if($self->{tpParam}{paramContext}->isa("PIL::CxtSlurpy")) {
      push @js, "args = PIL2JS.make_slurpy_array(args);\n";
    }

    my $jsname = PIL::Nodes::name_mangle $name;
    # We're a take-everything slurpy param (*@foo, as opposed to *$foo)?
    if(
      not $self->{tpParam}{paramContext}->isa("PIL::CxtSlurpy") or
      $name !~ /^@/
    ) {
      push @js, "if($jsname == undefined) $jsname = args.shift();";
    } else {
      push @js, "if($jsname == undefined) { $jsname = new PIL2JS.Box.Constant(args); args = [] }";
    }

    return join "\n", @js;
  }

  sub as_js3 {
    # - !perl/PIL::MkTParam
    # tpDefault: !perl/@PIL::Nothing []
    # tpParam: !perl/PIL::MkParam
    #   isInvocant: !perl/@PIL::False []
    #   isLValue: !perl/@PIL::True []
    #   isLazy: !perl/@PIL::False []
    #   isNamed: !perl/@PIL::True []
    #   isOptional: !perl/@PIL::False []
    #   isWritable: !perl/@PIL::False []
    #   paramContext: !perl/@PIL::CxtSlurpy
    #     - !perl/@PIL::mkType
    #       - Array
    #   paramDefault: !perl/@PIL::Noop []
    #   paramName: '@c'
    my $self = shift;
    my $name = $self->{tpParam}{paramName};
    die unless defined $name and not ref $name;
    warn "Skipping \%_ parameter.\n" and return "" if $name eq "%_";

    my @js;
    my $jsname = PIL::Nodes::name_mangle $name;

    # We're required, but a value hasn't been supplied?
    if($self->{tpParam}{isOptional}->isa("PIL::False")) {
      push @js,
        "if($jsname == undefined) " .
        "PIL2JS.die(\"Required parameter \\\"$name\\\" not passed!\");";
    }

    # Should we (and can we) supply a default for an optional param?
    if($self->{tpDefault}->isa("PIL::Just")) {
      push @js,
        "if($jsname == undefined) " .
        "$jsname = " . $self->{tpDefault}->[0]->as_js . ";";
    }

    # is copy?
    if($self->{tpParam}{isLValue}->isa("PIL::False")) {
      push @js, "$jsname = $jsname.clone();";
    }

    # is rw?
    if($self->{tpParam}{isWritable}->isa("PIL::False")) {
      push @js, "$jsname = new PIL2JS.Box.ReadOnly($jsname);";
    }

    # Always bind $?SELF to us, if we're the invocant param.
    if($self->{tpParam}{isInvocant}->isa("PIL::True")) {
#      my $tree = bless [
#        bless [] => "PIL::SMy",
#        [bless ['$?SELF', bless ['$?SELF'] => "PIL::PRawName"]],
#        bless [
#          bless [
#            bless [
#              bless [
#                [bless ['$?SELF'] => "PIL::PVar"],
#                bless [
#                  bless [] => "PIL::MkPos",
#                  bless [bless 
#                ] => "PIL::PPos",
#              ] => "PIL::PBind",
#            ] => "PIL::PExp",
#          ] => "PIL::PStmt",
#          bless [] => "PIL::PNil",
#        ] => "PIL::PStmts",
#      ] => "PIL::PPad";
    }

    return join "\n", @js;
  }
}

# my $x = ...
{
  package PIL::PPad;

  sub as_js {
    my $self = shift;

    die unless @$self == 3;
    die unless $self->[0]->isa("PIL::SMy");
    die unless ref $self->[1] eq "ARRAY";
    die unless $self->[2]->isa("PIL::PStmts");

    # Emit appropriate var foo = new PIL2JS.Box(undefined) statements.
    local $_;
    return
      "var " .
      join(", ", map {
        my $sigil = substr $_->[0], 0, 1;
        my %undef = (
          '$' => "undefined",
          '@' => "[]",
          '%' => "new PIL2JS.Hash",
          '&' => "undefined",
        );
        PIL::Nodes::name_mangle($_->[0]) .
        " = new PIL2JS.Box(@{[ $undef{$sigil} || die ]})"
      } @{ $self->[1] }) .
      ";\n" .
      $self->[2]->as_js;
  }
}

# Assignment.
{
  package PIL::PAssign;

  sub as_js {
    my $self = shift;

    die unless @$self == 2;
    die unless ref $self->[0] eq "ARRAY";
    die unless @{ $self->[0] } == 1;

    # Hack? Fully qualified variables don't need a declaration, but JavaScript
    # needs one.
    if($self->[0]->[0]->isa("PIL::PVar")) {
      my $varname = $self->[0]->[0]->[0];
      if($varname =~ /::/) {
        $UNDECLARED_VARS{$varname}++;
      }
    }

    return sprintf "%s.STORE(%s)",
      $self->[0]->[0]->as_js,
      $self->[1]->as_js;
  }
}

# Binding.
{
  package PIL::PBind;

  sub as_js {
    my $self = shift;

    die unless @$self == 2;
    die unless ref $self->[0] eq "ARRAY";
    die unless @{ $self->[0] } == 1;

    # Hack? Fully qualified variables don't need a declaration, but JavaScript
    # needs one.
    if($self->[0]->[0]->isa("PIL::PVar")) {
      my $varname = $self->[0]->[0]->[0];
      if($varname =~ /::/) {
        $UNDECLARED_VARS{$varname}++;
      }
    }

    return sprintf "%s.BINDTO(%s)",
      $self->[0]->[0]->as_js,
      $self->[1]->as_js;
  }
}

1;
