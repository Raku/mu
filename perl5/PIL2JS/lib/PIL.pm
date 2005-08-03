package PIL;
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
  SUBMACRO   => 7,
};

# Are we in a sublike thing? If yes, what sublevel does that thing have?
our $IN_SUBLIKE = undef;

# What's the name of the sub we're currently in?
our $CUR_SUBNAME;

# Our current pos?
our $CUR_POS  = bless [ "<unknown>", (0) x 4 ] => "PIL::MkPos";

# Are we in pilGlob?
our $IN_GLOBPIL;

# $A::B++, without a decl., is legal, but JS does't like this, so we have to
# declare all undeclared vars.
our %UNDECLARED_VARS;

# We predeclare all lexicals, to proper support
#   { my $a; sub foo { $a++ } }
# and $OUTER:: and $CALLER::.
our @ALL_LEXICALS;
# IDs of all scopes we're currently in (Array of Hash of [NormalVarname =>
# FixedVarname]), i.e. NormalVarname is $a and FixedVarname is $a_padid.
our @CUR_LEXSCOPES;
# ID supply
our $CUR_LEXSCOPE_ID;
our $LEXSCOPE_PREFIX;
# We've to backup some wars to make nested subcalls work (as we don't use JS'
# "lexical" vars anymore).
our @VARS_TO_BACKUP;

# Guard against reentrancy.
our $PROCESSING_HAS_STARTED;

sub lookup_var {
  my $name = shift;

  my $outer_level = $name =~ s/OUTER:://g;

  my @scopes = @CUR_LEXSCOPES[0 .. $#CUR_LEXSCOPES - $outer_level];
  for(reverse @scopes) {
    return $_->{$name} if $_->{$name};
  }

  # It's a global.
  return $name;
}

sub fail { die "*** $_[0]\n    at $CUR_POS\n" }

sub generic_catch {
  my ($level, $body, @vars_to_restore) = @_;

  $body = add_indent(1, $body);

  my $restores = join "; ", map {
    sprintf "%s = backup_%s", name_mangle($_), name_mangle($_);
  } @vars_to_restore;

  return sprintf <<EOF, $body, @vars_to_restore ? $restores : ""; }
try {
%s
} catch(err) {
  PIL2JS.call_chain.pop();
  %s;
  if(err instanceof PIL2JS.ControlException.ret && $level >= err.level) {
    return err.return_value;
  } else {
    throw err;
  }
}
EOF

sub fixup {
  local $_;
  return bless {
    pilMain => $_[0]->{pilMain}->fixup,
    pilGlob => [ map { $_->fixup } @{ $_[0]->{pilGlob} } ],
  } => "PIL";
}

sub as_js {
  my $self = shift;
  die unless $self->{pilMain}->isa("PIL::PStmts");
  die unless ref($self->{pilGlob}) eq "ARRAY";
  local $_;

  die "PIL::as_js is not reentrant, sorry...\n"
    if $PROCESSING_HAS_STARTED;
  local $PROCESSING_HAS_STARTED = 1;
  local $CUR_LEXSCOPE_ID        = 1;
  local $LEXSCOPE_PREFIX        = "";
  # I'll fill a unique id of the file we're processing in, to fix var stomping:
  # A.pm: my $a = 3          # ==> my $a_1 = 3;
  # B.pm: use A; my $a = 4;  # ==> my $a_1 = 4; XXX!

  my $fixed_tree = $self->fixup;
  warn "# Number of lexical scopes: $CUR_LEXSCOPE_ID\n";

  $IN_GLOBPIL++;
  my @glob_js = map { $_->as_js } @{ $fixed_tree->{"pilGlob"} };
  $IN_GLOBPIL = 0;
  my $main_js = $fixed_tree->{pilMain}->as_js;

  my $decl_js =
    "// Declaration of vars:\n" .
    join("\n", map {
      sprintf "var %s = %s;",
        name_mangle($_),
        undef_of($_);
    } keys %UNDECLARED_VARS, @ALL_LEXICALS, map { $_->[0] } @{ $fixed_tree->{"pilGlob"} }) .
    "\n// End declaration of vars.\n";
  %UNDECLARED_VARS = ();
  @ALL_LEXICALS    = ();

  my $init_js =
    "// Initialization of global vars and exportation of subs:\n" .
    join("\n", map {
      my $name = $_->[0];
      $name =~ /^(?:__init_|__export_)/ && $name !~ /import$/
        ? sprintf("%s.FETCH()([PIL2JS.Context.Void]);", PIL::name_mangle $name)
        : ();
    } @{ $fixed_tree->{"pilGlob" } })  .
    "\n// End of initialization of global vars and exportation of subs.\n";

  return sprintf <<EOF, $decl_js, add_indent(1, join "\n", @glob_js, $init_js, $main_js);
%s
PIL2JS.catch_all_exceptions(function () {
%s
});
EOF
}

# Possible contexts:
{
  package PIL::TCxt;
  
  sub cxt  { $_[0] }
  sub type { $_[0]->cxt->[0]->[0] }

  sub fixup { $_[0] }

  sub as_js {
    return sprintf "new PIL2JS.Box.Constant(new PIL2JS.Context({ main: %s, type: %s }))",
      PIL::doublequote($_[0]->main),
      defined $_[0]->type
        ? PIL::doublequote($_[0]->type)
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
{ package PIL::SubRoutine; our @ISA = qw<PIL::SubType>; sub as_constant { PIL::SUBROUTINE } }
{ package PIL::SubPrim;    our @ISA = qw<PIL::SubType>; sub as_constant { PIL::SUBPRIM } }
{ package PIL::SubBlock;   our @ISA = qw<PIL::SubType>; sub as_constant { PIL::SUBBLOCK } }
{ package PIL::SubPointy;  our @ISA = qw<PIL::SubType>; sub as_constant { PIL::SUBPOINTY } }
{ package PIL::SubMethod;  our @ISA = qw<PIL::SubType>; sub as_constant { PIL::SUBMETHOD } }
{ package PIL::SubMacro;   our @ISA = qw<PIL::SubType>; sub as_constant { PIL::SUBMACRO } }

# Returns the undef/zero/default container for a given variable type.
#   my $x;   # Really my $x = undef
#   my @x;   # Really my @x = ()
#   etc.
sub undef_of($) {
  return "new PIL2JS.Box(undefined)"
    if $_[0] =~ /(?:__init_|__export_)/; # minor hack

  my $sigil = substr $_[0], 0, 1;
  die "Sigil doesn't match /[\$&@%]/!\n" unless $sigil =~ /[\$&@%]/;
  return {
    '$' => 'new PIL2JS.Box(undefined)',
    '&' => 'new PIL2JS.Box(undefined)',
    '@' => 'new PIL2JS.Box([])',
    '%' => 'new PIL2JS.Box(new PIL2JS.Hash)',
  }->{$sigil};
}

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

use PIL::PApp;
use PIL::PAssign;
use PIL::PBind;
use PIL::PExp;
use PIL::PLit;
use PIL::PNil;
use PIL::PNoop;
use PIL::PPad;
use PIL::Params;
use PIL::PPos;
use PIL::PStmt;
use PIL::PStmts;
use PIL::PVal;
use PIL::PVar;
use PIL::Subs;

1;
