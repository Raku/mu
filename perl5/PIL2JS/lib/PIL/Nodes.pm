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
  SUBMACRO   => 7,
};
our $IN_SUBLIKE = undef;
our $CUR_SUBNAME;
our $CUR_POS  = bless [ "<unknown>", (0) x 4 ] => "PIL::MkPos";
our $IN_GLOBPIL;
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
  $IN_GLOBPIL++;
  my @glob_js = map { $_->as_js } @{ $self->{"pilGlob"} };
  $IN_GLOBPIL = 0;
  my $main_js = $self->{pilMain}->as_js;

  my $decl_js =
    "// Declaration of undeclared vars:\n" .
    join("\n", map {
      sprintf "var %s = new PIL2JS.Box(undefined);",
        PIL::Nodes::name_mangle($_);
    } keys %UNDECLARED_VARS) .
    "\n// End declaration of undeclared vars.\n" .
    "// Declaration of global vars:\n" .
    "var " . join(", ", map { PIL::Nodes::name_mangle($_->[0]) } @{ $self->{"pilGlob"} }) . ";\n";
  %UNDECLARED_VARS = ();

  my $init_js =
    "// Initialization of global vars and exportation of subs:\n" .
    join("\n", map {
      my $name = $_->[0];
      $name =~ /^(?:__init_|__export_)/
        ? sprintf("%s.FETCH()([PIL2JS.Context.Void]);", PIL::Nodes::name_mangle $name)
        : ();
    } @{ $self->{"pilGlob" } })  .
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
{ package PIL::SubMacro;   our @ISA = qw<PIL::SubType>; sub as_constant { PIL::Nodes::SUBMACRO } }

# Returns the undef/zero/default container for a given variable type.
#   my $x;   # Really my $x = undef
#   my @x;   # Really my @x = ()
#   etc.
sub undef_of($) {
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

use PIL::Nodes::PApp;
use PIL::Nodes::PAssign;
use PIL::Nodes::PBind;
use PIL::Nodes::PExp;
use PIL::Nodes::PLit;
use PIL::Nodes::PNil;
use PIL::Nodes::PNoop;
use PIL::Nodes::PPad;
use PIL::Nodes::PParams;
use PIL::Nodes::PPos;
use PIL::Nodes::PStmt;
use PIL::Nodes::PStmts;
use PIL::Nodes::PVal;
use PIL::Nodes::PVar;
use PIL::Nodes::Subs;

1;
