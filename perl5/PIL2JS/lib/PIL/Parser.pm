package PIL::Parser;
# This module parses PIL (as given by pugs -CPIL) into a structure of objects.
# Its design is slightly influenced by Haskell's excellent Parsec module.

use warnings;
use strict;

our $pil;

# Main class method: Parses the given PIL.
sub parse {
  my ($class, $str) = @_;

  die "No string to parse given!\n"     unless defined $str;
  die "PIL::Parser is not reentrant!\n" if     defined $pil;

  $pil    = $str;
  my $ret = record();
  undef $pil;

  return $ret;
}

# Like Parsec's string: Matches a given string or dies.
sub string {
  my $should = shift;
  my $got    = substr($pil, 0, length $should, "");

  if($got eq $should) {
    return $got;
  } else {
    die "Expected: \"$should\", got: \"$got\"!\n$pil";
  }
}

# Like Parsec's symbol: Matches a given string and eats following whitespace,
# or dies.
sub symbol {
  my $should = shift;
  string $should;
  $pil =~ s/^\s*//;
  return $should;
}

# Matches a sequence of /\w+/ and eating following whitespace. Returns the word
# matched.
sub word {
  $pil =~ s/^(\w+)\s*// or
    die "Expected: word!\n$pil";
  return $1;
}

# Execute $_[0], but pretend $_[0] didn't consume any input if $_[0] failed.
# Like Parsec's try.
sub try(&) {
  no warnings "recursion";
  my $code = shift;
  my $safe = $pil;
  my $ret  = eval { $code->() };

  if($@) {
    $pil = $safe;
    die $@;
  } else {
    return $ret;
  }
}

# Execute $_[0], but (always) pretend $_[0] didn't consume any input.
# Likes Parsec's lookAhead.
sub lookahead(&) {
  my $code = shift;
  my $safe = $pil;
  my $ret  = $code->();
  $pil     = $safe;
  return $ret;
}

# Tries all parsers in @_ in order until one doesn't fail.
sub choice {
  my @choices = @_;
  no warnings "recursion";

  my $ret;
  while(my $ch = shift @choices) {
    $ret = eval { $ch->() };
    return $ret unless $@;
  }

  die "No match.\n$pil";
}

# Tries to apply $_[0], but doesn't fail if $_[0] couldn't be applied.
sub optional(&) { my $code = shift; eval { try { $code->() } } };

# Tries to apply $_[0] as often as possible. Returns an arrayref of the results
# of $_[0].
sub many(&) {
  no warnings "recursion";
  my $code = shift;
  my @ret;

  while(1) {
    my $ret = eval { $code->() };
    return \@ret if $@;
    push @ret, $ret;
  }
}

# Parses a Haskell record, as given by Haskell's show function:
#   foo
#     { bar = baz
#     , baz, grtz = baka
#     }
sub record {
  no warnings "recursion";
  my $base = word();
  symbol "{";

  local $_;
  my %r = map { @$_ } @{( many {
    my $key = word();
    symbol "=";
    my $value = expr();
    optional { symbol "," };
    return [$key => $value];
  })};

  symbol "}";

  return bless \%r => "PIL::$base";
}

# Parses (foo).
sub braces {
  no warnings "recursion";
  symbol "(";
  my $exp = expr();
  symbol ")";
  $exp;
}

# Parses an "expression".
sub expr {
  no warnings "recursion";

  if($pil =~ /^\(/) {
    choice
      # Is the expression surrounded by parens?
      sub { try { braces() } },
      # Is it a tuple/quadruble/whatever?
      sub { try { tuple() } };
  } elsif($pil =~ /^"/) {
    # Is it a double quoted string?
    dqstring();
  } elsif($pil =~ /^[0-9]/) {
    # Is it a rational number (e.g. 3%2)?
    choice
      sub { try { rat() } },
      # Is it a normal number?
      sub { try { num() } };
  } elsif($pil =~ /^\[/) {
    # Is it a list (e.g. [foo, bar])?
    list();
  } elsif($pil =~ /^MkT?Param/) {
    # Is it a record update?
    my $word = choice
      sub { try { lookahead { symbol "MkTParam" } } },
      sub { try { lookahead { symbol "MkParam"  } } };

    return record();
  } else {
    # Default
    my $obj = bless [] => "PIL::" . (my $word = word());
    my %arity = (
      PNil  => 0, PNoop    => 0,
      PPos  => 3, PRawName => 1,
      PVal  => 1, PVar     => 1,
      PExp  => 1, PLit     => 1, PThunk => 1, PCode => 3,
      PStmt => 1, PStmts   => 2,
      PApp  => 4, PAssign  => 2, PBind  => 2,
      PPad  => 3, PSub     => 4, MkPos  => 5,
      SMy   => 0,

      TTailCall  => 1,
      TCxtItem   => 1, TCxtSlurpy => 1, TCxtVoid => 0, TCxtLValue => 1,
      CxtItem    => 1, CxtSlurpy  => 1, CxtVoid  => 0, CxtLValue  => 1,
      mkType     => 1,

      VInt    => 1, VRat   => 1, VNum => 1,
      VStr    => 1, VUndef => 0,
      VBool   => 1, VList  => 1,
      SubPrim => 0, SubRoutine => 0, SubBlock => 0, SubPointy => 0, SubMethod => 0,
      True    => 0, False => 0,
      Nothing => 0, Just  => 1,

      Noop    => 0,
      App     => 3,
      Syn     => 2,
      Cxt     => 2,
      Pos     => 2,
      Pad     => 3,
      Sym     => 3,
      Stmts   => 2,
      Prim    => 1,
      Val     => 1,
      Var     => 1,
      NonTerm => 1,
    );
    warn "Unknown arity: \"$word\"!\n" and exit unless defined $arity{$word};

    for(1..$arity{$word}) {
      my $r = eval { expr() };
      die $@ if $@;
      # return $obj if $@;
      push @$obj, $r;
    }
    return $obj;
  }
}

# Match a double quoted string, returning the string unquoted.
sub dqstring {
  string '"';
  $pil =~ s/^((?:(?:\\\\)*\\.|.)*?)"/"/ or die "Expected: double quoted string.\n$pil";
  my $str = $1;
  symbol '"';

  $str =~ s/\\(.)/"\"\\$1\""/eeg; # hack
  return $str;
}

# Match a number.
sub num {
  $pil =~ s/^(-?\d+)\s*// or die "Expected: number.\n$pil";
  return $1;
}

# Match a rational number (e.g. 3%2).
sub rat {
  my $e = num();
  symbol "%";
  my $f = num();
  return "$e%$f";
}

# Match a list (e.g. [foo, bar]).
sub list {
  no warnings "recursion";
  symbol "[";

  my $ret = many {
    my $r = expr();
    optional { symbol "," };
    $r;
  };

  symbol "]";
  return $ret;
}

# Match a n-tuple.
sub tuple {
  no warnings "recursion";
  symbol "(";

  my $ret = many {
    my $r = expr();
    optional { symbol "," };
    $r;
  };

  symbol ")";
  return $ret;
}

1;
