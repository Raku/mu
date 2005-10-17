#!/usr/bin/pugs

use v6;
use Test;

plan 8;

{
  my $a is env = 9;
  my $sub = sub { $CALLER::a };

  {
    my $a = 3;
    is $sub(), 3, 'basic $CALLER:: works';
  }
}

{
  my $a is env = 9;
  my $sub2 = sub { $CALLER::a };
  my $sub1 = sub {
    my $a is env = 10;
    $sub2();
  };

  {
    my $a = 11;
    is $sub1(), 10, '$CALLER:: with nested subs works';
  }
}

{
  my $a is env = 9;
  my $sub2 = sub { $CALLER::a };
  my $sub1 = sub ($a is env) {
    $sub2();
  };

  {
    my $a = 11;
    is $sub1(15), 15, '$CALLER:: works when accessing subparams, too';
  }
}

{
  my $get_caller = sub { return sub { $CALLER::CALLER::a } };
  my $sub1 = sub {
    my $a is env = 3;
    $get_caller();
  };
  my $sub2 = sub {
    my $a is env = 5;
    $get_caller();
  };

  my $result_of_sub1 = $sub1();
  my $result_of_sub2 = $sub2();

  # We can't use the more elegant dies_ok here as it would influence $CALLER::
  # calculation.
  ok !(try{ $result_of_sub1() }), '$CALLER::CALLER:: is recalculated on each access (1)';
  ok !(try{ $result_of_sub2() }), '$CALLER::CALLER:: is recalculated on each access (2)';
}

# L<S02/"Names" /The CALLER package refers to the lexical scope/>
{
  # $_ is always implicitly declared "is env".
  my sub foo () { $CALLER::_ }
  my sub bar () {
    $_ = 42;
    foo();
  }

  $_ = 23;
  is bar(), 42, '$_ is implicitly declared "is env" (1)';
}

{
  # $_ is always implicitly declared "is env".
  # (And, BTW, $_ is lexical.)
  my sub foo () { $_ = 17; $CALLER::_ }
  my sub bar () {
    $_ = 42;
    foo();
  }

  $_ = 23;
  is bar(), 42, '$_ is implicitly declared "is env" (2)';
}

{
  # ...but other vars are not
  my sub foo { my $abc = 17; $CALLER::_ }
  my sub bar {
    my $abc = 42;
    foo();
  }

  my $abs = 23;
  dies_ok { bar() },
    'vars not declared "is env" are not accessible via $CALLER::_';
}

=pod

Larry ruled that as erroneous.

15:13 < iblech> autrijus: :) BTW, WRT lex hoisting: sub foo { $CALLER::a }; { foo(); my $a
= 3; foo() }
15:13 < autrijus> iblech: larry ruled it as erroneous.
15:13 < autrijus> i.e. foo()'s behaviour is undefined.
15:14 < iblech> ok then :)
15:14 < autrijus> it's essential we do that because
15:14 < autrijus> foo($a, my $a)
15:14 < autrijus> is legal
15:14 < autrijus> and will be simply hazadrous to implement either way.
15:14 < autrijus> s/implement/mandate/

{
  if $*OS eq "browser" {  # test works under PIL2JS :)
    my $sub = sub { $CALLER::a };

    # No declaration of $a yet.
    dies_ok { $sub() }, '$CALLER:: dies when accessing not yet declared vars';

    my $a = 3;
    is $sub(), 3, '$CALLER:: works now (accessing a declared var)';
  } else {
    fail "Test loops infinitely";
    fail "Test loops infinitely";
  }
}
