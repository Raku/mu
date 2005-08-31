#!/usr/bin/pugs

use v6;
use Test;

plan 35;

# L<S02/Out-of-scope names>
ok !eval('module MY;     1'), "MY is an out of scope name";
ok !eval('module OUTER;  1'), "OUTER is an out of scope name";
ok !eval('module CALLER; 1'), "CALLER is an out of scope name";


# L<S04/The Relationship of Blocks and Declarations>
ok !eval('my $x; my $x; 1'),       'it is illegal to declare $x twice in the same scope.', :todo<bug>;
ok !eval('state $x; state $x; 1'), 'it is illegal to declare $x twice in the same scope.', :todo<bug>;

# XXX -- dunno why test test fails, but the next outer test works. --iblech
{ my $a = 1; {
   my $a=2; {
      my $a=3;
      is($a, 3,               'get regular a'); 
      is($OUTER::a, 2,        'get $OUTER::a'); 
      is($OUTER::OUTER::a, 1, 'get $OUTER::OUTER::a', :todo<bug>);
}}}

{
  my $a = 1;
  is $a, 1, 'get regular $a (1)';

  {
    is $a, 1, 'get regular $a (2)';
    my $a = 2;
    is $a, 2, 'get new regular $a (1)';

    {
      is $a, 2, 'get new regular $a (2)';
      my $a = 3;

      is $a,               3, 'get very new regular $a';
      is $OUTER::a,        2, 'get $OUTER::a';
      is $OUTER::OUTER::a, 1, 'get $OUTER::OUTER::a';
    }
  }
}

{
  my $a = 3;
  my $sub = { $a++ };

  {
    my $a = -10;
    is $a, -10,   'get regular $a';
    is $sub(), 3, 'get hidden $a (1)';
    is $sub(), 4, 'get hidden $a (2)';
    is $sub(), 5, 'get hidden $a (3)';
  }
}

{
  my $sub = -> $stop {
    my $x = 3;
    if $stop {
      $x++;
    } else {
      $sub(1);
      $x;
    }
  };

  is $sub(0), 3,
    "recursively called subref shouldn't stomp on the lexical vars of other instances";
}

{
  sub stomptest ($stop) {
    my $x = 3;
    if $stop {
      $x++;
    } else {
      stomptest 1;
      $x;
    }
  };

  is stomptest(0), 3,
    "recursively called sub shouldn't stomp on the lexical vars of other instances";
}

{
  is foo(), 0, "get variable not yet declared using a sub (1)";
  is foo(), 1, "get variable not yet declared using a sub (2)";
  is foo(), 2, "get variable not yet declared using a sub (3)";

  my $a;
  sub foo { $a++ }
}

{
  is bar(), 0, "runtime part of my not yet executed (1)";
  is bar(), 1, "runtime part of my not yet executed (2)";
  is bar(), 2, "runtime part of my not yet executed (3)";

  my $a = 3;
  sub bar { $a++ }
}

{
  is baz(), 3, "runtime part of my not yet executed (1)";
  is baz(), 4, "runtime part of my not yet executed (2)";
  is baz(), 5, "runtime part of my not yet executed (3)";

  my $a; BEGIN { $a = 3 };
  sub baz { $a++ }
}

{
  {
    my $a = 3;
    sub grtz { $a++ }
  }

  is grtz(), 3, "get real hidden var using a sub (1)";
  is grtz(), 4, "get real hidden var using a sub (1)";
  is grtz(), 5, "get real hidden var using a sub (1)";
}

{
  my $a;
  sub rmbl { $a++ }

  is rmbl(), 0, "var captured by sub is the right var (1)";
  $a++;
  is rmbl(), 2, "var captured by sub is the right var (2)";
}
