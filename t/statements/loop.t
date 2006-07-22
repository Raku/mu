use v6-alpha;

use Test;

=kwid

loop statement tests

L<S04/"The general loop statement">

=cut

plan 21;

# basic loop

my $i = 0;
is($i, 0, 'verify our starting condition');
loop ($i = 0; $i < 10; $i++) {}
is($i, 10, 'verify our ending condition');

# loop with last()

my $i = 0;
is($i, 0, 'verify our starting condition');
loop ($i = 0; $i < 10; $i++) {
    if ($i == 5) { 
        last(); # should this really need the ()
    }
}
is($i, 5, 'verify our ending condition');

# infinite loop

my $i = 0;
is($i, 0, 'verify our starting condition');
loop (;;) { $i++; last(); }
is($i, 1, 'verify our ending condition');

# declare variable $j inside loop
my $count  = 0;
is($count, 0, 'verify our starting condition');
my $j; loop ($j = 0; $j < 10; $j++) { $count++; };
is($count, 10, 'verify our ending condition');

# Ensure condition is tested on the first iteration
{
    my $never_did_body = 1;
    loop (;0;)
    {
        $never_did_body = 0;
    }
    ok($never_did_body, "loop with an initially-false condition executes 0 times");
}

# Loop with next should still execute the continue expression
{
    my ($i,    $continued);
    loop ($i = 0;; $continued = 1)
    {
        last if $i;
        $i++;
        next;
    }
    ok($continued, "next performs a loop's continue expression");
}

=kwid

repeat { } while tests... i.e. loops without the () bits

L<S04/"Loop statements">

=cut

{
  my $x = 0; repeat { $x++ } while $x < 10;
  is($x, 10, 'repeat {} while');
}

{
  my $x = 1; repeat { $x++ } while 0;
  is($x, 2, 'ensure repeat {} while runs at least once');
}

{
  my $x = 0; try { repeat { $x++; redo if $x < 10 } while 0 };
  is($x, 10, 'redo works in repeat', :todo<feature>);
}

{
  my $x = 0; repeat while $x < 10 { $x++ }
  is($x, 10, 'repeat {} while');
}

{
  my $x = 1; repeat while 0 { $x++ }
  is($x, 2, 'ensure repeat {} while runs at least once');
}

{
  my $x = 0; try { repeat while 0 { $x++; redo if $x < 10 } };
  is($x, 10, 'redo works in repeat', :todo<feature>);
}

{
  my $x = 0; repeat { $x++ } until $x >= 10;
  is($x, 10, 'repeat {} until');
}

{
  my $x = 1; repeat { $x++ } until 1;
  is($x, 2, 'ensure repeat {} until runs at least once');
}

{
  my $x = 0; try { repeat { $x++; redo if $x < 10 } until 1 };
  is($x, 10, 'redo works in repeat {} until', :todo<feature>);
}

{
  my $x = 0; repeat until $x >= 10 { $x++ }
  is($x, 10, 'repeat until {}');
}

{
  my $x = 1; repeat until 1 { $x++ }
  is($x, 2, 'ensure repeat until {} runs at least once');
}

{
  my $x = 0; try { repeat until 1 { $x++; redo if $x < 10 } };
  is($x, 10, 'redo works in repeat until {}', :todo<feature>);
}
