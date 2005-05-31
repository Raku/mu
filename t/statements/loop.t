#!/usr/bin/pugs

use v6;
use Test;

=kwid

loop statement tests

L<S04/"The general loop statement">

=cut

plan 13;

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
eval 'loop (my $j = 0; $j < 10; $j++) { $count++; }';
is($count, 10, 'verify our ending condition',:todo);

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
	my ($i,	$continued);
	loop ($i = 0;; $continued = 1)
	{
		last if $i;
		$i++;
		next;
	}
	ok($continued, "next performs a loop's continue expression");
}

=kwid

loop { } while tests... i.e. loops without the () bits

L<S04/"Loop statements">

=cut

{
  eval_is('my $x = 0; loop { $x++ } while $x < 10; $x', 10, 'loop {} while');
}

{
  eval_is('my $x = 1; loop { $x++ } while 0; $x', 2, 'ensure loop {} while runs at least once');
}

{
  eval_is('my $x = 0; loop { $x++; redo if $x < 10 } while 0; $x', '$x', 'redo works in loop', :todo<feature>);
}
