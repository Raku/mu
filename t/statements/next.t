#!/usr/bin/pugs

use v6;
use Test;

=kwid

next
next if <condition>;
<condition> and next;
next <label>;
next in nested loops
next <label> in nested loops

=cut

plan 12;

# test for loops with next

{
    eval_is(
        'my $tracker=0;for (1..2) { next; $tracker++;} $tracker',
        0,
        "tracker is 0 because next before increment",
    );
}

{
    eval_is(
        'my $tracker = 0; for (1..5) { next if 2 < $_ < 4; $tracker = $_;} $tracker',
        3,
        "... nothing before or after 3 (next if <cond>)",
        :todo<bug>
    );
}

{
    eval_is(
        'my $tracker = 0; for (1..5) { $_ > 3 && next; $tracker = $_;} $tracker',
        3,
        "... nothing after 3 (<cond> && next)",
    );
}

{
    eval_is(
        'my $tracker = 0; for (1..5) { $_ > 3 and next; $tracker = $_;} $tracker',
        3,
        "... nothing after 3 (<cond> and next)",
    );
}

{
    eval_is(
        'my $tracker=0; DONE: for (1..2) { next DONE; $tracker++;} $tracker',
        0,
        "tracker is 0 because next before increment",
        :todo<bug>
    );
}

{
    eval_is('my $tracker=0;for (1..5)->$out {for (10..11)->$in {next if $out > 2;$tracker = $in + $out;}}$tracker;',
        13,
        'inner loop skips once inner is run twice (next inside nested loops)',
    );
}

{
    eval_is(
        'my $tracker=0; OUT: for (1..2) { IN: for (1..2) { next OUT; $tracker++; } } $tracker',
        0,
        "tracker is 0 because next before increment in nested loop",
        :todo<bug>
    );
}

=pod

Check that C<next> works on the correct loop/block

=cut

{
  my $foo;
  for 1..2 -> $a {
    $foo ~= "A";
    for 1..2 -> $b {
        $foo ~= "B";
        next;             # works on higher level loop, should work on inner
    }
  }
  is($foo, "ABBABB", "next works on inner loop of 2");
}

{
    my $bar;
    for 1..2 -> $a {
        $bar ~= "A";
        for 1..2 -> $b {
            $bar ~= "B";
            for 1..2 -> $c {
                $bar ~= "C";
                next;         # same thing
            }
        }
    }
	is($bar, "ABCCBCCABCCBCC", "next works on inner loop of 3");
}

{
	my @log;	
	my $i;
	while ++$i < 2 {
		push @log, "before";
		next;
		push @log, "after";
	}
	
	is(~@log, "before", "statements after next are not executed");
}

{
	my $i = 0;
	
	for (1, 1, 0, 1, 0, 1) -> $x {
		if ($x){ next }
		$i++;
	}
	
	is($i, 2, '$i++ executed only twice, because next ')
}

{
	my $i = 0;
	my $j;
	
	loop ($j = 0; $j < 6; $j++) {
		if ($j % 2 == 0){ next }
		$i++;
	}
	
	is($i, 3, '$i++ was not executed when next was called before it in loop {}');
}
