#!/usr/bin/pugs

use v6;
use Test;

plan 56;

=pod

Misc. Junction tests 

L<S09/"Junctions">

L<S03/"Junctive operators">

=cut

{ # L<S09/"Junctions">

    # initalize them all to empty strings
    my $a = '';
    my $b = '';
    my $c = '';
    
    # make sure they all match to an empty string
    ok('' eq ($a & $b & $c), 'junction of ($a & $b & $c) matches and empty string');
    ok('' eq all($a, $b, $c), 'junction of all($a, $b, $c) matches and empty string');   
    
    # give $a a value
    $a = 'a';  
    
    # make sure that at least one of them matches 'a' 
    ok('a' eq ($b | $c | $a), 'junction of ($b | $c | $a) matches at least one "a"');
    ok('a' eq any($b, $c, $a), 'junction of any($b, $c, $a) matches at least one "a"');   

    ok('' eq ($b | $c | $a), 'junction of ($b | $c | $a) matches at least one empty string');
    ok('' eq any($b, $c, $a), 'junction of any($b, $c, $a) matches at least one empty string');
    
    # make sure that ~only~ one of them matches 'a'
    ok('a' eq ($b ^ $c ^ $a), 'junction of ($b ^ $c ^ $a) matches at ~only~ one "a"');
    ok('a' eq one($b, $c, $a), 'junction of one($b, $c, $a) matches at ~only~ one "a"');
    
    # give $b a value
    $b = 'a';
    
    # now this will fail
    ok('a' ne ($b ^ $c ^ $a), 'junction of ($b ^ $c ^ $a) matches at more than one "a"');              

    # change $b and give $c a value
    $b = 'b';
    $c = 'c';
    
    ok('a' eq ($b ^ $c ^ $a), 'junction of ($b ^ $c ^ $a) matches at ~only~ one "a"');
    ok('b' eq ($a ^ $b ^ $c), 'junction of ($a ^ $b ^ $c) matches at ~only~ one "b"');
    ok('c' eq ($c ^ $a ^ $b), 'junction of ($c ^ $a ^ $b) matches at ~only~ one "c"');  

    ok('a' eq ($b | $c | $a), 'junction of ($b | $c | $a) matches at least one "a"');
    ok('b' eq ($a | $b | $c), 'junction of ($a | $b | $c) matches at least one "b"');
    ok('c' eq ($c | $a | $b), 'junction of ($c | $a | $b) matches at least one "c"'); 
    
    # test junction to junction
    
    ok(('a' | 'b' | 'c') eq ($a & $b & $c), 'junction ("a" | "b" | "c") matches junction ($a & $b & $c)');    
    ok(('a' & 'b' & 'c') eq ($a | $b | $c), 'junction ("a" & "b" & "c") matches junction ($a | $b | $c)'); 
    
    # mix around variables and literals
    
    ok(($a & 'b' & 'c') eq ('a' | $b | $c), 'junction ($a & "b" & "c") matches junction ("a" | $b | $c)');              
    ok(($a & 'b' & $c) eq ('a' | $b | 'c'), 'junction ($a & "b" & $c) matches junction ("a" | $b | "c")');              
    
}

# same tests, but with junctions as variables
{
        # initalize them all to empty strings
    my $a = '';
    my $b = '';
    my $c = '';
    
    my $all_of_them = $a & $b & $c;
    ok('' eq $all_of_them, 'junction variable of ($a & $b & $c) matches and empty string');
    
    $a = 'a';  
    
    my $any_of_them = $b | $c | $a;
    ok('a' eq $any_of_them, 'junction variable of ($b | $c | $a) matches at least one "a"');  
    ok('' eq $any_of_them, 'junction variable of ($b | $c | $a) matches at least one empty string');
    
    my $one_of_them = $b ^ $c ^ $a;
    ok('a' eq $one_of_them, 'junction variable of ($b ^ $c ^ $a) matches at ~only~ one "a"');
    
    $b = 'a';
    
    {
        my $one_of_them = $b ^ $c ^ $a;
        ok('a' ne $one_of_them, 'junction variable of ($b ^ $c ^ $a) matches at more than one "a"');              
    }
    
    $b = 'b';
    $c = 'c';
    
    {
        my $one_of_them = $b ^ $c ^ $a;    
        ok('a' eq $one_of_them, 'junction of ($b ^ $c ^ $a) matches at ~only~ one "a"');
        ok('b' eq $one_of_them, 'junction of ($a ^ $b ^ $c) matches at ~only~ one "b"');
        ok('c' eq $one_of_them, 'junction of ($c ^ $a ^ $b) matches at ~only~ one "c"');  
    }

    {
        my $any_of_them = $b | $c | $a;
        ok('a' eq $any_of_them, 'junction of ($b | $c | $a) matches at least one "a"');
        ok('b' eq $any_of_them, 'junction of ($a | $b | $c) matches at least one "b"');
        ok('c' eq $any_of_them, 'junction of ($c | $a | $b) matches at least one "c"'); 
    }

}

{
	my $j = 1 | 2;
	eval '$j = 5';
	is($j, 5, 'reassignment of junction variable');
}

{
	my ($j,$k,$l);

	$j = 1|2;
	is(ref($j),'Junction', 'basic junction type reference test');

	$k=$j;
	is(ref($k),'Junction', 'assignment preserves reference');

	# XXX does this next one make any sense?
	$l=\$j;
	is(ref($l),'Junction', 'hard reference to junction');
}


=pod

Tests junction examples from Synopsis 03 

Tests use .perl representation for checking until a better way presents itself

L<S03/"Junctive operators">

=cut

# Canonical stringification of a junction
sub j (*@j is Junction) { return map { $_.perl } @j; }

{
    # L<S03/"Junctive operators"/"They thread through operations">
    my ($got, $want);
    $got = ((1|2|3)+4);
    $want = (5|6|7);
    is( j($got), j($want), 'thread + returning junctive result');

    $got = ((1|2) + (3&4));
    $want = ((4|5) & (5|6));
    is( j($got), j($want), 'thread + returning junctive combination of results');

    # L<S03/"Junctive operators"/"This opens doors for constructions like">
    # unless $roll == any(1..6) { print "Invalid roll" }
    my ($roll, $note);
    $roll = 3; $note = '';
    unless $roll == any(1..6) { $note = "Invalid roll"; };
    is($note, "", 'any() junction threading ==');

    $roll = 7; $note = '';
    unless $roll == any(1..6) { $note = "Invalid roll"; };
    is($note, "Invalid roll", 'any() junction threading ==');

    # if $roll == 1|2|3 { print "Low roll" }
    $roll = 4; $note = '';
    if $roll == 1|2|3 { $note = "Low roll" }
    is($note, "", '| junction threading ==');

    $roll = 2; $note = '';
    if $roll == 1|2|3 { $note = "Low roll" }
    is($note, "Low roll", '| junction threading ==');


    # L<S03/"Junctive operators"/"Junctions work through subscripting">
    my ($got, @foo);
    $got = ''; @foo = ();
    $got ~= 'y' if @foo[any(1,2,3)]
    is($got, '', "junctions work through subscripting, 0 matches");

    $got = ''; @foo = (0,1);
    $got ~= 'y' if @foo[any(1,2,3)]
    is($got, '', "junctions work through subscripting, 1 match");

    $got = ''; @foo = (1,1,1);
    $got ~= 'y' if @foo[any(1,2,3)]
    is($got, '', "junctions work through subscripting, 3 matches");


    # L<S03/"Junctive operators"/"Junctions are specifically unordered">
    # Compiler *can* reorder and parallelize but *may not* so don't test
    # for all(@foo) {...};  

    # Not sure what is expected
    #my %got = ('1' => 1); # Hashes are unordered too
    #@foo = (2,3,4);
    #for all(@foo) { %got{$_} = 1; };
    #is( %got.keys.sort.join(','), '1,2,3,4',
    #    'for all(...) { ...} as parallelizable');
}

=pod

These are implemented but still awaiting clarification on p6l.

L<S03/"Junctive operators"/"They thread through operations">

 On Fri, 2005-02-11 at 10:46 +1100, Damian Conway wrote:
 > Subject: Re: Fwd: Junctive puzzles.
 >
 > Junctions have an associated boolean predicate that's preserved across 
 > operations on the junction. Junctions also implicitly distribute across 
 > operations, and rejunctify the results.

=cut

{
    my @subs = (sub {3}, sub {2});

    my ($got, $want);

    is(j(any(@subs)()), j(3|2), '.() on any() junction of subs');

    $want = (3&2);
    $got = all(@subs)();
    is(j($got), j($want), '.() on all() junction of subs');

    $want = (3^2);
    $got = one(@subs)();
    is(j($got), j($want), '.() on one() junction of subs');

    $want = none(3,2);
    $got = none(@subs)();
    is(j($got), j($want), '.() on none() junction of subs');

    $want = one( any(3,2), all(3,2) );
    $got = one( any(@subs), all(@subs) )();
    is(j($got), j($want), '.() on complex junction of subs');

    # Avoid future constant folding
    #my $rand = rand;
    #my $zero = int($rand-$rand);
    #my @subs = (sub {3+$zero}, sub {2+$zero});
}

# Check functional and operator versions produce the same structure
{
    my ($got, $want);
    $got = ((1|2)^(3&4)).perl;
    $want = one(any(1,2),all(3,4)).perl;
    is($got, $want, '((1|2)^(3&4)) equiv to one(any(1,2),all(3,4))');

    $got = ((1|2)!(3&4)).perl;
    $want = none(any(1,2),all(3,4)).perl;
    is($got, $want, '((1|2)!(3&4)) equiv to none(any(1,2),all(3,4))');

    $got = ((1|2)&(3&4)).perl;
    $want = all(any(1,2),all(3,4)).perl;
    is($got, $want, '((1|2)!(3&4)) equiv to all(any(1,2),all(3,4))');

    $got = ((1|2)|(3&4)).perl;
    $want = any(any(1,2),all(3,4)).perl;
    is($got, $want, '((1|2)|(3&4)) equiv to any(any(1,2),all(3,4))');

}

is(none(1).pick, undef, 'none(1).pick should be undef');
is(none(1,1).pick, undef, 'none(1,1).pick should be undef');

is(one(1).pick, 1, 'one(1).pick should be 1');
is(one(1,1).pick, undef, 'one(1,1).pick should be undef');

is(all(1).pick, 1, 'all(1).pick should be 1');
is(all(1,1).pick, 1, 'all(1,1).pick should be 1');
is(all(1,2).pick, undef, 'all(1,2).pick should be undef');


