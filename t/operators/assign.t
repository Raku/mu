#!/usr/bin/pugs

use v6;
require Test;

plan 24;

# tests various assignment styles

{
	my ($foo, $bar) = ("FOO", "BAR");
	is($foo, "FOO", "assigned correct value to first of two scalars");
	is($bar, "BAR", "... and second");

    # swaping them this thows the error:
    #    Cannot modify constant item
    #    Syn "," [Var "$foo",Var "$bar"]

	eval '($foo, $bar) = ($bar, $foo)';
	is($foo, "BAR", "swap assignment works for the first value");
	is($bar, "FOO", "... and second");
};


{
    # swap two elements in the same array 
    # (moved this from array.t)
    
    my @a = (1 .. 5);
    @a[0,1] = @a[1,0];
    is(@a[0], 2, "slice assignment swapping two element in the same array");
    is(@a[1], 1, "slice assignment swapping two element in the same array");
};

{
    # slice assignments
    
    my @a = (1 .. 3);
    my ($one, $two, $three) = @a;
    is($one, 1, "slice assignment my ($, $, $) = @ works");
    is($two, 2, "slice assignment my ($, $, $) = @ works");
    is($three, 3, "slice assignment my ($, $, $) = @ works");    
};

{
   # testing list assignment syntax 

	my ($a,$b,$c,@a);
	($a,$b,$c) = 1 .. 3;
	@a = 1 .. 3;
	my(@b) = 1 .. 3;

	is($a,1,"'$a' is '1'?: ($,$,$) = 1 .. 3");
	is($b,2,"'$b' is '2'?: ($,$,$) = 1 .. 3");
	is($c,3,"'$c' is '3'?: ($,$,$) = 1 .. 3"); 
	is(@a,'1 2 3',"'{@a}' is '1 2 3'?:      @a = 1 .. 3");
	is(@b,'1 2 3',"'{@b}' is '1 2 3'?: my (@a) = 1 .. 3"); 
}

{
	my @a;
	@a[1, 2, 3] = (100, 200, 300);
	is(@a[1], 100, "assigned correct value from list to sliced list");
	is(@a[2], 200, "... and second");
	is(@a[3], 300, "... and third");
	is(@a[0], undef, "won't modify unassigned one");

	my @b;
	@b[2, 1, 0] = (401, 201, 1);
	is(@b[0], 1, "assigned correct value from list to unsorted sliced list");
	is(@b[1], 201, "... and second");
	is(@b[2], 401, "... and third");
    
}

{
        my $a;
        $a ||= 3;
        is($a,3, "||= operator");
        $a ||= 10;
        is($a,3, "... and second");
}

{
  my $c; 
	(($c = 3) = 4); 
	is($c,4, "((\$c = 3) = 4) return val should be good as an lval");
}
