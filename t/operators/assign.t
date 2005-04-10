#!/usr/bin/pugs

use v6;
require Test;

plan 51;

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
	my($s,@b) = 1 .. 3;

	is($a,1,"'$a' is '1'?: ($,$,$) = 1 .. 3");
	is($b,2,"'$b' is '2'?: ($,$,$) = 1 .. 3");
	is($c,3,"'$c' is '3'?: ($,$,$) = 1 .. 3"); 
	is(@a,'1 2 3',"'{@a}' is '1 2 3'?:       @a = 1 .. 3");
	is($s,'1',  "$s is '1'?:       my ($s,@a) = 1 .. 3");
	is(@b,'2 3',"'{@b}' is '2 3'?: my ($s,@a) = 1 .. 3"); 
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
    my $a;
    $a //= 3;
    is($a, 3, "//= operator");
    $a //= 10;
    is($a, 3, "... and second");
}

{
    my $a = 3;
    $a &&= 42;
    is($a, 42, "&&= operator");
    $a = 0;
    $a &&= 10;
    is($a, 0, "... and second");
}

{
  my $c; 

	eval '(($c = 3) = 4)'; 
	is($c,4, "((\$c = 3) = 4) return val should be good as an lval");
}

{
    my $x = 42;
    eval('$x += 6;');
    is($x, 48, '+= operator');
}

{
    my $x = 42;
    eval('$x -= 6;');
    is($x, 36, '-= operator');
}

{
    my $x = 4;
    eval('$x *= 3;');
    is($x, 12, '*= operator');
}

{
    my $x = 6;
    eval('$x /= 3;');
    is($x, 2, '/= operator');
}

{
    my $x = 2;
    eval('$x **= 3;');
    is($x, 8, '**= operator');
}

{
    my $x = "abc";
    eval('$x ~= "yz";');
    is($x, 'abcyz', '~= operator');
}

{
    my $x = "abc";
    eval('$x x= 3;');
    is($x, 'abcabcabc', 'x= operator');
}

{
    my @x = ( 'a', 'z' );
    eval('@x xx= 3;');
    is(+@x,   6,   'xx= operator elems');
    is(@x[0], 'a', 'xx= operator 0');
    is(@x[1], 'z', 'xx= operator 1');
    is(@x[2], 'a', 'xx= operator 2');
    is(@x[3], 'z', 'xx= operator 3');
    is(@x[4], 'a', 'xx= operator 4');
    is(@x[5], 'z', 'xx= operator 5');
}

{
    my $x = 1;
    eval('$x +&= 2;');
    is($x, 0, '+&= operator');
}

{
    my $x = 1;
    eval('$x +|= 2;');
    is($x, 3, '+|= operator');
}

{
    my $x = "z";
    eval('$x ~&= "I";');
    is($x, 'H', '~&= operator');
}

{
    my $x = "z";
    eval('$x ~|= "I";');
    is($x, '{', '~|= operator');
}

# XXX: nasty are the next four, causing whole program to terminate
# with 'cannot cast into a handle'--even though wrapped in eval.

{
	my $x = 4;
	# eval('$x %= 3;');
	# is($x, 1, '%= operator');
	todo_fail 'eval($x %= 3) cannot cast into a handle: VInt 3'
}

{
	my $x = 1;
	# eval('$x +^= 3;');
	# is($x, 2, '+^= operator');
	todo_fail q< eval('$x +^= 3;') cannot cast into a handle: VInt 3>;
}

{
	my $x = "z";
	# eval('$x ~^= "C";');
	# is($x, 9, '~^= operator');
	todo_fail q< eval('$x ~^= "C";') cannot cast into a handle: VStr "C"> ;
}

{
	my $x = 0;
	# eval('$x ^^= 42;');
	# is($x, 42, '^^= operator');
	todo_fail q<eval('$x ^^= 42;'); cannot cast into a handle: VInt 42>;
}

