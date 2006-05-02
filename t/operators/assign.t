#!/usr/bin/pugs

use v6;
use Test;

plan 67;

# tests various assignment styles

{
    my ($foo, $bar) = ("FOO", "BAR");
    is($foo, "FOO", "assigned correct value to first of two scalars");
    is($bar, "BAR", "... and second");

    ($foo, $bar) = ($bar, $foo);
    is($foo, "BAR", "swap assignment works for the first value");
    is($bar, "FOO", "... and second");
};

{
    my $x = 1;
    &infix:<=>.($x, 0);
    is($x, 0, 'assignment operator called as function');
}

{
    # swap two elements in the same array 
    # (moved this from array.t)
    
    my @a = (1 .. 5);
    (@a[0,1]) = @a[1,0];
    is(@a[0], 2, "slice assignment swapping two element in the same array");
    is(@a[1], 1, "slice assignment swapping two element in the same array");
    eval '@a[0,1] = @a[1,0]';
    ok($!, "lvalue slice without parens fails");
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
    (@a[1, 2, 3]) = 100, 200, 300;
    is(@a[1], 100, "assigned correct value from list to sliced array");
    is(@a[2], 200, "... and second");
    is(@a[3], 300, "... and third");
    is(@a[0], undef, "won't modify unassigned one");

    my @b;
    (@b[2, 1, 0]) = 401, 201, 1;
    is(@b[0], 1, "assigned correct value from list to unsorted sliced array");
    is(@b[1], 201, "... and second");
    is(@b[2], 401, "... and third");
    
}

{
    # chained @array = %hash = list assignment 
    my (@a, @b, %h);
    @a = %h = (1,2);
    @b = %h;
    is(@a[0], @b[0], "chained @ = % = list assignment");
    is(@a[1], @b[1], "chained @ = % = list assignment");
}

{
    # chained $scalar = %hash = list assignment 
    my ($s, $t, %h);
    $s = %h = (1,2);
    $t = %h;
    is($s, $t, "chained $ = % = list assignment");
}

{
    # (@b, @a) = (@a, @b) assignment
    my (@a, @b);
    @a = (1);
    @b = (2);
    (@b, @a) = (@a, @b);
    is(@a[0], undef, "(@b, @a) = (@a, @b) assignment \@a[0] == undef");
    is(@b[0], 1,     "(@b, @a) = (@a, @b) assignment \@b[0]");
    is(@b[1], 2,     "(@b, @a) = (@a, @b) assignment \@b[1]");
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
    my %hash;
    %hash<foo> //= hash();
    is(ref %hash<foo>, 'Hash', "Verify //= autovivifies correctly");
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
    (($c = 3) = 4); 
    is($c, 4, '(($c = 3) = 4) return val should be good as an lval');
}

{
    my $x = 42;
    $x += 6;
    is($x, 48, '+= operator');
}

{
    my $x = 42;
    $x -= 6;
    is($x, 36, '-= operator');
}

{
    my $x = 4;
    $x *= 3;
    is($x, 12, '*= operator');
}

{
    my $x = 6;
    $x /= 3;
    is($x, 2, '/= operator');
}

{
    my $x = 2;
    $x **= 3;
    is($x, 8, '**= operator');
}

{
    my $x = "abc";
    $x ~= "yz";
    is($x, 'abcyz', '~= operator');
}

{
    my $x = "abc";
    $x x= 3;
    is($x, 'abcabcabc', 'x= operator');
}

{
    my @x = ( 'a', 'z' );
    @x xx= 3;
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
    $x +&= 2;
    is($x, 0, '+&= operator');
}

{
    my $x = 1;
    $x +|= 2;
    is($x, 3, '+|= operator');
}

{
    my $x = "z";
    $x ~&= "I";
    is($x, 'H', '~&= operator');
}

{
    my $x = "z";
    $x ~|= "I";
    is($x, '{', '~|= operator');
}

{
    my $x = 4;
    $x %= 3;
    is($x, 1, '%= operator');
}

{
    my $x = 1;
    $x +^= 3;
    is($x, 2, '+^= operator');
}

{
    my $x = "z";
    $x ~^= "C";
    is($x, 9, '~^= operator');
}

{
    my $x = 0;
    $x ^^= 42;
    is($x, 42, '^^= operator');
}

{
    my $x = 42;
    $x ?|= 24;
    is($x, 1, '?|= operator');
}

{
    my $x = 42;
    eval '$x ?&= 24';   # XXX: compiler blows up
    is($x, 1, '?&= operator', :todo);
}

{
    my $x = 0;
    eval '$x ?^= 42';   # XXX: compiler blows up
    is($x, 1, '?^= operator', :todo);
}

{
    my $x = 1;
    eval '$x +<= 8';   # XXX: compiler blows up
    is($x, 256, '+<= operator', :todo);
}

{
    my $x = 511;
    eval '$x +>= 8';   # XXX: compiler blows up
    is($x, 1, '+>= operator', :todo);
}

# XXX: The following tests assume autoconvertion between "a" and buf8 type
{
    my $x = "a";
    eval '$x ~<= 8';   # XXX: compiler blows up
    is($x, "a\0", '~<= operator', :todo);
}

{
    my $x = "aa";
    eval '$x ~>= 8';   # XXX: compiler blows up
    is($x, "a", '~>= operator', :todo);
}
