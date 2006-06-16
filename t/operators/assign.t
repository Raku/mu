#!/usr/bin/pugs

use v6;
use Test;

plan 198;

# tests various assignment styles

{
    my ($foo, $bar) = ("FOO", "BAR");
    is($foo, "FOO", "assigned correct value to first of two scalars");
    is($bar, "BAR", "... and second");

    ($foo, $bar) = ($bar, $foo);
    is($foo, "BAR", "swap assignment works for the first value");
    is($bar, "FOO", "... and second");
}

{
    my $x = 1;
    &infix:<=>.($x, 0);
    is($x, 0, 'assignment operator called as function');
}

{
    # swap two elements in the same array 
    # (moved this from array.t)
    
    my @a = (1 .. 5);
    @a[0,1] = @a[1,0];
    is(@a[0], 2, "slice assignment swapping two element in the same array");
    is(@a[1], 1, "slice assignment swapping two element in the same array");
}

{
    # swap two elements as slice, dwim slice from @b subscript
    
    my @a = (1 .. 2);
    my @b = (0 .. 2);
    @a[@b] = @a[1, 0], 3;
    is(@a[0], 2, "slice assignment swapping with array dwim");
    is(@a[1], 1, "slice assignment swapping with array dwim");
    is(@a[2], 3, "slice assignment swapping with array dwim makes listop");
}

{
    # list assignments
    
    my @a = (1 .. 3);
    my ($one, $two, $three) = @a;
    is($one, 1, "list assignment my ($, $, $) = @ works");
    is($two, 2, "list assignment my ($, $, $) = @ works");
    is($three, 3, "list assignment my ($, $, $) = @ works");    
}

{
   # testing list assignment syntax 

    my ($a,$b,$c,@a);
    ($a,$b,$c) = 1 .. 3;
    @a = 1 .. 3;
    my ($s,@b) = 1 .. 3;

    is($a,1,"'$a' is '1'?: ($,$,$) = 1 .. 3");
    is($b,2,"'$b' is '2'?: ($,$,$) = 1 .. 3");
    is($c,3,"'$c' is '3'?: ($,$,$) = 1 .. 3"); 
    is(@a,'1 2 3',"'{@a}' is '1 2 3'?:       @a = 1 .. 3");
    is($s,'1',  "$s is '1'?:       my ($s,@a) = 1 .. 3");
    is(@b,'2 3',"'{@b}' is '2 3'?: my ($s,@a) = 1 .. 3"); 
}

{
    my @a;
    @a[1, 2, 3] = 100, 200, 300;
    is(@a[1], 100, "assigned correct value from list to sliced array");
    is(@a[2], 200, "... and second");
    is(@a[3], 300, "... and third");
    is(@a[0], undef, "won't modify unassigned one");

    my @b;
    (@b[2, 1, 0]) = 401, 201, 1;
    is(@b[0], 1, "assigned correct value from list to unsorted sliced array");
    is(@b[1], 201, "... and second");
    is(@b[2], 401, "... and third");
    
    my @c;
    my @d;
    (@c[1, 2], @c[3], @d) = 100, 200, 300, 400, 500;
    is(@c[1], 100, "assigned correct value from list to slice-in-list");
    is(@c[2], 200, "... and second");
    is(@c[3], 300, "... and third", :todo<feature>);
    is(@d[0], 400, "... and fourth", :todo<feature>);
    is(@d[1], 500, "... and fifth", :todo<feature>);
    is(@c[0], undef, "won't modify unassigned one");

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
    # (@b, @a) = @a, @b assignment
    my (@a, @b);
    @a = (1);
    @b = (2);
    (@b, @a) = @a, @b;
    is(@a[0], undef, "(@b, @a) = @a, @b assignment \@a[0] == undef");
    is(@b[0], 1,     "(@b, @a) = @a, @b assignment \@b[0]");
    is(@b[1], 2,     "(@b, @a) = @a, @b assignment \@b[1]");
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

# Tests of dwimming scalar/listiness of lhs

my sub W () { substr(want, 0, 1) }

{
    my $a;
    my @z = ($a = W, W);
    is($a, 'S',    'lhs dwims $a as scalar');
    is(@z[0], 'S', 'lhs dwims $a as scalar');
    is(@z[1], 'L', 'lhs dwims $a as scalar');
}

{
    my $a;
    my @z = (($a) = W, W, W);
    is($a, 'L', 'lhs dwims ($a) as list');
    is(@z, "L", 'lhs dwims ($a) as list');
}

{
    my $a;
    my $b;
    my @z = (($a,$b) = W, W, W);
    is($a, 'L',   'lhs dwims ($a,$b) as list');
    is($b, 'L',   'lhs dwims ($a,$b) as list');
    is(@z, "L L", 'lhs dwims ($a,$b) as list');
}

{
    my @a;
    my @z = (@a[0] = W, W);
    is(@a, 'S',    'lhs dwims @a[0] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[0] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[0] as scalar');
}

{
    my @a;
    my @z = (@a[0,] = W, W);
    is(@a, 'L',      'lhs dwims @a[0,] as list');
    is(@z[0], 'L',   'lhs dwims @a[0,] as list');
    is(@z[1], undef, 'lhs dwims @a[0,] as list');
}

{
    my %a;
    my @z = (%a<x> = W, W);
    is(%a{"x"}, 'S', 'lhs dwims %a<x> as scalar');
    is(@z[0], 'S',   'lhs dwims %a<x> as scalar');
    is(@z[1], 'L',   'lhs dwims %a<x> as scalar');
}

{
    my %a;
    my @z = (%a<x y z> = W, W, W);
    is(%a<x>, 'L',    'lhs dwims %a<x y z> as list');
    is(%a<y>, 'L',    'lhs dwims %a<x y z> as list');
    is(%a<z>, 'L',    'lhs dwims %a<x y z> as list');
}

{
    my %a;
    my @z = (%a{'x'} = W, W);
    is(%a{"x"}, 'S', q/lhs dwims %a{'x'} as scalar/);
    is(@z[0], 'S',   q/lhs dwims %a{'x'} as scalar/);
    is(@z[1], 'L',   q/lhs dwims %a{'x'} as scalar/);
}

{
    my %a;
    my @z = (%a{'x','y','z'} = W, W, W);
    is(%a<x>, 'L',    q/lhs dwims %a{'x','y','z'} as list/);
    is(%a<y>, 'L',    q/lhs dwims %a{'x','y','z'} as list/);
    is(%a<z>, 'L',    q/lhs dwims %a{'x','y','z'} as list/);
}

{
    my %a;
    my @z = (%a{'x'..'z'} = W, W, W);
    is(%a<x>, 'L',    q/lhs dwims %a{'x'..'z'} as list/);
    is(%a<y>, 'L',    q/lhs dwims %a{'x'..'z'} as list/);
    is(%a<z>, 'L',    q/lhs dwims %a{'x'..'z'} as list/);
}

{
    my %a;
    my @z = (%a{'x' x 1} = W, W);
    is(%a{"x"}, 'S', q/lhs dwims %a{'x' x 1} as scalar/);
    is(@z[0], 'S',   q/lhs dwims %a{'x' x 1} as scalar/);
    is(@z[1], 'L',   q/lhs dwims %a{'x' x 1} as scalar/);
}

{
    my %a;
    my @z = (%a{'x' xx 1} = W, W, W);
    is(%a<x>, 'L',    q/lhs dwims %a{'x' xx 1} as list/);
    is(@z[0], 'L',    q/lhs dwims %a{'x' xx 1} as list/);
    is(@z[1], undef,  q/lhs dwims %a{'x' xx 1} as list/);
}

{
    my @a;
    my $b = 0;
    my @z = (@a[$b] = W, W);
    is(@a, 'S',    'lhs dwims @a[$b] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[$b] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[$b] as scalar');
}

{
    my @a;
    my $b = 0;
    my @z = (@a[$b,] = W, W);
    is(@a, 'L',      'lhs dwims @a[$b,] as list');
    is(@z[0], 'L',   'lhs dwims @a[$b,] as list');
    is(@z[1], undef, 'lhs dwims @a[$b,] as list');
}

{
    my @a;
    my @b = (0,1);
    my @z = (@a[@b] = W, W, W);
    is(@a, 'L L',  'lhs dwims @a[@b] as list');
    is(@z[0], 'L', 'lhs dwims @a[@b] as list');
    is(@z[1], 'L', 'lhs dwims @a[@b] as list');
    is(@z[2], undef, 'lhs dwims @a[@b] as list');
}

{
    my @a;
    my @b = (0,0);
    my $c = 1;
    my @z = (@a[@b[$c]] = W, W);
    is(@a, 'S',    'lhs dwims @a[@b[$c]] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[@b[$c]] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[@b[$c]] as scalar');
}

{
    my @a;
    my @b = (0,0);
    my $c = 1;
    my @z = (@a[@b[$c,]] = W, W);
    is(@a, 'L',      'lhs dwims @a[@b[$c,]] as list');
    is(@z[0], 'L',   'lhs dwims @a[@b[$c,]] as list');
    is(@z[1], undef, 'lhs dwims @a[@b[$c,]] as list');
}

{
    my @a;
    my $b = 0;
    sub foo { \@a }
    my @z = eval '(foo()[$b] = W, W)';
    is(@a, 'S',    'lhs dwims foo()[$b] as scalar', :todo<bug>);
    is(@z[0], 'S', 'lhs dwims foo()[$b] as scalar', :todo<bug>);
    is(@z[1], 'L', 'lhs dwims foo()[$b] as scalar', :todo<bug>);
}

{
    my @a;
    my $b = 0;
    sub foo { \@a }
    my @z = eval '(foo()[$b,] = W, W)';
    is(@a, 'L',      'lhs dwims foo()[$b,] as list', :todo<bug>);
    is(@z[0], 'L',   'lhs dwims foo()[$b,] as list', :todo<bug>);
    is(@z[1], undef, 'lhs dwims foo()[$b,] as list');
}

{
    my @a;
    my $b = 0;
    sub foo { \@a }
    my @z = (@a[foo()[$b]] = W, W);
    is(@a, 'S',    'lhs dwims @a[foo()[$b]] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[foo()[$b]] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[foo()[$b]] as scalar');
}

{
    my @a;
    my $b = 0;
    sub foo { \@a }
    my @z = (@a[foo()[$b,]] = W, W);
    is(@a, 'L',      'lhs dwims @a[foo()[$b,]] as list');
    is(@z[0], 'L',   'lhs dwims @a[foo()[$b,]] as list');
    is(@z[1], undef, 'lhs dwims @a[foo()[$b,]] as list');
}

{
    my @a;
    sub foo { 0 }
    my @z = (@a[+foo()] = W, W);
    is(@a, 'S',    'lhs dwims @a[+foo()] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[+foo()] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[+foo()] as scalar');
}

{
    my @a;
    sub foo { '0' }
    my @z = (@a[~foo()] = W, W);
    is(@a, 'S',    'lhs dwims @a[~foo()] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[~foo()] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[~foo()] as scalar');
}

{
    my @a;
    sub foo { 0 }
    my @z = (@a[?foo()] = W, W);
    is(@a, 'S',    'lhs dwims @a[?foo()] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[?foo()] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[?foo()] as scalar');
}

{
    my @a;
    sub foo { 1 }
    my @z = (@a[!foo()] = W, W);
    is(@a, 'S',    'lhs dwims @a[!foo()] as scalar', :todo<bug>);
    is(@z[0], 'S', 'lhs dwims @a[!foo()] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[!foo()] as scalar');
}

{
    my @a;
    my $b = 0;
    sub foo { 0 }
    my @z = (@a[foo()] = W, W);
    is(@a, 'L',      'lhs dwims @a[foo()] as list');
    is(@z[0], 'L',   'lhs dwims @a[foo()] as list');
    is(@z[1], undef, 'lhs dwims @a[foo()] as list');
}

{
    my %a;
    sub foo { 0 }
    my @z = (%a{+foo()} = W, W);
    is(%a, 'S',    'lhs dwims %a{+foo()} as scalar', :todo<bug>);
    is(@z[0], 'S', 'lhs dwims %a{+foo()} as scalar');
    is(@z[1], 'L', 'lhs dwims %a{+foo()} as scalar');
}

{
    my %a;
    sub foo { '0' }
    my @z = (%a{~foo()} = W, W);
    is(%a, 'S',    'lhs dwims %a{~foo()} as scalar', :todo<bug>);
    is(@z[0], 'S', 'lhs dwims %a{~foo()} as scalar');
    is(@z[1], 'L', 'lhs dwims %a{~foo()} as scalar');
}

{
    my %a;
    sub foo { 0 }
    my @z = (%a{?foo()} = W, W);
    is(%a, 'S',    'lhs dwims %a{?foo()} as scalar', :todo<bug>);
    is(@z[0], 'S', 'lhs dwims %a{?foo()} as scalar');
    is(@z[1], 'L', 'lhs dwims %a{?foo()} as scalar');
}

{
    my %a;
    sub foo { 1 }
    my @z = (%a{!foo()} = W, W);
    is(%a, 'S',    'lhs dwims %a{!foo()} as scalar', :todo<bug>);
    is(@z[0], 'S', 'lhs dwims %a{!foo()} as scalar');
    is(@z[1], 'L', 'lhs dwims %a{!foo()} as scalar');
}

{
    my %a;
    my $b = 0;
    sub foo { 0 }
    my @z = (%a{foo()} = W, W);
    is(%a{0}, 'L',      'lhs dwims %a{foo()} as list');
    is(@z[0], 'L',   'lhs dwims %a{foo()} as list');
    is(@z[1], undef, 'lhs dwims %a{foo()} as list');
}

{
    my @a;
    my @z = (@a[0+0] = W, W);
    is(@a, 'S',    'lhs dwims @a[0+0] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[0+0] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[0+0] as scalar');
}

{
    my @a;
    my @z = (@a[0*0] = W, W);
    is(@a, 'S',    'lhs dwims @a[0*0] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[0*0] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[0*0] as scalar');
}

{
    my @a;
    my @z = (@a[0/1] = W, W);
    is(@a, 'S',    'lhs dwims @a[0/1] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[0/1] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[0/1] as scalar');
}

{
    my @a;
    my @z = (@a[0*1**1] = W, W);
    is(@a, 'S',    'lhs dwims @a[0*1**1] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[0*1**1] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[0*1**1] as scalar');
}

{
    my @a;
    my $b = 0;
    my @z = (@a[$b++] = W, W);
    is(@a, 'S',    'lhs dwims @a[$b++] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[$b++] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[$b++] as scalar');
}

{
    my @a;
    my $b = 1;
    my @z = (@a[--$b] = W, W);
    is(@a, 'S',    'lhs dwims @a[--$b] as scalar');
    is(@z[0], 'S', 'lhs dwims @a[--$b] as scalar');
    is(@z[1], 'L', 'lhs dwims @a[--$b] as scalar');
}

{
    my @a;
    my @z = (@a[0==1] = W, W);
    is(@a, 'L',      'lhs dwims @a[0==1] as list');
    is(@z[0], 'L',   'lhs dwims @a[0==1] as list');
    is(@z[1], undef, 'lhs dwims @a[0==1] as list');
}

{
    my @a;
    my @z = (@a[rand 1] = W, W);
    is(@a, 'L',      'lhs dwims @a[rand 1] as list');
    is(@z[0], 'L',   'lhs dwims @a[rand 1] as list');
    is(@z[1], undef, 'lhs dwims @a[rand 1] as list');
}

{
    my @a;
    my @z = (@a[(0|0).pick] = W, W);
    is(@a, 'L',      'lhs dwims @a[(0|0).pick] as list');
    is(@z[0], 'L',   'lhs dwims @a[(0|0).pick] as list');
    is(@z[1], undef, 'lhs dwims @a[(0|0).pick] as list');
}

