#!/usr/bin/pugs

use v6;
use Test;

plan 10;

=pod

Test pointy sub behaviour described in S06

L<S06/"Pointy subs">

=cut

# L<S06/"Pointy subs"/"the parameter list of a pointy sub does not require parentheses">
my ($sub, $got);

$got = '';
$sub = -> $x { $got = "x $x" };
$sub.(123);
is $got, 'x 123', 'pointy sub without param parens';

$got = '';
$sub = -> ($x) { $got = "x $x" }.(123);
is $got, 'x 123', 'pointy sub with param parens dot called';

$got = '';
$sub = -> ($x) { $got = "x $x" }(123);
is $got, 'x 123', 'pointy sub with param parens called';

$got = '';
-> $x { $got = "x $x" }.(123);
is $got, 'x 123', 'called pointy immediately: -> $x { ... }.(...)';

$got = '';
-> $x { $got = "x $x" }(123); 
is $got, 'x 123', 'called pointy immediately: -> $x { ... }(...)';


# L<S06/"Pointy subs"/"not require a preceding comma when included in a list">
# Is this what is really intended?
# my @a;
# eval_ok '@a = ("one" -> $x { $x**2 }, "three")', 
#         'pointy sub without preceding comma';
# is @a[0], 'one', 'pointy sub in list';
# isa_ok @a[1], 'Code', 'pointy sub in list';
# is @a[2], 'three', 'pointy sub in list';


# L<S06/"Pointy subs"/"behaves like a block with respect to control exceptions">
my $n = 1;
my $s = -> { 
    last if $n == 10;
    $n++;
    redo if $n < 10;
};
try { $s.() };
is($!, undef, 'pointy with block control exceptions', :todo<feature>);
is $n, 10, "pointy control exceptions ran", :todo<feature>;

# L<S06/"Pointy subs"/"will return from the innermost enclosing sub or method">
my $str = '';

sub outer {  
    my $s = -> $x { 
        is($?SUBNAME, '&main::outer', 'pointy still sees outer\'s $?SUBNAME'); 

        $str ~= 'inner'; 
        return 'inner ret'; 
    };
    $s.(); 
    $str ~= 'outer';
    return 'outer ret';
}

is outer(), 'inner ret', 'return in pointy returns from enclosing sub';
is $str, 'inner', 'return in pointy returns from enclosing sub';

# What about nested pointies -> { ... -> {} }?


# L<S06/"Pointy subs"/"It is referenced by &?BLOCK, not &?SUB">
# Coming soon...
