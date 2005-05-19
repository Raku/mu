#!/usr/bin/pugs

# plays with some fun operator overloading.

use v6;

multi sub postfix:<!> ($x) { [*] 1..$x }
multi sub postfix:<!> (@x) { [*] @x }

multi sub infix:<z> (@x,@y) { zip @x,@y }
multi sub infix:<z> ($x,$y) { $x ~ $y }

my @x = (1..5);
my @y = (6..10);

(@x z @y).perl.say;
my $test = "hello" z "goodbye";
$test.perl.say;

$test = 10!; $test.perl.say;
my @test = (1..5);
$test = @test!;
$test.perl.say;