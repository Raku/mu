#!/usr/bin/pugs

# plays with some fun operator overloading.

use v6;

multi sub postfix:<!> ($x) { [*] 1..$x };
multi sub postfix:<!> (@x) { [*] @x };

multi sub infix:<z> (@x,@y) { zip @x,@y };
multi sub infix:<z> ($x,$y) { $x ~ $y };

my @x = (1..5);
my @y = (6..10);

(@x z @y).perl.say;
my $test = "hello" z "goodbye";
$test.perl.say;

$test = 10!; $test.perl.say;

my @test = (1..5);
$test = @test!;
$test.perl.say;

multi sub postfix:<<%%>> { $_ / 100 }; #since overloading % breaks it in infix
multi sub infix:<<of>> ($x,$y) {$x * $y};
say 50%% of 100;

sub base (Int $M, Int $N) {
	return $M if ($M < $N);
   my $t = $M % $N;
   return base(int($M/$N),$N) ~ $t;
}

multi sub infix:<<base>> ($x,$y) {base($x,$y)};
say $_ base 2 for (1..5);	
