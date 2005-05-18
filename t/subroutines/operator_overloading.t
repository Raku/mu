#!/usr/bin/pugs

use v6;
use Test;

plan 11;

=pod

Testing operator overloading subroutines

L<S06/"Operator overloading">

=cut

# This set of tests is very basic for now.

sub prefix:<X> ($thing) { return "ROUGHLY$thing"; };

is(X "fish", "ROUGHLYfish",
   'prefix operator overloading for new operator');

sub prefix:<±> ($thing) { return "AROUND$thing"; };
eval_ok('(± "fish") eq "AROUNDfish',
	'prefix operator overloading for new operator (unicode)',
	:todo<bug>
       );
sub prefix:<(+-)> ($thing) { return "ABOUT$thing"; };
eval_ok('((+-) "fish") eq "ABOUTfish',
	'prefix operator overloading for new operator (nasty)',
	:todo<feature>
       );

sub prefix:<->($thing) { return "CROSS$thing"; };
is(-"fish", "CROSSfish",
   'prefix operator overloading for existing operator');

sub infix:<C> ($text, $owner) { return "$text copyright $owner"; };
eval_ok( ' ("romeo & juliet" C "Shakespeare") eq
            "romeo & juliet copyright Shakespeare" ',
	 'infix operator overloading for new operator', :todo<feature>);

sub infix:<©> ($text, $owner) { return "$text Copyright $owner"; };
eval_ok( ' ("romeo & juliet" © "Shakespeare") eq
            "romeo & juliet copyright Shakespeare" ',
	 'infix operator overloading for new operator (unicode)',
	 :todo<feature>);

sub infix:<(C)> ($text, $owner) { return "$text CopyRight $owner"; };
eval_ok( ' ("romeo & juliet" (C) "Shakespeare") eq
            "romeo & juliet copyright Shakespeare" ',
	 'infix operator overloading for new operator (nasty)',
	 :todo<feature>);

eval_ok('sub infix:"<"($one, $two) { return (rand(1) <=> 0.5) }',
	"quoted infix subs", :todo<bug>);

sub postfix:<W> ($wobble) { return "ANDANDAND$wobble"; };

is("boop" W, "ANDANDANDboop", 
   'postfix operator overloading for new operator');

my $var = 0;
eval_ok('macro circumfix:<!--...-->   ($text) { "" }; <!-- $var = 1; -->; $var == 0;', 'circumfix macro', :todo);

# demonstrate sum prefix

sub prefix:<Σ> (@x) { [+] *@x }
is(Σ [1..10], 55, "sum prefix operator");

sub postfix:<!> ($x) { [*] 1..$x }
is(10!, 3628800, "factorial postfix operator");


