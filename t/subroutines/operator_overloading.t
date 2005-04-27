#!/usr/bin/pugs

use v6;
use Test;

plan 5;

=pod

Testing operator overloading subroutines

L<S06/"Operator overloading">

=cut

# This set of tests is not supposed to be comprehensive...

eval_ok('sub prefix:± ($thing) { return "ROUGHLY$thing"; }; ±"fish" eq "ROUGHLYfish";', 'prefix operator overloading for new operator', :todo(1));
eval_ok('sub prefix:+ ($thing) { return "CROSS$thing"; }; +"fish" eq "CROSSfish";', 'prefix operator overloading for existing operator', :todo(1));
eval_ok('sub infix:(c)        ($text, $owner) { return "$text copyright $owner"; }; ("romeo & juliet" (c) "Shakespeare") eq "romeo & juliet copyright Shakespeare";', 'infix operator overloading for new operator', :todo(1));
eval_ok('sub postfix:&&&&&        ($wobble) { return "ANDANDAND$wobble"; }; ("boop"&&&&& eq "ANDANDANDboop";', 'postfix operator overloading for new operator', :todo(1));
my $var = 0;
eval_ok('macro circumfix:<!--...-->   ($text) { "" }; <!-- $var = 1; -->; $var == 0;', 'circumfix macro', :todo(1));
