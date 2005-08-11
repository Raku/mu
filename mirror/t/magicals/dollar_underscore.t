#!/usr/bin/pugs

use Test;
use v6;

plan 6;

=head1 DESCRIPTION

This test tests the C<$!> builtin.

=cut

try { &nonexisting_subroutine() };
ok $!, 'Calling a nonexisting subroutine sets $!';

undefine $!;
try { nonexisting_subroutine; };
ok $!, 'Calling a nonexisting subroutine sets $!';

undefine $!;
my $called;
sub foo(Str $s) { return $called++ };
my @a;
try { foo(@a,@a) };
ok $!, 'Calling a subroutine with a nonmatching signature sets $!';
ok !$called, 'The subroutine also was not called';

undefine $!;
try { 1 / 0 };
ok $!, 'Dividing one by zero sets $!';

sub incr ( $a is rw ) { $a++ };
undefine $!;
try { incr(19) };
ok $!, 'Modifying a constant sets $!';
