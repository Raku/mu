#!/usr/bin/pugs

use Test;
use v6;

plan 6;

=head1 DESCRIPTION

This test tests the C<$!> builtin.

=cut

eval '&nonexisting_subroutine()';
ok $!, 'Calling a nonexisting subroutine sets $!';

undefine $!;
eval 'nonexisting_subroutine;';
ok $!, 'Calling a nonexisting subroutine sets $!';

undefine $!;
my $called;
sub foo(Str $s) { return $called++ };
my @a;
eval 'foo(@a,@a)';
ok $!, 'Calling a subroutine with a nonmatching signature sets $!';
ok !$called, 'The subroutine also was not called';

undefine $!;
eval '1 / 0';
ok $!, 'Dividing one by zero sets $!';

sub incr ( $a is rw ) { $a++ };
undefine $!;
eval 'incr(19)';
ok $!, 'Modifying a constant sets $!';
