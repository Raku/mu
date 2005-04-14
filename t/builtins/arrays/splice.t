#!/usr/bin/pugs

require Test;
use v6;

=head1 DESCRIPTION

This test tests the C<splice> builtin, see S29 and Perl 5's perlfunc.

Ported from the equivalent Perl 5 test.

=cut

plan 11;
my @a = (1..10);

todo_eval_ok '~splice(@a,@a,0,11,12) eq "" && ~@a eq ~[1..12]';

todo_eval_ok '~splice(@a,-1) eq "12" && ~@a eq ~[1..11]';

todo_eval_ok '~splice(@a,0,1) eq "1" && ~@a eq ~[2..11]';

todo_eval_ok '~splice(@a,0,0,0,1) eq "" && ~@a eq ~[0..11]';

todo_eval_ok '~splice(@a,5,1,5) eq "5" && ~@a eq ~[0..11]';

todo_eval_ok '~splice(@a, @a, 0, 12, 13) eq "" && ~@a eq ~[0..13]';

todo_eval_ok '~splice(@a, -@a, @a, 1, 2, 3) eq ~[0.13] && ~@a eq ~[1..3]';

todo_eval_ok '~splice(@a, 1, -1, 7, 7) eq "2" && ~@a eq ~[1,7,7,3]';

todo_eval_ok '~splice(@a,-3,-2,2) eq ~[7] && ~@a eq ~[1,2,7,3]';

# Bug 20000223.001 - no test for splice(@array).  Destructive test!
todo_eval_ok '~splice(@a) eq ~[1,2,7,3] && ~@a eq \'\'';

my $foo;
@a   = ('red', 'green', 'blue');
$foo = eval 'splice @a, 1, 2';
todo_is $foo, "blue";
