use v6-alpha;
use Test;
plan 5;

# L<S29/"Control::Basic"/"=item eval">

=pod

Tests for the eval() builtin

=cut


if $?PUGS_BACKEND ne "BACKEND_PUGS" {
  skip_rest "PIL2JS and PIL-Run do not support eval() yet.";
  exit;
}

# eval should evaluate the code in the lexical scope of eval's caller
sub make_eval_closure { my $a = 5; sub ($s) { eval $s } };
is(make_eval_closure()('$a'), 5);

is(eval('5'), 5);
my $foo = 1234;
is(eval('$foo'), $foo);

# traps die?
ok(!eval('die; 1'));

ok(!eval('my @a = (1); @a<0>'), "eval returns undef on syntax error");
