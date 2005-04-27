#!/usr/bin/pugs

use v6;
use Test;

=kwid

Test basic escape continuations obtained from &?CALLER_CONTINUATION

=cut

plan 11;

sub simple1() returns Int {
  &?CALLER_CONTINUATION(2);
  return 1;
}
is(simple1(), 2, 'using ec instead of return');

sub simple2($n) {
  if ($n == 5) {
    &?CALLER_CONTINUATION(1);
  }
  return 0;
}
ok(simple2(5), 'ec used to escape ($n = 5)');
ok(!simple2(1), 'ec not used');

sub closure1 () returns Int {
  my $cont = &?CALLER_CONTINUATION;
  my $a = sub { $cont(5) };
  $a();
  return 6;
}
is(closure1(), 5, 'closure uses ec to escape', :todo);

sub call_argument($f) {
  return $f();
}
sub foo($f) returns Int {
  call_argument($f);
  return 3;
}
sub passing1 returns Int {
  foo(&?CALLER_CONTINUATION);
  return 2;
}
eval_is('passing1()', 8, 'ec passed as an argument', :todo);

sub is_five($n, $f) {
  if ($n == 5) {
    $f(1);
  }
  return 0;
}
sub passing2_not_cont($n) {
  my $a = 9;
  is_five($n, sub { $a = 1 });
  return $a;
}
sub passing2($n) {
  is_five($n, &?CALLER_CONTINUATION);
  return 9;
}
sub passing2_closure($n) {
  my $c = &?CALLER_CONTINUATION;
  is_five($n, sub { $c(1) });
  return 9;
}
is(passing2_not_cont(5), 1, 'is_five w/o ec');
is(passing2_not_cont(2), 9, 'is_five w/o ec');
is(passing2(5), 1, 'is_five passing ec itself', :todo);
is(passing2(2), 9, 'is_five passing ec itself');
is(passing2_closure(5), 1, 'is_five passing ec via closure', :todo);
is(passing2_closure(2), 9, 'is_five passing ec via closure');
