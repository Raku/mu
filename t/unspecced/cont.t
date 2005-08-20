#!/usr/bin/pugs

use v6;
use Test;

=kwid

Test basic escape continuations obtained from &?CALLER_CONTINUATION

=cut

plan 14;

sub simple1() returns Int {
  &?CALLER_CONTINUATION(2);
  return 1;
}
is(simple1(), 2, 'using ec instead of return');

sub simple2(Num $n) {
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

sub call_argument(Code $f, $arg) {
  return $f($arg);
}
sub foo(Code $f) returns Int {
  call_argument($f, 8);
  return 3;
}
sub passing1 () returns Int {
  foo(&?CALLER_CONTINUATION);
  return 2;
}
is(try { passing1() }, 8, 'ec passed as an argument', :todo);

sub is_five(Num $n, Code $f) {
  if ($n == 5) {
    $f(1);
  }
  return 0;
}
sub passing2_not_cont(Num $n) {
  my $a = 9;
  is_five($n, sub { $a = 1 });
  return $a;
}
sub passing2(Num $n) {
  is_five($n, &?CALLER_CONTINUATION);
  return 9;
}
sub passing2_closure(Num $n) {
  my $c = &?CALLER_CONTINUATION;
  is_five($n, sub { $c(1) });
  return 9;
}
is(passing2_not_cont(5), 1, 'is_five w/o ec (1)');
is(passing2_not_cont(2), 9, 'is_five w/o ec (2)');
is(passing2(5), 1, 'is_five passing ec itself (1)', :todo);
is(passing2(2), 9, 'is_five passing ec itself (2)');
is(passing2_closure(5), 1, 'is_five passing ec via closure (1)', :todo);
is(passing2_closure(2), 9, 'is_five passing ec via closure (2)');

sub callconty() {
    conty(&?CALLER_CONTINUATION);
    return 1;
}
sub conty(Code $c) {
    $c(2);
    return 3;
}
is(callconty(), 2, 'continuation bug', :todo<bug>);

# Now test complicated full continuations got from the same place.

{
  sub callcc (Code &block) {  &block(&?CALLER_CONTINUATION) }

  my $cont;
  my $counter = 0;

  callcc -> $cc { $cont = $cc };
  $counter++;
  $cont(undef) unless $counter == 3;
  is($counter, 3, "Looping with a full continuation", :todo<feature>);
}

# Really evil: Store continuations in an aggregate
{
  my %conts;
  my $foo = sub { %conts<foo> = &?CALLER_CONTINUATION; return "normalfoo" };
  my $bar = sub { %conts<bar> = &?CALLER_CONTINUATION; return "normalbar" };

  my $history;
  my $counter = 0;

  $history ~= $foo(); $history ~= "foo$counter"; $counter++;
  $history ~= $bar(); $history ~= "bar$counter"; $counter++;
  %conts<foo>("secondfoo") if $counter <= 2;
  %conts<bar>("secondbar") if $counter <= 4;

  is
    $history,
    "normalfoofoo0normalbarbar1secondfoofoo2normalbarbar3secondbarbar4",
    "continuations stored in an aggregate with loops";
}
