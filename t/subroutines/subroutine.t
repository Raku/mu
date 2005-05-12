#!/usr/bin/pugs

use v6;
use Test;

=kwid

Tests for Synopsis 6

=cut

plan 16;

sub foobar ($var) {
    return $var;
}

my $foo = "foo";
my $bar;
eval '$bar = foobar($foo); ';
is($foo, $bar, 'subroutine at beginning');
$bar = "";
eval '$bar = check $foo';
ok($bar, 'subroutine at end');

sub check {
    return $_;
}

sub twice { $_ * 2 }
ok(twice(5) == 10);
#if (&twice(5 - 3) == 4) { say "ok 4" } else { say "not ok 4" }
ok(eval 'twice(5 - 3) == 4');

my $_;

sub callerunderscore (?$foo = $CALLER::_) {
    return "-" ~ $foo ~ "-"
}

is(callerunderscore("foo"), "-foo-", 'CALLER:: string arg');
is(callerunderscore(1), "-1-", 'CALLER:: number arg');
$_ = "foo";
is(callerunderscore(), "-foo-", 'CALLER:: $_ set once');
$_ = "bar";
is(callerunderscore(), "-bar-", 'CALLER:: $_ set twice');
for ("quux") {
  is(callerunderscore(), '-quux-', 'CALLER:: $_ set by for');
}
is(callerunderscore(), '-bar-', 'CALLER:: $_ reset after for');

# Check that closures are closed over variables they do use
# if they don't undefined variable exceptions get thrown
sub createclosure_sub () {
  my $a = "-wibble-";
  return sub { $a };
}
sub createclosure_block () {
  my $a = "-quux-";
  return { $a };
}
my $sub = createclosure_sub();
my $block = createclosure_block();
my $_ = "not-wibble-or-quux";
$_ = $sub.();
is($_, "-wibble-", 'sub closures close');
$_ = $block.();
is($_, "-quux-", 'block closures close');

my @result;
sub perl5sub {
    push @result, @_[0];
    push @result, @_[1];
}
perl5sub(<foo bar>);
is(@result, [<foo bar>], 'use @_ in sub');

=pod

L<S06/"Unpacking array parameters">

=cut

sub argShifter (@a) {
	my $first := shift @a;
	return $first;
}

#fail("FIXME parsefail", :todo); # actually exe fail...
#is eval 'argShifter(3..5)', 3, "use shift on an array argument";
eval_is 'argShifter(3..5)', 3, "use shift on an array argument", :todo;

eval 'sub unpack_array ([$first, *@rest]) { return $first; }';

my @array = 3..7;
is(eval 'unpack_array(@array)', 3, 'unpacking an array parameter', :todo);

=pod

L<S06/"Unpacking hash parameters">

=cut

eval 'sub unpack_hash({+$yo, *%other}){ return $yo; }';

my %params = yo => 3, nope => 4;
is(eval 'unpack_hash(%params)', 3, 'unpacking a hash parameter', :todo);
