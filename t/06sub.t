#!/usr/bin/pugs

use v6;
require Test;

#say "1..12";

sub foobar ($var) {
    return $var;
}

my $foo = "foo";
my $bar;
eval '$bar = foobar($foo); ';
is($foo, $bar, 'subroutine at beginning');
$bar = "";
eval '$bar = check $foo';
todo_ok($bar, 'subroutine at end');

sub check {
    return $_;
}

sub twice { $_ * 2 }
ok(twice(5) == 10);
#if (&twice(5 - 3) == 4) { say "ok 4" } else { say "not ok 4" }
todo_ok(eval 'twice(5 - 3) == 4');

my $_;

sub callerunderscore {
    return "wrong one, needed to avoid errors"
}

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
  todo_is(callerunderscore(), '-quux-', 'CALLER:: $_ set by for');
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
