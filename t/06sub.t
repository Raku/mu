use v6;

my $loop = 1;
say "1..12";

sub foobar ($var) {
    return $var;
}

my $foo = "foo";
my $bar;
eval '$bar = foobar($foo); ';
if ($foo eq $bar) {
    say "ok 1 # subroutine at beginning";
} else {
    say "not ok 1 # subroutine at beginning";
}
$bar = "";
eval '$bar = check $foo';
if ($bar) { say "ok 2"; } else { say "not ok 2 # TODO subroutine at end"; }

sub check {
    return $_;
}

sub twice { $_ * 2 }
if (twice(5) == 10) { say "ok 3" } else { say "not ok 3" }
#if (&twice(5 - 3) == 4) { say "ok 4" } else { say "not ok 4" }
if (eval 'twice(5 - 3) == 4') { say "ok 4 # TODO" } else { say "not ok 4 # TODO" }

my $_;

sub callerunderscore {
    return "wrong one, needed to avoid errors"
}

sub callerunderscore (?$foo = $CALLER::_) { 
    return "-" ~ $foo ~ "-"
}

if (callerunderscore("foo") eq "-foo-") { say "ok 5" } else { say "not ok 5 # TODO CALLER::" }
if (callerunderscore(1) eq "-1-") { say "ok 6" } else { say "not ok 6 # TODO CALLER::" }
$_ = "foo";
if (callerunderscore() eq "-foo-") { say "ok 7" } else { say "not ok 7 # TODO CALLER::" }
$_ = "bar";
if (callerunderscore() eq "-bar-") { say "ok 8" } else { say "not ok 8 # TODO CALLER::" }
for ("quux") { 
    if (callerunderscore() eq "-quux-") { say "ok 9" } else { say "not ok 9 # TODO CALLER::" }
}
if (callerunderscore() eq "-bar-") { say "ok 10" } else { say "not ok 10 # TODO CALLER::" }

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
if ($_ eq "-wibble-") { say "ok 11" } else { say "not ok 11" }
$_ = $block.();
if ($_ eq "-quux-") { say "ok 12" } else { say "not ok 12" }
