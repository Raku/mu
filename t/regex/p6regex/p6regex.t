use v6-alpha;

## test perl 6 regexes, requiring P5 regex to work

use Test;

plan 527;

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
  exit;
}

my @force_todo = (31,32,36,62,64,66,68,82,86,87,88,89,101,114,115,117,121,122,124,129,130,143,144,157,158,170,171,172,173,179,181,182,185,186,191,192,194,202,210,214,219,221,256,257,260,262,266,268,269,278,303,309,310,368,371,373,375,377,378,379,380,434,436,437,439,441,442,443,454,455,458,459,460,461,464,466,467,469,470,472,475,476,477,481,482,486,487,489,490,492,493,494,499,508,509,510,511,512,513,516,517,518,521,522,523,524,525);
my %force_todo = @force_todo.map:{($_,1)};

#my $test_data = slurp 'regex_tests';
#ok $test_data, 'slurp data successful';

# diag $test_data;

my $tests = $*PROGRAM_NAME;
$tests ~~ s:P5/p6regex.t/regex_tests/;

my $fh = open $tests;

sub p6rule_is( $target, $pattern, $description?, :$todo ) {
    eval "\$target ~~ rx/$pattern/";
    ok( !$! && $/, $description, :$todo );
    $! and diag $!;
}


sub p6rule_isnt( $target, $pattern, $description?, :$todo ) {
    eval "\$target ~~ rx/$pattern/";
    ok( !$! && !$/, $description, :$todo );
    $! and diag $!;
}


sub p6rule_like( $target, $pattern, $expected, $description?, :$todo ) {
    eval "\$target ~~ rx/$pattern/";
    like( $! ?? $! !! "$/", $expected, $description, :$todo );
}

my $line_count = 1;
for slurp($fh) -> $line {
    next unless $line ~~ rx:P5/\t/;
    next if $line ~~ rx:P5/^#/;
    chomp $line;

    my ($pattern, $target, $result, $description) = split rx:P5/\t+/, $line;

    $target ~~ s:P5/^'(.*)'$/$1/;
    $target ~~ s:P5:g/\\n/\n/;
    $target ~~ s:P5:g/\\r/\r/;
    $target ~~ s:P5:g/\\e/\e/;
    $target ~~ s:P5:g/\\t/\t/;
    $target ~~ s:P5:g/\\f/\f/;
    $target ~~ s:P5:g/\\(\d{3})/{chr(:8($1))}/;
    $target ~~ s:P5:g/\\x\[([a-fA-F0-9]+)\]/{chr(:16($1))}/;
    $target ~~ s:P5:g/\\x([a-fA-F0-9]+)/{chr(:16($1))}/;

    my $todo;

    if $description ~~ rx:P5 {TODO:} {
	$todo = $description;
    } else {
        $todo = 'for release' if %force_todo{$line_count};
    }

    if $result ~~ rx:P5 {^/(.*)/$} {
        p6rule_like($target, $pattern, rx:P5/$1/, $description, :$todo);
    }
    elsif $result eq 'y' {
        p6rule_is($target, $pattern, $description, :$todo);
    }
    elsif $result eq 'n' {
        p6rule_isnt($target, $pattern, $description, :$todo);
    }
    else { die "bug\n$line" }

    sleep 1 unless ($line_count++ % 50);
}
