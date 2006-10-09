use v6-alpha;

## test perl 6 regexes, requiring P5 regex to work

use Test;

plan 527;

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
  exit;
}

my @force_todo = (2,24..25,27,31..32,36,62,64,66,68,83,87,101,114..118,121..125,129..130,143..144,157..158,170..173,179,181..182,184..187,190..194,197..198,204..206,212..215,218..221,224..225,230..232,237,256..257,260,262,266,268..269,278,302..304,309..310,322..323,368,371,373,375,377..380,382..383,426..429,431..432,434,436..437,439,441..443,445..448,451..455,458..461,464,466..467,469..470,472..473,475..477,481..482,486..487,489..490,492..494,499,508..513,516..518,521..525);
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
