use v6-alpha;

## test perl 6 regexes, requiring P5 regex to work

use Test;

# XXX: add sanity check to see we can run the test or skip

plan 525;

my @force_todo = (2,24..25,27,31..32,36,61,63,65,67,82,86,100,113..117,120..124,128..129,142..143,156..157,169..172,178,180..181,183..186,189..193,196..197,203..205,211..214,217..220,223..224,229..231,236,255..256,259,261,265,267..268,277,301..303,308..309,321..322,367,370,372,374,376..379,381..382,425..428,430..431,433,435..436,438,440..442,444..447,450..454,457..460,463,465..466,468..469,471..472,474..476,480..481,485..486,488..489,491..493,498,507..512,515..517,520..525);
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

    if $result eq 'y' {
        p6rule_is($target, $pattern, $description, :$todo);
    }

    if $result eq 'n' {
        p6rule_isnt($target, $pattern, $description, :$todo);
    }

    sleep 1 unless ($line_count++ % 50);
}
