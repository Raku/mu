use v6-alpha;

## test perl 6 regexes, requiring p5 regex to work

use Test;

# XXX: add sanity check to see we can run the test or skip

plan 494;


#my $test_data = slurp 'regex_tests';
#ok $test_data, 'slurp data successful';

# diag $test_data;

my $tests = $*PROGRAM_NAME;
$tests ~~ s:p5/p6regex.t/regex_tests/;

my $fh = open $tests;

sub p6rule_is( $target, $pattern, $description?, :$todo ) {
    try { $target ~~ /$pattern/ };
    ok( !$! && $/, $description, :$todo );
    $! and diag $!;
}


sub p6rule_isnt( $target, $pattern, $description?, :$todo ) {
    try { $target ~~ /$pattern/ };
    ok( !$! && !$/, $description, :$todo );
    $! and diag $!;
}


sub p6rule_like( $target, $pattern, $expected, $description?, :$todo ) {
    try { $target ~~ /$pattern/ };
    like( $! ?? $! !! "$/", $expected, $description, :$todo );
}

while (my $line = =$fh) {
    next unless $line ~~ rx:perl5/\t/;
    next if $line ~~ rx:perl5/^#/;
    chomp $line;

    my ($pattern, $target, $result, $description) = split rx:perl5/\t+/, $line;

    $target ~~ s:p5/^'(.*)'$/$1/;
    $target ~~ s:p5:g/\\n/\n/;
    $target ~~ s:p5:g/\\r/\r/;
    $target ~~ s:p5:g/\\e/\e/;
    $target ~~ s:p5:g/\\t/\t/;
    $target ~~ s:p5:g/\\f/\f/;
    $target ~~ s:p5:g:e/\\(\d{3})/chr(oct($1))/;
    $target ~~ s:p5:g:e/\\x(..)/chr(hex($1))/;

    my $todo;

    if $description ~~ rx:perl5{TODO:} {
	$todo = $description;
    }

    if $result ~~ rx:perl5{^/(.*)/$} {
        p6rule_like($target, $pattern, rx:perl5/$1/, $description, :$todo);
    }

    if $result eq 'y' {
        p6rule_is($target, $pattern, $description, :$todo);
    }

    if $result eq 'n' {
        p6rule_isnt($target, $pattern, $description, :$todo);
    }
}
