use v6-alpha;

=pod

Temporary test illustrating an current oddity.
Watch the output and see what happens to the counter after test 9.

=cut


use Test;

# XXX: add sanity check to see we can run the test or skip

plan 61;

my @force_todo = (1..2,6,31);
my %force_todo = @force_todo.map:{($_,1)};

#my $test_data = slurp 'regex_tests';
#ok $test_data, 'slurp data successful';

# diag $test_data;

my $tests = $*PROGRAM_NAME;
$tests ~~ s:P5/p6regex_oddity.t/p6regex_oddity_input/;

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

    say $line_count;

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
