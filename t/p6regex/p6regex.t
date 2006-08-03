use v6-alpha;

## test perl 6 regexes

use Test;


plan 3;


#my $test_data = slurp 'regex_tests';
#ok $test_data, 'slurp data successful';

# diag $test_data;


p6rule_is  ( 'foo', 'f(oo)**{2}', 'p6rule_is: simple test' );
p6rule_isnt( 'foo', 'f(oo)**{3}', 'p6rule_isnt: simple test' );
p6rule_like( 'foo', 'f(oo)**{2}', 'foo', 'p6rule_like: simple test' );


sub p6rule_is( $target, $pattern, $description?, :%todo ) {
    my $match = $target ~~ /$pattern/;
    ok( !$! && $match, $description, %todo );
    $! and diag $!;
}


sub p6rule_isnt( $target, $pattern, $description?, :%todo ) {
    my $match = $target ~~ /$pattern/;
    ok( !$! && !$match, $description, %todo );
    $! and diag $!;
}


sub p6rule_like( $target, $pattern, $expected, $description?, :%todo ) {
    my $match = $target ~~ /$pattern/;
    like( $! ?? $! !! "$match", $expected, $description, %todo );
}
