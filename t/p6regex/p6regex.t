use v6-alpha;

## test perl 6 regexes

use Test;


plan 2;


my $test_data = slurp 'regex_tests';
ok $test_data, 'slurp data successful';

# diag $test_data;

p6rule_is( 'foo', 'f(oo)**{2}', 'simple test' );


exit;


sub p6rule_is( $target, $pattern, $description?, :%todo ) {
    my $match = $target ~~ /$pattern/;
    ok( ! $! ?& $match, $description, %todo );
    $! and diag $!;
}
