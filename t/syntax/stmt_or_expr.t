use v6;

use Test;

plan 3;

my $x = do {
	10
} + 1;

is($x, 11, "'} + 1' is in a single statement");

my $y = do {
	10
}
+ 1;

# L<A04/"RFC 022: Control flow: Builtin switch statement" /the final curly is on a line by itself/>

is($y, 10, "}\\n + 1 are two statements");

my $z = [];
eval q:to/EOC/
    $x = [ do { 1 }
            + 2 ];
    EOC
;

is($z[0], 3, 'auto-curly doesn\'t apply unless we\'re at top level', :todo<bug>);
