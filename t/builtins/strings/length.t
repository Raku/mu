#!/usr/bin/pugs

use v6;
require Test;

=kwid

Various length tests (though "length" should not be used)

This does not test .chars, which is language dependent and needs
more careful tests.

http://www.unicode.org/unicode/reports/tr11/

=cut

plan 19;

# string literals, for sanity

is("".bytes,         0, "empty string");
is("moose".bytes,    5, "moose");
my $x = undef; 
eval_is('$x.bytes',  undef, "undef"); #  test for warning

# and the real tests.

# Please add test strings in your favorite script, especially if
# it is boustrophedonic or otherwise interesting.

my $data = [
    # string       octets codepoints grapheme chars
	[ "",               0,        0,         0 ,  0],
	[ "moose",          5,        5,         5 ,  5],
    [ "בדיקה",         10,        5,         5 , 5],
    [ "בדיקה 123",     14,        9,         9 , 9]   # XXX: trailing commas parsefail for now
];
#:map { my %hash; %hash<string bytes codes graphs> = $_; \%hash };

for $data -> $row {
	my ($string, $bytes, $codes, $graphs, $chars) = $row;
	is($string.bytes, $bytes, "'{$string}'.bytes");
	is($string.chars, $chars, "'{$string}'.chars");
	eval_is ('$string.codes', $codes, "'{$string}'.codes");
	eval_is('$string.graphs', $graphs, "'{$string}'.graphs");
}
