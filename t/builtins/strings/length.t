#!/usr/bin/pugs

use v6;
require Test;

=kwid

Various length tests (though "length" should not be used)

This does not test .chars, which is language dependent and needs
more careful tests.

http://www.unicode.org/unicode/reports/tr11/

=cut

plan 15;

# string literals, for sanity

is(eval '"".bytes',         0, "empty string");
is(eval '"moose".bytes',    5, "moose");
is(eval 'undef.bytes',      0, "undef"); # TODO: test for warning

# and the real tests.

# Please add test strings in your favorite script, especially if
# it is boustrophedonic or otherwise interesting.

my $data = [
    # string       octets codepoints graphemes
	[ "",               0,        0,         0 ],
	[ "moose",          5,        5,         5 ],
    [ "בדיקה",         10,        5,         5 ],
    [ "בדיקה 123",     14,        9,         9 ]   # XXX: trailing commas parsefail for now
];
#:map { my %hash; %hash<string bytes codes graphs> = $_; \%hash };

#for @data -> $spec {
for $data -> $string, $bytes, $codes, $graphs {
	#$spec.perl.say; 
	#todo_is(eval '$spec<string>.bytes', $spec<bytes>, "'{$spec<string>}'.bytes");
	#todo_is(eval '$spec<string>.codes', $spec<codes>, "'{$spec<string>}'.codes");
	#todo_is(eval '$spec<string>.graphs', $spec<graphs>, "'{$spec<string>}'.graphs");

	#($string, $bytes, $codes, $graphs).perl.say;
        is(eval '$string.bytes', $bytes, "'{$string}'.bytes");
	todo_is(eval '$string.codes', $codes, "'{$string}'.codes");
	todo_is(eval '$string.graphs', $graphs, "'{$string}'.graphs");
}
