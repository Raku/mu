#!/usr/bin/pugs

use v6;
use Test;

=kwid

Various length tests (though "length" should not be used)

This does not adequately test .chars, which is language dependent 
and needs more careful tests.

http://www.unicode.org/unicode/reports/tr11/

=cut

plan 31;

# string literals, for sanity

is("".bytes,         0, "empty string");
is("moose".bytes,    5, "moose");
my $x = undef; 
is($x.bytes, 0, "undef"); #  test for warning

# and the real tests.

# Please add test strings in your favorite script, especially if
# it is boustrophedonic or otherwise interesting.
my @stringy = <@stringy>;
my @data = (
    # string       octets codepoints grapheme chars
    ",               0,        0,         0,  0",
    "moose,          5,        5,         5,  5",
    "C:\\Program Files,           16,        16,         16,  16",
    ~@stringy ~ ",           8,          8,         8,   8",
    "\x020ac \\x020ac,           11,        9,         9,  9",
    "בדיקה,         10,        5,         5,  5",
    "בדיקה 123,     14,        9,         9,  9",
);
#:map { my %hash; %hash<string bytes codes graphs> = $_; \%hash };

for @data -> $row {
    my ($string, $bytes, $codes, $graphs, $chars) = split(rx:perl5/,\s*/, $row);
    is($string.bytes, $bytes, "'{$string}'.bytes");
    is($string.chars, $chars, "'{$string}'.chars");
    is($string.codes, $codes, "'{$string}'.codes");
    is($string.graphs, $graphs, "'{$string}'.graphs");
}
