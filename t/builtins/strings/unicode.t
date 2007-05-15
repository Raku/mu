use v6-alpha;

use Test;
plan 9;
#L<S02/Literals/"Characters indexed by hex numbers">
my %unicode = (
	'a'  => "\x61",
	'æ'  => "\xE6",
	'喃' => "\x5583",
	'𨮁' => "\x28B81",
);

for %unicode.kv -> $literal, $codepoint {
	is(
		$codepoint,
		$literal,
		'Does a character codepoint (\x..) evaluate to the same thing as its literal?'
	);
}

#L<S02/"Built-In Data Types"/".bytes, .codes or .graphs">
is("møøse".perl.chars, 7, 'Is .perl handling unicode now?');

# LATIN CAPITAL LETTER A, COMBINING GRAVE ACCENT
my Str $u = "\x[0041,0300]";
is $u.bytes, 3, 'combining À is three bytes (assumes utf8)';
is $u.codes, 2, 'combining À is two codes';
is $u.graphs, 1, 'combining À is one graph';
is $u.chars, 1, 'combining À is one graph - .chars defaults to .graphs';
