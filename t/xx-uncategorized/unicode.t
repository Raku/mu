use v6-alpha;

use Test;
plan 5;

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

is("møøse".perl.chars, 7, 'Is .perl handling unicode now?');
