#!/usr/bin/pugs
use v6;
use Test;

my %unicode = (
	'a' => "\x61",
	'æ' => "\xc3\xa6",
	'喃' => "\xe5\x96\x83",
	'�' => "\xf0\xa8\xae\x81"
);

for %unicode.kv -> $literal, $codepoint {
	is(
		try { $codepoint },
		$literal,
		'Does a character codepoint (\x..) evaluate to the same thing as its literal?',
		:todo<bug>
	);
}
