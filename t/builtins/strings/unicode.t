use v6-alpha;

use Test;
plan 48;
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
# Speculation, .chars is unspecced
is $u.chars, 1, '.chars defaults to .graphs';
use_ok 'Bytes', 'use bytes works';
is $u.chars, 3, '.chars as bytes';
use_ok 'Codes', 'use codes works';
is $u.chars, 2, '.chars as codes';
use_ok 'Graphs', 'use graphs works';
is $u.chars, 1, '.chars as graphs';
use_ok 'utf16', 'use utf16 works';
is $u.bytes, 4, '.bytes in utf16';
use_ok 'utf32', 'use utf16 works';
is $u.bytes, 8, '.bytes in utf32';
use_ok 'utf8', 'use utf8 works';
is $u.bytes, 3, '.bytes in utf8';

#L<S02/"Built-In Data Types"/"coerce to the proper units">
$u = "\x[41,
        E1,
        41, 0300,
        41, 0302, 0323,
        E0]";
# in utf8 mode here
is eval('substr $u, 3.as(Bytes), 1.as(Bytes)'), "\x[41]", 'substr with Bytes as units';
is eval('substr $u, 3.as(Codes), 1.as(Codes)'), "\x[0300]", 'substr with Codes as units';
is eval('substr $u, 4.as(Graphs), 1.as(Graphs)'), "\x[E0]", 'substr with Graphs as units';
is eval('substr $u, 3.as(Graphs), 1.as(Codes)'), "\x[41]", 'substr with Graphs and Codes as units 1';
is eval('substr $u, 4.as(Codes), 1.as(Graphs)'), "\x[41, 0302, 0323]", 'substr with Graphs and Codes as units 2';
is eval('substr $u, 4.as(Bytes), 1.as(Codes)'), "\x[0300]", 'substr with Bytes and Codes as units 1';
is eval('substr $u, 1.as(Codes), 2.as(Bytes)'), "\x[E1]", 'substr with Bytes and Codes as units 2';
is eval('substr $u, 3.as(Bytes), 1.as(Graphs)'), "\x[41, 0300]", 'substr with Bytes and Graphs as units 1';
is eval('substr $u, 3.as(Graphs), 1.as(Bytes)'), "\x[41]", 'substr with Bytes and Graphs as units 2';
eval 'use utf16';
is eval('substr $u, 4.as(Bytes), 2.as(Bytes)'), "\x[41]", 'substr with Bytes as units';
is eval('substr $u, 3.as(Codes), 1.as(Codes)'), "\x[0300]", 'substr with Codes as units';
is eval('substr $u, 4.as(Graphs), 1.as(Graphs)'), "\x[E0]", 'substr with Graphs as units';
is eval('substr $u, 3.as(Graphs), 1.as(Codes)'), "\x[41]", 'substr with Graphs and Codes as units 1';
is eval('substr $u, 4.as(Codes), 1.as(Graphs)'), "\x[41, 0302, 0323]", 'substr with Graphs and Codes as units 2';
is eval('substr $u, 6.as(Bytes), 1.as(Codes)'), "\x[0300]", 'substr with Bytes and Codes as units 1';
is eval('substr $u, 1.as(Codes), 2.as(Bytes)'), "\x[E1]", 'substr with Bytes and Codes as units 2';
is eval('substr $u, 4.as(Bytes), 1.as(Graphs)'), "\x[41, 0300]", 'substr with Bytes and Graphs as units 1';
is eval('substr $u, 3.as(Graphs), 2.as(Bytes)'), "\x[41]", 'substr with Bytes and Graphs as units 2';
eval 'use utf32';
is eval('substr $u, 8.as(Bytes), 4.as(Bytes)'), "\x[41]", 'substr with Bytes as units';
is eval('substr $u, 3.as(Codes), 1.as(Codes)'), "\x[0300]", 'substr with Codes as units';
is eval('substr $u, 4.as(Graphs), 1.as(Graphs)'), "\x[E0]", 'substr with Graphs as units';
is eval('substr $u, 3.as(Graphs), 1.as(Codes)'), "\x[41]", 'substr with Graphs and Codes as units 1';
is eval('substr $u, 4.as(Codes), 1.as(Graphs)'), "\x[41, 0302, 0323]", 'substr with Graphs and Codes as units 2';
is eval('substr $u, 12.as(Bytes), 1.as(Codes)'), "\x[0300]", 'substr with Bytes and Codes as units 1';
is eval('substr $u, 1.as(Codes), 4.as(Bytes)'), "\x[E1]", 'substr with Bytes and Codes as units 2';
is eval('substr $u, 8.as(Bytes), 1.as(Graphs)'), "\x[41, 0300]", 'substr with Bytes and Graphs as units 1';
is eval('substr $u, 3.as(Graphs), 4.as(Bytes)'), "\x[41]", 'substr with Bytes and Graphs as units 2';
