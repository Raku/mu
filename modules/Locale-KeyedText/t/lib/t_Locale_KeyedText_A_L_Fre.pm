#!pugs
use v6;

module t_Locale_KeyedText_A_L_Fre;

my Str $xy ::= 'AF';
my Str %text_strings ::= (
	'one' => $xy~' - word {fork} { fork } {spoon} {{fork}}',
	'two' => $xy~' - sky pie rye',
);

sub get_text_by_key( Str $msg_key ) returns Str { return $text_strings{$msg_key}; }

1;
