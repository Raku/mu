#!pugs
use v6;

require Test;

plan( 102 );

@*INC.unshift( 't/lib' ); # Note: I did "use lib 't/lib';" in Perl 5.
require Locale::KeyedText-0.0.3;

######################################################################
# Here are some utility methods:

sub message( Str $detail ) {
	say "-- $detail";
}

sub serialize( Any $input ) returns Str {
	return (
		$input.meta.isa(Hash) ?? 
			( '{ ', ( $input.pairs.sort.map:{ serialize( $_ ) } ), '}, ' ) 
		:: $input.meta.isa(Array) ?? 
			( '[ ', ( $input.map:{ serialize( $_ ) } ), '], ' ) 
		:: $input.meta.isa(Pair) ?? 
			'\''~$input.key~'\' => \''~$input.value~'\', '
		:: $input.defined ??
			'\''~$input~'\', '
		:: 'undef, '
	).join( '' );
}

######################################################################
# Now perform the actual tests:

message( 'START TESTING Locale::KeyedText' );

######################################################################

{
	message( 'testing new_message() and Message object methods' );

	my ($did, $should, $msg1);

	$did = serialize( Locale::KeyedText.new_message() );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_message() returns '$did'" );

	$did = serialize( Locale::KeyedText.new_message( undef ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_message( undef ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_message( '' ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_message( '' ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_message( '0 ' ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_message( '0 ' ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_message( 'x-' ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_message( 'x-' ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_message( 'x:' ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_message( 'x:' ) returns '$did'" );

	$msg1 = Locale::KeyedText.new_message( '0' );
	isa_ok( $msg1, "Locale::KeyedText::Message", 
		"msg1 = new_message( '0' ) ret MSG obj" );
	$did = $msg1.as_string();
	$should = '0: ';
	is( $did, $should, "on init msg1.as_string() returns '$did'" );

	$msg1 = Locale::KeyedText.new_message( 'zZ9' );
	isa_ok( $msg1, "Locale::KeyedText::Message", 
		"msg1 = new_message( 'zZ9' ) ret MSG obj" );
	$did = $msg1.as_string();
	$should = 'zZ9: ';
	is( $did, $should, "on init msg1.as_string() returns '$did'" );

	$did = serialize( Locale::KeyedText.new_message( 'foo', [] ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_message( 'foo', [] ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_message( 'foo', { ' '=>'g' } ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_message( 'foo', \{ ' '=>'g' \} ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_message( 'foo', { ':'=>'g' } ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_message( 'foo', \{ ':'=>'g' \} ) returns '$did'" );

	$msg1 = Locale::KeyedText.new_message( 'foo', undef );
	isa_ok( $msg1, "Locale::KeyedText::Message", 
		"msg1 = new_message( 'foo', undef ) ret MSG obj" );
	$did = $msg1.as_string();
	$should = 'foo: ';
	is( $did, $should, "on init msg1.as_string() returns '$did'" );

	$msg1 = Locale::KeyedText.new_message( 'foo', {} );
	isa_ok( $msg1, "Locale::KeyedText::Message", 
		"msg1 = new_message( 'foo', {} ) ret MSG obj" );
	$did = $msg1.as_string();
	$should = 'foo: ';
	is( $did, $should, "on init msg1.as_string() returns '$did'" );

	$msg1 = Locale::KeyedText.new_message( 'foo', { 'bar' => 'baz' } );
	isa_ok( $msg1, "Locale::KeyedText::Message", 
		"msg1 = new_message( 'foo', { 'bar' => 'baz' } ) ret MSG obj" );
	$did = $msg1.as_string();
	$should = 'foo: bar=baz';
	is( $did, $should, "on init msg1.as_string() returns '$did'" );

	$msg1 = Locale::KeyedText.new_message( 'foo', { 'bar'=>'baz','c'=>'-','0'=>'1','z'=>'','y'=>'0' } );
	isa_ok( $msg1, "Locale::KeyedText::Message", 
		"msg1 = new_message( 'foo', { 'bar'=>'baz','c'=>'d','0'=>'1','z'=>'','y'=>'0' } ) ret MSG obj" );
	$did = $msg1.as_string();
	$should = 'foo: 0=1, bar=baz, c=-, y=0, z=';
	is( $did, $should, "on init msg1.as_string() returns '$did'" );

	$did = serialize( $msg1.get_message_key() );
	$should = '\'foo\', ';
	is( $did, $should, "on init msg1.get_message_key() returns '$did'" );

	$did = serialize( $msg1.get_message_variable() );
	$should = 'undef, ';
	is( $did, $should, "on init msg1.get_message_variable() returns '$did'" );

	$did = serialize( $msg1.get_message_variable( undef ) );
	$should = 'undef, ';
	is( $did, $should, "on init msg1.get_message_variable( undef ) returns '$did'" );

	$did = serialize( $msg1.get_message_variable( '' ) );
	$should = 'undef, ';
	is( $did, $should, "on init msg1.get_message_variable( '' ) returns '$did'" );

	$did = serialize( $msg1.get_message_variable( '0' ) );
	$should = '\'1\', ';
	is( $did, $should, "on init msg1.get_message_variable( '0' ) returns '$did'" );

	$did = serialize( $msg1.get_message_variable( 'zzz' ) );
	$should = 'undef, ';
	is( $did, $should, "on init msg1.get_message_variable( 'zzz' ) returns '$did'" );

	$did = serialize( $msg1.get_message_variable( 'bar' ) );
	$should = '\'baz\', ';
	is( $did, $should, "on init msg1.get_message_variable( 'bar' ) returns '$did'" );

	$did = serialize( $msg1.get_message_variables() );
	$should = '{ \'0\' => \'1\', \'bar\' => \'baz\', \'c\' => \'-\', \'y\' => \'0\', \'z\' => \'\', }, ';
	is( $did, $should, "on init msg1.get_message_variables() returns '$did'" );
}

######################################################################

{
	message( 'testing new_translator() and most Translator object methods' );

	my ($did, $should, $trn1);

	$did = serialize( Locale::KeyedText.new_translator() );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_translator() returns '$did'" );

	$did = serialize( Locale::KeyedText.new_translator( undef, undef ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_translator( undef, undef ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_translator( [], undef ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_translator( [], undef ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_translator( undef, [] ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_translator( undef, [] ) returns '$did'" );

	$trn1 = Locale::KeyedText.new_translator( [], [] );
	isa_ok( $trn1, "Locale::KeyedText::Translator", 
		"trn1 = new_translator( [], [] ) ret TRN obj" );
	$did = $trn1.as_string();
	$should = 'SETS: ; MEMBERS: ';
	is( $did, $should, "on init trn1.as_string() returns '$did'" );

	$did = serialize( Locale::KeyedText.new_translator( '', [] ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_translator( '', [] ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_translator( '0 ', [] ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_translator( '0 ', [] ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_translator( 'x-', [] ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_translator( 'x-', [] ) returns '$did'" );

	$trn1 = Locale::KeyedText.new_translator( '0', [] );
	isa_ok( $trn1, "Locale::KeyedText::Translator", 
		"trn1 = new_translator( '0', [] ) ret TRN obj" );
	$did = $trn1.as_string();
	$should = 'SETS: 0; MEMBERS: ';
	is( $did, $should, "on init trn1.as_string() returns '$did'" );

	$trn1 = Locale::KeyedText.new_translator( 'zZ9', [] );
	isa_ok( $trn1, "Locale::KeyedText::Translator", 
		"trn1 = new_translator( 'zZ9', [] ) ret TRN obj" );
	$did = $trn1.as_string();
	$should = 'SETS: zZ9; MEMBERS: ';
	is( $did, $should, "on init trn1.as_string() returns '$did'" );

	$trn1 = Locale::KeyedText.new_translator( ['zZ9'], [] );
	isa_ok( $trn1, "Locale::KeyedText::Translator", 
		"trn1 = new_translator( ['zZ9'], [] ) ret TRN obj" );
	$did = $trn1.as_string();
	$should = 'SETS: zZ9; MEMBERS: ';
	is( $did, $should, "on init trn1.as_string() returns '$did'" );

	$trn1 = Locale::KeyedText.new_translator( ['zZ9','aaa'], [] );
	isa_ok( $trn1, "Locale::KeyedText::Translator", 
		"trn1 = new_translator( ['zZ9','aaa'], [] ) ret TRN obj" );
	$did = $trn1.as_string();
	$should = 'SETS: zZ9, aaa; MEMBERS: ';
	is( $did, $should, "on init trn1.as_string() returns '$did'" );

	$did = serialize( Locale::KeyedText.new_translator( [], '' ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_translator( [], '' ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_translator( [], '0 ' ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_translator( [], '0 ' ) returns '$did'" );

	$did = serialize( Locale::KeyedText.new_translator( [], 'x-' ) );
	$should = 'undef, ';
	is( $did, $should, "Locale::KeyedText.new_translator( [], 'x-' ) returns '$did'" );

	$trn1 = Locale::KeyedText.new_translator( [], '0' );
	isa_ok( $trn1, "Locale::KeyedText::Translator", 
		"trn1 = new_translator( [], '0' ) ret TRN obj" );
	$did = $trn1.as_string();
	$should = 'SETS: ; MEMBERS: 0';
	is( $did, $should, "on init trn1.as_string() returns '$did'" );

	$trn1 = Locale::KeyedText.new_translator( [], 'zZ9' );
	isa_ok( $trn1, "Locale::KeyedText::Translator", 
		"trn1 = new_translator( [], 'zZ9' ) ret TRN obj" );
	$did = $trn1.as_string();
	$should = 'SETS: ; MEMBERS: zZ9';
	is( $did, $should, "on init trn1.as_string() returns '$did'" );

	$trn1 = Locale::KeyedText.new_translator( [], ['zZ9'] );
	isa_ok( $trn1, "Locale::KeyedText::Translator", 
		"trn1 = new_translator( [], ['zZ9'] ) ret TRN obj" );
	$did = $trn1.as_string();
	$should = 'SETS: ; MEMBERS: zZ9';
	is( $did, $should, "on init trn1.as_string() returns '$did'" );

	$trn1 = Locale::KeyedText.new_translator( [], ['zZ9','aaa'] );
	isa_ok( $trn1, "Locale::KeyedText::Translator", 
		"trn1 = new_translator( [], ['zZ9','aaa'] ) ret TRN obj" );
	$did = $trn1.as_string();
	$should = 'SETS: ; MEMBERS: zZ9, aaa';
	is( $did, $should, "on init trn1.as_string() returns '$did'" );

	$trn1 = Locale::KeyedText.new_translator( ['goo','har'], ['wer','thr'] );
	isa_ok( $trn1, "Locale::KeyedText::Translator", 
		"trn1 = new_translator( ['goo','har'], ['wer','thr'] ) ret TRN obj" );
	$did = $trn1.as_string();
	$should = 'SETS: goo, har; MEMBERS: wer, thr';
	is( $did, $should, "on init trn1.as_string() returns '$did'" );

	$did = serialize( $trn1.get_template_set_names() );
	$should = '[ \'goo\', \'har\', ], ';
	is( $did, $should, "on init trn1.get_template_set_names() returns '$did'" );

	$did = serialize( $trn1.get_template_member_names() );
	$should = '[ \'wer\', \'thr\', ], ';
	is( $did, $should, "on init trn1.get_template_member_names() returns '$did'" );

	$trn1 = Locale::KeyedText.new_translator( ['go::o','::har'], ['w::er','thr::'] );
	isa_ok( $trn1, "Locale::KeyedText::Translator", 
		"trn1 = new_translator( ['go::o','::har'], ['w::er','thr::'] ) ret TRN obj" );
	$did = $trn1.as_string();
	$should = 'SETS: go::o, ::har; MEMBERS: w::er, thr::';
	is( $did, $should, "on init trn1.as_string() returns '$did'" );
}

######################################################################

{
	message( 'testing Translator.translate_message() method' );

	my $AS = 't_Locale_KeyedText_A_L_';
	my $BS = 't_Locale_KeyedText_B_L_';
	my $CS = 't_Locale_KeyedText_C_L_';

	my ($did, $should, $msg1, $msg2, $msg3, $trn1, $trn2, $trn3, $trn4, $trn11);

	# First test that anything does or doesn't work, and test variable substitution.

	$msg1 = Locale::KeyedText.new_message( 'one' );
	ok( 1, "msg1 = new_message( 'one' ) contains '"~$msg1.as_string()~"'" );

	$msg2 = Locale::KeyedText.new_message( 'one', {'spoon'=>'lift','fork'=>'0'} );
	ok( 1, "msg2 = new_message( 'one', {'spoon'=>'lift','fork'=>'0'} ) contains '"~$msg2.as_string()~"'" );

	$msg3 = Locale::KeyedText.new_message( 'one', {'spoon'=> undef,'fork'=>''} );
	ok( 1, "msg3 = new_message( 'one', {'spoon'=> undef,'fork'=>''} ) contains '"~$msg3.as_string()~"'" );

	$trn1 = Locale::KeyedText.new_translator( [$AS],['Eng'] );
	ok( 1, "trn1 = new_translator( [$AS],['Eng'] ) contains '"~$trn1.as_string()~"'" );

	$trn2 = Locale::KeyedText.new_translator( [$BS],['Eng'] );
	ok( 1, "trn2 = new_translator( [$BS],['Eng'] ) contains '"~$trn2.as_string()~"'" );

	$did = serialize( $trn1.translate_message() );
	$should = 'undef, ';
	is( $did, $should, "trn1.translate_message() returns '$did'" );

	$did = serialize( $trn1.translate_message( 'foo' ) );
	$should = 'undef, ';
	is( $did, $should, "trn1.translate_message( 'foo' ) returns '$did'" );

	$did = serialize( $trn1.translate_message( 'Locale::KeyedText::Message' ) );
	$should = 'undef, ';
	is( $did, $should, "trn1.translate_message( 'Locale::KeyedText::Message' ) returns '$did'" );

	$did = $trn1.translate_message( $msg1 );
	$should = 'AE - word {fork} { fork } {spoon} {{fork}}';
	is( $did, $should, "trn1.translate_message( msg1 ) returns '$did'" );

	$did = $trn1.translate_message( $msg2 );
	$should = 'AE - word 0 { fork } lift {0}';
	is( $did, $should, "trn1.translate_message( msg2 ) returns '$did'" );

	$did = $trn1.translate_message( $msg3 );
	$should = 'AE - word  { fork }  {}';
	is( $did, $should, "trn1.translate_message( msg3 ) returns '$did'" );

	$did = serialize( $trn2.translate_message( $msg2 ) );
	$should = 'undef, ';
	is( $did, $should, "trn2.translate_message( msg2 ) returns '$did'" );

	# Next test multiple module searching.

	$msg1 = Locale::KeyedText.new_message( 'one', {'spoon'=>'lift','fork'=>'poke'} );
	ok( 1, "msg1 = new_message( 'one', {'spoon'=>'lift','fork'=>'poke'} ) contains '"~$msg1.as_string()~"'" );

	$msg2 = Locale::KeyedText.new_message( 'two' );
	ok( 1, "msg2 = new_message( 'two' ) contains '"~$msg2.as_string()~"'" );

	$msg3 = Locale::KeyedText.new_message( 'three', { 'knife'=>'sharp' } );
	ok( 1, "msg3 = new_message( 'three', { 'knife'=>'sharp' } ) contains '"~$msg3.as_string()~"'" );

	$trn1 = Locale::KeyedText.new_translator( [$AS,$BS],['Eng','Fre'] );
	ok( 1, "trn1 = new_translator( [$AS],['Eng'] ) contains '"~$trn1.as_string()~"'" );

	$trn2 = Locale::KeyedText.new_translator( [$AS,$BS],['Fre','Eng'] );
	ok( 1, "trn2 = new_translator( [$AS],['Eng'] ) contains '"~$trn2.as_string()~"'" );

	$trn3 = Locale::KeyedText.new_translator( [$BS,$AS],['Eng','Fre'] );
	ok( 1, "trn3 = new_translator( [$AS],['Eng'] ) contains '"~$trn3.as_string()~"'" );

	$trn4 = Locale::KeyedText.new_translator( [$BS,$AS],['Fre','Eng'] );
	ok( 1, "trn4 = new_translator( [$AS],['Eng'] ) contains '"~$trn4.as_string()~"'" );

	$did = serialize( $trn1.translate_message( $msg1 ) );
	$should = '\'AE - word poke { fork } lift {poke}\', ';
	is( $did, $should, "trn1.translate_message( msg1 ) returns '$did'" );

	$did = serialize( $trn1.translate_message( $msg2 ) );
	$should = '\'AE - sky pie rye\', ';
	is( $did, $should, "trn1.translate_message( msg2 ) returns '$did'" );

	$did = serialize( $trn1.translate_message( $msg3 ) );
	$should = '\'BE - eat sharp\', ';
	is( $did, $should, "trn1.translate_message( msg3 ) returns '$did'" );

	$did = serialize( $trn2.translate_message( $msg1 ) );
	$should = '\'AF - word poke { fork } lift {poke}\', ';
	is( $did, $should, "trn2.translate_message( msg1 ) returns '$did'" );

	$did = serialize( $trn2.translate_message( $msg2 ) );
	$should = '\'AF - sky pie rye\', ';
	is( $did, $should, "trn2.translate_message( msg2 ) returns '$did'" );

	$did = serialize( $trn2.translate_message( $msg3 ) );
	$should = '\'BF - eat sharp\', ';
	is( $did, $should, "trn2.translate_message( msg3 ) returns '$did'" );

	$did = serialize( $trn3.translate_message( $msg1 ) );
	$should = '\'AE - word poke { fork } lift {poke}\', ';
	is( $did, $should, "trn3.translate_message( msg1 ) returns '$did'" );

	$did = serialize( $trn3.translate_message( $msg2 ) );
	$should = '\'BE - sky pie rye\', ';
	is( $did, $should, "trn3.translate_message( msg2 ) returns '$did'" );

	$did = serialize( $trn3.translate_message( $msg3 ) );
	$should = '\'BE - eat sharp\', ';
	is( $did, $should, "trn3.translate_message( msg3 ) returns '$did'" );

	$did = serialize( $trn4.translate_message( $msg1 ) );
	$should = '\'AF - word poke { fork } lift {poke}\', ';
	is( $did, $should, "trn4.translate_message( msg1 ) returns '$did'" );

	$did = serialize( $trn4.translate_message( $msg2 ) );
	$should = '\'BF - sky pie rye\', ';
	is( $did, $should, "trn4.translate_message( msg2 ) returns '$did'" );

	$did = serialize( $trn4.translate_message( $msg3 ) );
	$should = '\'BF - eat sharp\', ';
	is( $did, $should, "trn4.translate_message( msg3 ) returns '$did'" );

	$trn11 = Locale::KeyedText.new_translator( [$CS],['Eng'] );
	ok( 1, "trn11 = new_translator( [$CS],['Eng'] ) contains '"~$trn11.as_string()~"'" );

	$did = serialize( $trn11.translate_message( $msg1 ) );
	$should = '\'poke shore lift\', ';
	is( $did, $should, "trn11.translate_message( msg1 ) returns '$did'" );

	$did = serialize( $trn11.translate_message( $msg2 ) );
	$should = '\'sky fly high\', ';
	is( $did, $should, "trn11.translate_message( msg2 ) returns '$did'" );

	$did = serialize( $trn11.translate_message( $msg3 ) );
	$should = '\'sharp zot\', ';
	is( $did, $should, "trn11.translate_message( msg3 ) returns '$did'" );
}

######################################################################

{
	message( 'confirming availability of all test Template modules' );

	# done after all translate_message() calls as that method should be 
	# 'requiring' these itself, and we don't want to "interfere" with that.

	try {
		require t_Locale_KeyedText_A_L_Eng;
		t_Locale_KeyedText_A_L_Eng.get_text_by_key( 'foo' );
		ok( 1, "load and invoke t_Locale_KeyedText_A_L_Eng" );
		CATCH {
			ok( 0, "load and invoke t_Locale_KeyedText_A_L_Eng; \$\! contains '$!'" );
		}
	}

	try {
		require t_Locale_KeyedText_A_L_Fre;
		t_Locale_KeyedText_A_L_Fre.get_text_by_key( 'foo' );
		ok( 1, "load and invoke t_Locale_KeyedText_A_L_Fre" );
		CATCH {
			ok( 0, "load and invoke t_Locale_KeyedText_A_L_Fre; \$\! contains '$!'" );
		}
	}

	try {
		require t_Locale_KeyedText_B_L_Eng;
		t_Locale_KeyedText_B_L_Eng.get_text_by_key( 'foo' );
		ok( 1, "load and invoke t_Locale_KeyedText_B_L_Eng" );
		CATCH {
			ok( 0, "load and invoke t_Locale_KeyedText_B_L_Eng; \$\! contains '$!'" );
		}
	}

	try {
		require t_Locale_KeyedText_B_L_Fre;
		t_Locale_KeyedText_B_L_Fre.get_text_by_key( 'foo' );
		ok( 1, "load and invoke t_Locale_KeyedText_B_L_Fre" );
		CATCH {
			ok( 0, "load and invoke t_Locale_KeyedText_B_L_Fre; \$\! contains '$!'" );
		}
	}
}

######################################################################

message( 'DONE TESTING Locale::KeyedText' );

######################################################################
######################################################################

module t_Locale_KeyedText_C_L_Eng {
	my Str %text_strings ::= (
		'one' => '{fork} shore {spoon}',
		'two' => 'sky fly high',
		'three' => '{knife} zot',
	);
	sub get_text_by_key( Str $msg_key ) returns Str {
		return %text_strings{$msg_key};
	}
}

######################################################################

1;
