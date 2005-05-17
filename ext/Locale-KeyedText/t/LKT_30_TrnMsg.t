#!pugs
use v6;

use Test;

plan( 35 );

use lib; 
BEGIN { import( 'lib': 't/lib' ); }
use t_LKT_Util;
use Locale::KeyedText;

t_LKT_Util.message( 'testing Translator.translate_message() method' );

my $AS = 't_LKT_A_L_';
my $BS = 't_LKT_B_L_';
my $CS = 't_LKT_C_L_';

my ($did, $should, $msg1, $msg2, $msg3, $trn1, $trn2, $trn3, $trn4, $trn11);

# First test that anything does or doesn't work, and test variable substitution.

$msg1 = Locale::KeyedText.new_message( 'one' );
pass( "msg1 = new_message( 'one' ) contains '"~$msg1.as_string()~"'" );

$msg2 = Locale::KeyedText.new_message( 'one', {'spoon'=>'lift','fork'=>'0'} );
pass( "msg2 = new_message( 'one', {'spoon'=>'lift','fork'=>'0'} ) contains '"~$msg2.as_string()~"'" );

$msg3 = Locale::KeyedText.new_message( 'one', {'spoon'=> undef,'fork'=>''} );
pass( "msg3 = new_message( 'one', {'spoon'=> undef,'fork'=>''} ) contains '"~$msg3.as_string()~"'" );

$trn1 = Locale::KeyedText.new_translator( [$AS],['Eng'] );
pass( "trn1 = new_translator( [$AS],['Eng'] ) contains '"~$trn1.as_string()~"'" );

$trn2 = Locale::KeyedText.new_translator( [$BS],['Eng'] );
pass( "trn2 = new_translator( [$BS],['Eng'] ) contains '"~$trn2.as_string()~"'" );

$did = t_LKT_Util.serialize( $trn1.translate_message() );
$should = 'undef, ';
is( $did, $should, "trn1.translate_message() returns '$did'" );

$did = t_LKT_Util.serialize( $trn1.translate_message( 'foo' ) );
$should = 'undef, ';
is( $did, $should, "trn1.translate_message( 'foo' ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn1.translate_message( 'Locale::KeyedText::Message' ) );
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

$did = t_LKT_Util.serialize( $trn2.translate_message( $msg2 ) );
$should = 'undef, ';
is( $did, $should, "trn2.translate_message( msg2 ) returns '$did'" );

# Next test multiple module searching.

$msg1 = Locale::KeyedText.new_message( 'one', {'spoon'=>'lift','fork'=>'poke'} );
pass( "msg1 = new_message( 'one', {'spoon'=>'lift','fork'=>'poke'} ) contains '"~$msg1.as_string()~"'" );

$msg2 = Locale::KeyedText.new_message( 'two' );
pass( "msg2 = new_message( 'two' ) contains '"~$msg2.as_string()~"'" );

$msg3 = Locale::KeyedText.new_message( 'three', { 'knife'=>'sharp' } );
pass( "msg3 = new_message( 'three', { 'knife'=>'sharp' } ) contains '"~$msg3.as_string()~"'" );

$trn1 = Locale::KeyedText.new_translator( [$AS,$BS],['Eng','Fre'] );
pass( "trn1 = new_translator( [$AS],['Eng'] ) contains '"~$trn1.as_string()~"'" );

$trn2 = Locale::KeyedText.new_translator( [$AS,$BS],['Fre','Eng'] );
pass( "trn2 = new_translator( [$AS],['Eng'] ) contains '"~$trn2.as_string()~"'" );

$trn3 = Locale::KeyedText.new_translator( [$BS,$AS],['Eng','Fre'] );
pass( "trn3 = new_translator( [$AS],['Eng'] ) contains '"~$trn3.as_string()~"'" );

$trn4 = Locale::KeyedText.new_translator( [$BS,$AS],['Fre','Eng'] );
pass( "trn4 = new_translator( [$AS],['Eng'] ) contains '"~$trn4.as_string()~"'" );

$did = t_LKT_Util.serialize( $trn1.translate_message( $msg1 ) );
$should = '\'AE - word poke { fork } lift {poke}\', ';
is( $did, $should, "trn1.translate_message( msg1 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn1.translate_message( $msg2 ) );
$should = '\'AE - sky pie rye\', ';
is( $did, $should, "trn1.translate_message( msg2 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn1.translate_message( $msg3 ) );
$should = '\'BE - eat sharp\', ';
is( $did, $should, "trn1.translate_message( msg3 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn2.translate_message( $msg1 ) );
$should = '\'AF - word poke { fork } lift {poke}\', ';
is( $did, $should, "trn2.translate_message( msg1 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn2.translate_message( $msg2 ) );
$should = '\'AF - sky pie rye\', ';
is( $did, $should, "trn2.translate_message( msg2 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn2.translate_message( $msg3 ) );
$should = '\'BF - eat sharp\', ';
is( $did, $should, "trn2.translate_message( msg3 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn3.translate_message( $msg1 ) );
$should = '\'AE - word poke { fork } lift {poke}\', ';
is( $did, $should, "trn3.translate_message( msg1 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn3.translate_message( $msg2 ) );
$should = '\'BE - sky pie rye\', ';
is( $did, $should, "trn3.translate_message( msg2 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn3.translate_message( $msg3 ) );
$should = '\'BE - eat sharp\', ';
is( $did, $should, "trn3.translate_message( msg3 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn4.translate_message( $msg1 ) );
$should = '\'AF - word poke { fork } lift {poke}\', ';
is( $did, $should, "trn4.translate_message( msg1 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn4.translate_message( $msg2 ) );
$should = '\'BF - sky pie rye\', ';
is( $did, $should, "trn4.translate_message( msg2 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn4.translate_message( $msg3 ) );
$should = '\'BF - eat sharp\', ';
is( $did, $should, "trn4.translate_message( msg3 ) returns '$did'" );

$trn11 = Locale::KeyedText.new_translator( [$CS],['Eng'] );
pass( "trn11 = new_translator( [$CS],['Eng'] ) contains '"~$trn11.as_string()~"'" );

$did = t_LKT_Util.serialize( $trn11.translate_message( $msg1 ) );
$should = '\'poke shore lift\', ';
is( $did, $should, "trn11.translate_message( msg1 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn11.translate_message( $msg2 ) );
$should = '\'sky fly high\', ';
is( $did, $should, "trn11.translate_message( msg2 ) returns '$did'" );

$did = t_LKT_Util.serialize( $trn11.translate_message( $msg3 ) );
$should = '\'sharp zot\', ';
is( $did, $should, "trn11.translate_message( msg3 ) returns '$did'" );

module t_LKT_C_L_Eng;

my Str %text_strings = ( # is constant
	'one' => '{fork} shore {spoon}',
	'two' => 'sky fly high',
	'three' => '{knife} zot',
);

sub get_text_by_key( Str $msg_key ) returns Str {
	return %text_strings{$msg_key};
}
