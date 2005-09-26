#!pugs
use v6;

use Test;

plan( 34 );

use lib <t/lib ext/Locale-KeyedText/t/lib>;
use t_LKT_Util;
use Locale::KeyedText;

t_LKT_Util::message( 'testing Translator.translate_message() method' );

module t_LKT_C_L_Eng {

my Str %text_strings is readonly = (
    'one' => q[{fork} shore {spoon}],
    'two' => q[sky fly high],
    'three' => q[{knife} zot],
);

sub get_text_by_key (Str $msg_key) returns Str {
    return %text_strings{$msg_key};
}

} # end module t_LKT_C_L_Eng

my $AS = 't_LKT_A_L_';
my $BS = 't_LKT_B_L_';
my $CS = 't_LKT_C_L_';

my ($did, $should, $msg1, $msg2, $msg3, $trn1, $trn2, $trn3, $trn4, $trn11);

# First test that anything does or doesn't work, and test variable substitution.

$msg1 = Locale::KeyedText.new_message( 'one' );
pass( q|msg1 = new_message( 'one' ) contains '|~$msg1.as_string()~q|'| );

$msg2 = Locale::KeyedText.new_message( 'one', {'spoon'=>'lift','fork'=>'0'} );
pass( q|msg2 = new_message( 'one', {'spoon'=>'lift','fork'=>'0'} ) contains '|~$msg2.as_string()~q|'| );

$msg3 = Locale::KeyedText.new_message( 'one', {'spoon'=> undef,'fork'=>q{}} );
pass( q|msg3 = new_message( 'one', {'spoon'=> undef,'fork'=>q{}} ) contains '|~$msg3.as_string()~q|'| );

$trn1 = Locale::KeyedText.new_translator( [$AS],['Eng'] );
pass( "trn1 = new_translator( [$AS],['Eng'] ) contains '"~$trn1.as_string()~q|'| );

$trn2 = Locale::KeyedText.new_translator( [$BS],['Eng'] );
pass( "trn2 = new_translator( [$BS],['Eng'] ) contains '"~$trn2.as_string()~q|'| );

$did = t_LKT_Util::serialize( $trn1.translate_message( 'foo' ) );
$should = 'undef, ';
is( $did, $should, "trn1.translate_message( 'foo' ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn1.translate_message( 'Locale::KeyedText::Message' ) );
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

$did = t_LKT_Util::serialize( $trn2.translate_message( $msg2 ) );
$should = 'undef, ';
is( $did, $should, "trn2.translate_message( msg2 ) returns '$did'" );

# Next test multiple module searching.

$msg1 = Locale::KeyedText.new_message( 'one', {'spoon'=>'lift','fork'=>'poke'} );
pass( q|msg1 = new_message( 'one', {'spoon'=>'lift','fork'=>'poke'} ) contains '|~$msg1.as_string()~q|'| );

$msg2 = Locale::KeyedText.new_message( 'two' );
pass( q|msg2 = new_message( 'two' ) contains '|~$msg2.as_string()~q|'| );

$msg3 = Locale::KeyedText.new_message( 'three', { 'knife'=>'sharp' } );
pass( q|msg3 = new_message( 'three', { 'knife'=>'sharp' } ) contains '|~$msg3.as_string()~q|'| );

$trn1 = Locale::KeyedText.new_translator( [$AS,$BS],['Eng','Fre'] );
pass( "trn1 = new_translator( [$AS],['Eng'] ) contains '"~$trn1.as_string()~q|'| );

$trn2 = Locale::KeyedText.new_translator( [$AS,$BS],['Fre','Eng'] );
pass( "trn2 = new_translator( [$AS],['Eng'] ) contains '"~$trn2.as_string()~q|'| );

$trn3 = Locale::KeyedText.new_translator( [$BS,$AS],['Eng','Fre'] );
pass( "trn3 = new_translator( [$AS],['Eng'] ) contains '"~$trn3.as_string()~q|'| );

$trn4 = Locale::KeyedText.new_translator( [$BS,$AS],['Fre','Eng'] );
pass( "trn4 = new_translator( [$AS],['Eng'] ) contains '"~$trn4.as_string()~q|'| );

$did = t_LKT_Util::serialize( $trn1.translate_message( $msg1 ) );
$should = q|'AE - word poke { fork } lift {poke}', |;
is( $did, $should, "trn1.translate_message( msg1 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn1.translate_message( $msg2 ) );
$should = q|'AE - sky pie rye', |;
is( $did, $should, "trn1.translate_message( msg2 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn1.translate_message( $msg3 ) );
$should = q|'BE - eat sharp', |;
is( $did, $should, "trn1.translate_message( msg3 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn2.translate_message( $msg1 ) );
$should = q|'AF - word poke { fork } lift {poke}', |;
is( $did, $should, "trn2.translate_message( msg1 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn2.translate_message( $msg2 ) );
$should = q|'AF - sky pie rye', |;
is( $did, $should, "trn2.translate_message( msg2 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn2.translate_message( $msg3 ) );
$should = q|'BF - eat sharp', |;
is( $did, $should, "trn2.translate_message( msg3 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn3.translate_message( $msg1 ) );
$should = q|'AE - word poke { fork } lift {poke}', |;
is( $did, $should, "trn3.translate_message( msg1 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn3.translate_message( $msg2 ) );
$should = q|'BE - sky pie rye', |;
is( $did, $should, "trn3.translate_message( msg2 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn3.translate_message( $msg3 ) );
$should = q|'BE - eat sharp', |;
is( $did, $should, "trn3.translate_message( msg3 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn4.translate_message( $msg1 ) );
$should = q|'AF - word poke { fork } lift {poke}', |;
is( $did, $should, "trn4.translate_message( msg1 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn4.translate_message( $msg2 ) );
$should = q|'BF - sky pie rye', |;
is( $did, $should, "trn4.translate_message( msg2 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn4.translate_message( $msg3 ) );
$should = q|'BF - eat sharp', |;
is( $did, $should, "trn4.translate_message( msg3 ) returns '$did'" );

$trn11 = Locale::KeyedText.new_translator( [$CS],['Eng'] );
pass( "trn11 = new_translator( [$CS],['Eng'] ) contains '"~$trn11.as_string()~q|'| );

$did = t_LKT_Util::serialize( $trn11.translate_message( $msg1 ) );
$should = q|'poke shore lift', |;
is( $did, $should, "trn11.translate_message( msg1 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn11.translate_message( $msg2 ) );
$should = q|'sky fly high', |;
is( $did, $should, "trn11.translate_message( msg2 ) returns '$did'" );

$did = t_LKT_Util::serialize( $trn11.translate_message( $msg3 ) );
$should = q|'sharp zot', |;
is( $did, $should, "trn11.translate_message( msg3 ) returns '$did'" );
