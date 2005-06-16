#!pugs
use v6;

use Test;

plan( 24 );

use lib <t/lib ext/Locale-KeyedText/t/lib>;
use t_LKT_Util;
use Locale::KeyedText;

t_LKT_Util::message( 'testing new_message() and Message object methods' );

my ($did, $should, $msg1, $temp);

$did = t_LKT_Util::serialize( Locale::KeyedText.new_message( undef ) );
$should = 'undef, ';
is( $did, $should, "Locale::KeyedText.new_message( undef ) returns '$did'" );

$msg1 = Locale::KeyedText.new_message( '' );
isa_ok( $msg1, "Locale::KeyedText::Message", 
	"msg1 = new_message( '' ) ret MSG obj" );
$did = $msg1.as_string();
$should = ': ';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

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

$temp = undef;
$msg1 = Locale::KeyedText.new_message( 'foo', $temp );
isa_ok( $msg1, "Locale::KeyedText::Message", 
	"msg1 = new_message( 'foo', undef ) ret MSG obj" );
$did = $msg1.as_string();
$should = 'foo: ';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$temp = hash();
$msg1 = Locale::KeyedText.new_message( 'foo', $temp );
isa_ok( $msg1, "Locale::KeyedText::Message", 
	"msg1 = new_message( 'foo', hash() ) ret MSG obj" );
$did = $msg1.as_string();
$should = 'foo: ';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$temp = { '' => 'g' };
$msg1 = Locale::KeyedText.new_message( 'foo', $temp );
isa_ok( $msg1, "Locale::KeyedText::Message", 
	"msg1 = new_message( 'foo', \{ '' => 'g' \} ) ret MSG obj" );
$did = $msg1.as_string(); # Pugs bug: pairs() on single-pair hashes become 2-elem list
$should = 'foo: =g';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$temp = { 'bar' => 'baz' };
$msg1 = Locale::KeyedText.new_message( 'foo', $temp ); # $temp used so Hash not auto-changed to List
isa_ok( $msg1, "Locale::KeyedText::Message", 
	"msg1 = new_message( 'foo', \{ 'bar' => 'baz' \} ) ret MSG obj" );
$did = $msg1.as_string(); # Pugs bug: pairs() on single-pair hashes become 2-elem list
$should = 'foo: bar=baz';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$temp = { 'bar'=>'baz','c'=>'-','0'=>'1','z'=>'','y'=>'0' };
$msg1 = Locale::KeyedText.new_message( 'foo', $temp );
isa_ok( $msg1, "Locale::KeyedText::Message", 
	"msg1 = new_message( 'foo', \{ 'bar'=>'baz','c'=>'d','0'=>'1','z'=>'','y'=>'0' \} ) ret MSG obj" );
$did = $msg1.as_string();
$should = 'foo: 0=1, bar=baz, c=-, y=0, z=';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_key() );
$should = '\'foo\', ';
is( $did, $should, "on init msg1.get_message_key() returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_variable( undef ) );
$should = 'undef, ';
is( $did, $should, "on init msg1.get_message_variable( undef ) returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_variable( '' ) );
$should = 'undef, ';
is( $did, $should, "on init msg1.get_message_variable( '' ) returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_variable( '0' ) );
$should = '\'1\', ';
is( $did, $should, "on init msg1.get_message_variable( '0' ) returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_variable( 'zzz' ) );
$should = 'undef, ';
is( $did, $should, "on init msg1.get_message_variable( 'zzz' ) returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_variable( 'bar' ) );
$should = '\'baz\', ';
is( $did, $should, "on init msg1.get_message_variable( 'bar' ) returns '$did'" );

# Pugs bug: simply reading a hash key (in get_message_variable) will create it;
# that is what causes this test to fail as $did contains extra keys with null values.
$temp = $msg1.get_message_variables(); # $temp used so Hash not auto-changed to List
$did = t_LKT_Util::serialize( $temp );
$should = '{ \'0\' => \'1\', \'bar\' => \'baz\', \'c\' => \'-\', \'y\' => \'0\', \'z\' => \'\', }, ';
is( $did, $should, "on init msg1.get_message_variables() returns '$did'" );
