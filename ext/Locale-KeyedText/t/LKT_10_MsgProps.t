#!pugs
use v6;

use Test;

plan( 24 );

use lib <t/lib ext/Locale-KeyedText/t/lib>;
use t_LKT_Util;
use Locale::KeyedText;

t_LKT_Util::message( 'testing new_message() and Message object methods' );

my ($did, $should, $msg1);

$did = t_LKT_Util::serialize( Locale::KeyedText.new_message( undef ) );
$should = 'undef, ';
is( $did, $should, "Locale::KeyedText.new_message( undef ) returns '$did'" );

$msg1 = Locale::KeyedText.new_message( q{} );
isa_ok( $msg1, 'Locale::KeyedText::Message',
    q|msg1 = new_message( q{} ) ret MSG obj| );
$did = $msg1.as_string();
$should = ': ';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$msg1 = Locale::KeyedText.new_message( '0' );
isa_ok( $msg1, 'Locale::KeyedText::Message',
    q|msg1 = new_message( '0' ) ret MSG obj| );
$did = $msg1.as_string();
$should = '0: ';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$msg1 = Locale::KeyedText.new_message( 'zZ9' );
isa_ok( $msg1, 'Locale::KeyedText::Message',
    q|msg1 = new_message( 'zZ9' ) ret MSG obj| );
$did = $msg1.as_string();
$should = 'zZ9: ';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$msg1 = Locale::KeyedText.new_message( 'foo', undef );
isa_ok( $msg1, 'Locale::KeyedText::Message',
    q|msg1 = new_message( 'foo', undef ) ret MSG obj| );
$did = $msg1.as_string();
$should = 'foo: ';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$msg1 = Locale::KeyedText.new_message( 'foo', hash() );
isa_ok( $msg1, 'Locale::KeyedText::Message',
    q|msg1 = new_message( 'foo', hash() ) ret MSG obj| );
$did = $msg1.as_string();
$should = 'foo: ';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$msg1 = Locale::KeyedText.new_message( 'foo', { q{} => 'g' } );
isa_ok( $msg1, 'Locale::KeyedText::Message',
    q|msg1 = new_message( 'foo', \{ q{} => 'g' \} ) ret MSG obj| );
$did = $msg1.as_string();
$should = 'foo: =g';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$msg1 = Locale::KeyedText.new_message( 'foo', { 'bar' => 'baz' } );
isa_ok( $msg1, 'Locale::KeyedText::Message',
    q|msg1 = new_message( 'foo', \{ 'bar' => 'baz' \} ) ret MSG obj| );
$did = $msg1.as_string();
$should = 'foo: bar=baz';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$msg1 = Locale::KeyedText.new_message( 'foo', { 'bar'=>'baz','c'=>'-','0'=>'1','z'=>q{},'y'=>'0' } );
isa_ok( $msg1, 'Locale::KeyedText::Message',
    q|msg1 = new_message( 'foo', \{ 'bar'=>'baz','c'=>'d','0'=>'1','z'=>q{},'y'=>'0' \} ) ret MSG obj| );
$did = $msg1.as_string();
$should = 'foo: 0=1, bar=baz, c=-, y=0, z=';
is( $did, $should, "on init msg1.as_string() returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_key() );
$should = q|'foo', |;
is( $did, $should, "on init msg1.get_message_key() returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_variable( undef ) );
$should = 'undef, ';
is( $did, $should, "on init msg1.get_message_variable( undef ) returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_variable( q{} ) );
$should = 'undef, ';
is( $did, $should, "on init msg1.get_message_variable( q{} ) returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_variable( '0' ) );
$should = q|'1', |;
is( $did, $should, "on init msg1.get_message_variable( '0' ) returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_variable( 'zzz' ) );
$should = 'undef, ';
is( $did, $should, "on init msg1.get_message_variable( 'zzz' ) returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_variable( 'bar' ) );
$should = q|'baz', |;
is( $did, $should, "on init msg1.get_message_variable( 'bar' ) returns '$did'" );

$did = t_LKT_Util::serialize( $msg1.get_message_variables() );
$should = q|{ '0' => '1', 'bar' => 'baz', 'c' => '-', 'y' => '0', 'z' => q{}, }, |;
is( $did, $should, "on init msg1.get_message_variables() returns '$did'" );
