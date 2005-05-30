#!pugs
use v6;

use Test;

plan( 34 );
#skip_rest "skipping tests"; # for release
#exit;

use lib <t/lib ext/Locale-KeyedText/t/lib>;
use t_LKT_Util;
use Locale::KeyedText;

t_LKT_Util::message( 'testing new_translator() and most Translator object methods' );

my ($did, $should, $trn1);

#$did = t_LKT_Util::serialize( Locale::KeyedText.new_translator() );
#$should = 'undef, ';
#is( $did, $should, "Locale::KeyedText.new_translator() returns '$did'" );

$did = t_LKT_Util::serialize( Locale::KeyedText.new_translator( undef, undef ) );
$should = 'undef, ';
is( $did, $should, "Locale::KeyedText.new_translator( undef, undef ) returns '$did'" );

$did = t_LKT_Util::serialize( Locale::KeyedText.new_translator( [], undef ) );
$should = 'undef, ';
is( $did, $should, "Locale::KeyedText.new_translator( [], undef ) returns '$did'" );

$did = t_LKT_Util::serialize( Locale::KeyedText.new_translator( undef, [] ) );
$should = 'undef, ';
is( $did, $should, "Locale::KeyedText.new_translator( undef, [] ) returns '$did'" );

$trn1 = Locale::KeyedText.new_translator( [], [] );
isa_ok( $trn1, "Locale::KeyedText::Translator", 
	"trn1 = new_translator( [], [] ) ret TRN obj" );
$did = $trn1.as_string();
$should = 'SETS: ; MEMBERS: ';
is( $did, $should, "on init trn1.as_string() returns '$did'" );

$did = t_LKT_Util::serialize( Locale::KeyedText.new_translator( '', [] ) );
$should = 'undef, ';
is( $did, $should, "Locale::KeyedText.new_translator( '', [] ) returns '$did'" );

$did = t_LKT_Util::serialize( Locale::KeyedText.new_translator( '0 ', [] ) );
$should = 'undef, ';
is( $did, $should, "Locale::KeyedText.new_translator( '0 ', [] ) returns '$did'" );

$did = t_LKT_Util::serialize( Locale::KeyedText.new_translator( 'x-', [] ) );
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

$did = t_LKT_Util::serialize( Locale::KeyedText.new_translator( [], '' ) );
$should = 'undef, ';
is( $did, $should, "Locale::KeyedText.new_translator( [], '' ) returns '$did'" );

$did = t_LKT_Util::serialize( Locale::KeyedText.new_translator( [], '0 ' ) );
$should = 'undef, ';
is( $did, $should, "Locale::KeyedText.new_translator( [], '0 ' ) returns '$did'" );

$did = t_LKT_Util::serialize( Locale::KeyedText.new_translator( [], 'x-' ) );
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

$did = t_LKT_Util::serialize( $trn1.get_template_set_names() );
$should = '[ \'goo\', \'har\', ], ';
is( $did, $should, "on init trn1.get_template_set_names() returns '$did'" );

$did = t_LKT_Util::serialize( $trn1.get_template_member_names() );
$should = '[ \'wer\', \'thr\', ], ';
is( $did, $should, "on init trn1.get_template_member_names() returns '$did'" );

$trn1 = Locale::KeyedText.new_translator( ['go::o','::har'], ['w::er','thr::'] );
isa_ok( $trn1, "Locale::KeyedText::Translator", 
	"trn1 = new_translator( ['go::o','::har'], ['w::er','thr::'] ) ret TRN obj" );
$did = $trn1.as_string();
$should = 'SETS: go::o, ::har; MEMBERS: w::er, thr::';
is( $did, $should, "on init trn1.as_string() returns '$did'" );
