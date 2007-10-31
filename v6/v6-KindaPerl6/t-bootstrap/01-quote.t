
use lib "compiled/perl5-kp6-kp6/lib";
use strict;
use KindaPerl6::Runtime::Perl5::Runtime;
use KindaPerl6::Grammar::Quote;
use Test::More tests => 1;

$_ = ::DISPATCH( $::Scalar, "new" );
::DISPATCH_VAR( $_, 'STORE', ::DISPATCH( $::Str, 'new', '123' ) );

my $MATCH = ::DISPATCH( $::KindaPerl6::Grammar, 'quoted_any' );

# ::DISPATCH( $GLOBAL::Code_print, 'APPLY',
#            ::DISPATCH( $MATCH, 'perl', ) );

ok( $MATCH->true, "quoted_any matched" );
