use lib 'temp';
#use KindaPerl6::Perl5::MOP;
use MOP;
use Data::Dumper;

my $meth = ::CALL( $::Method, 'new', sub { 'hi' } );
print 'Method: ',Dumper($meth);
print 'Method.WHAT: ',Dumper( ::CALL( $meth, 'WHAT') );
print '$::Method.str: ',Dumper( ::CALL( $::Method, 'str') );
print 'Method.HOW: ',Dumper( ::CALL( $meth, 'HOW') );

print "---\n";
#print "Value: ",Dumper($::Value);
print 'Value: ',Dumper( ::CALL( $::Value, 'new', 'abc' ) );

my $str = ::CALL( $::Str, 'new', 'abc' );
print 'Str: ',Dumper( $str );
my $str2 = ::CALL( $str, 'FETCH' );
print $str2->{_value},"\n";

my $var = ::CALL( $::Scalar, 'new' );
my $value = ::CALL( $var, 'FETCH' );
print ::CALL( $value, 'perl' )->{_value},"\n";
