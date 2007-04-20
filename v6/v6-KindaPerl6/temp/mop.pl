use lib 'temp';
#use KindaPerl6::Perl5::MOP;
use MOP;
use Data::Dumper;

my $meth = ::CALL( $::Method, 'new', sub { 'hi' } );
print 'Method: ',Dumper($meth);
print 'Method.WHAT: ',Dumper( ::CALL( $meth, 'WHAT') );
print '$::Method.str: ',Dumper( ::CALL( $::Method, 'str') );


print 'Method.HOW: ',Dumper( ::CALL( $meth, 'HOW') );
