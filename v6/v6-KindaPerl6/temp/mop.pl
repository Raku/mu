use lib 'temp';
#use KindaPerl6::Perl5::MOP;
use MOP;
use Data::Dumper;

my $meth = ::CALL( $::Method, 'new', sub { 'hi' } );
print 'Method: ',Dumper($meth);
print 'Method.HOW: ',Dumper( ::CALL( $meth, 'HOW') );
print 'Method.str: ',Dumper( ::CALL( $::Method, 'str') );

__END__

KindaPerl6::Class->new;

print "Class = ", Dumper( $Class_KindaPerl6::Class );

my $v = $Class_Type_Constant->new( 123 );
print "v = ", Dumper($v);
