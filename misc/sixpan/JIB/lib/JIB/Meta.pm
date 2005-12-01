package JIB::Meta;

use strict;
use warnings;

use YAML                    qw[LoadFile];
use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];

use base 'Object::Accessor';

=head1 METHODS

=head2 $meta = JIB::Meta->new( file => /path/to/meta.info )

=cut

sub new { 
    my $class   = shift;
    my %hash    = @_;
    
    my $file;
    my $tmpl = {
        file => { required => 1, allow => sub { -e shift() }, store => \$file },
    };
    
    check( $tmpl, \%hash ) or ( error( Params::Check->last_error), return );

    my $struct = eval { LoadFile( $file ) }  or error( $@ ), return; 
        
    ### XXX check validity of the struct
    
    my $obj = $class->SUPER::new;

    ### XXX add a fields accessors for all the fields in the file
    ### so we can have other accessors for other meta data?
    $obj->mk_accessors( keys %$struct );
    $obj->$_( $struct->{$_} ) for keys %$struct;
    
    return $obj;
}

1;
