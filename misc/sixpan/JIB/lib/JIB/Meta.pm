package JIB::Meta;

use strict;
use warnings;
use JIB::Constants;

use JIB::Constants;
use JIB::Dependency;

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
        file => { required => 1, allow => FILE_EXISTS, store => \$file },
    };
    
    check( $tmpl, \%hash ) or ( error( Params::Check->last_error), return );

    my $struct = eval { LoadFile( $file ) } or error( $@ ), return; 

    return $class->new_from_struct( struct => $struct );        
}

=head2 $meta = JIB::Meta->new_from_struct( struct => $struct );

=cut

sub new_from_struct {
    my $class   = shift;
    my %hash    = @_;
    
    my $struct;
    my $tmpl = {
        struct => { required => 1, store => \$struct },
    };
    
    check( $tmpl, \%hash ) or ( error( Params::Check->last_error), return );

    ### XXX check validity of the struct
    
    my $obj = $class->SUPER::new;

    ### XXX add a fields accessors for all the fields in the file
    ### so we can have other accessors for other meta data?
    $obj->mk_accessors( keys %$struct );
    $obj->$_( $struct->{$_} ) for keys %$struct;
    
    ### if it has a dependency list, transform it into an object
    if( $obj->can('depends') ) {
        $obj->depends( 
            JIB::Dependency->new_from_struct( struct => $obj->depends )
        ) or return;
    }
    
    return $obj;
}

=head2 $struct = $meta->to_struct;

=cut

sub to_struct {
    my $self = shift;

    my %struct = map { $_ => $self->$_ } $self->ls_accessors;

    return \%struct;
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
