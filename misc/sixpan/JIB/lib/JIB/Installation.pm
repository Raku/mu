package JIB::Installation;

use strict;
use warnings;

use JIB::Config;
use JIB::Utils;
use JIB::Meta;


use YAML                    qw[LoadFile DumpFile];
use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];
use Data::Dumper;

use base 'Object::Accessor';

=head1 ACCESSORS

=head1 METHODS

=head2 $inst = JIB::Installation->new;

=cut

### XXX do some smart caching here -- but can't just make it a singleton
### due to stacked installations
{   my $config  = JIB::Config->new;

    sub new { 
        my $class   = shift;
        my $obj     = $class->SUPER::new;
    
        ### XXX tidy up
        {   $obj->mk_accessors( qw[available registered_alternatives config] );
            
            my @avail = eval { 
                map { JIB::Meta->new_from_struct( struct => $_ ) }
                    LoadFile( $config->available ) 
            };
            $@ and error( "Could not load available file: $@" ), return;
            $obj->available( \@avail );
            
            ### XXX objectify
            my $href = eval { LoadFile( $config->registered_alternatives ) };
            $@ and error( "Could not load registered alts file: $@" ), return;
            $obj->registered_alternatives( $href );
        
            $obj->config( $config );
        }
  
        return $obj;
    }
}

=head2 .... = $inst->is_installed( package => $package )

=cut

sub is_installed {
    my $self = shift;
    my %hash = @_;
    
    my $pkg;
    my $tmpl = {
        package => { required => 1, store => \$pkg, 
                     allow => sub { UNIVERSAL::isa(shift(), 'JIB::Package') } },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;

    for my $meta ( @{ $self->available } ) {
        return 1 if $meta->package eq $pkg->package;
    }
    
    return;
}    

=head2 .... = $inst->register( package => $package )

=cut

sub register {
    my $self = shift;
    my %hash = @_;
    
    my $pkg;
    my $tmpl = {
        package => { required => 1, store => \$pkg, 
                     allow => sub { UNIVERSAL::isa(shift(), 'JIB::Package') } },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;

    push @{ $self->available }, $pkg->meta;

    return $self->write;

}

=head2 .... = $inst->write;

=cut

sub write {
    my $self = shift;
    my $conf = $self->config;
    
    ### XXX use tempfiles

    my @avail = map { $_->to_struct } @{ $self->available };
    eval { DumpFile( $conf->available, @avail ) };
    $@ and error( "Could not write available file: $@" ), return;
    
    eval { DumpFile( $conf->registered_alternatives,
                     $self->registered_alternatives ) };
    $@ and error( "Could not write alternatives file: $@" ), return;

    return 1;
}


# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:

1;

















