package JIB::Package::Installed;

use strict;
use warnings;

use base 'JIB::Package';

use JIB::Constants;
use JIB::Config;
use JIB::Installation;

use File::Spec;
use File::Basename          qw[basename];
use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];
use YAML                    qw[LoadFile];



### only called form JIB::Package
sub new {
    my $self    = shift;
    my %hash    = @_;
    my $config  = $self->config;
    
    my $meta;
    my $tmpl = {
        meta    => { required => 1, store => \$meta, 
                     allow => sub { UNIVERSAL::ISA(shift(), 'JIB::META') } },
    };
    
    my $args = check( $tmpl, \%hash ) 
                or error( Params::Check->last_error ), return;
    
    while( my($acc,$val) = each %$args ) {
        $self->$acc( $val );
    }

    $self->package( $self->meta->package );
    
    return $self;
}

=head2 $pkg->install( ... )

=cut

sub install {
    msg("Package already installed");
    return 1;
}    

1;


# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
