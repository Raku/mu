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
    my $conf    = $self->config;
    my %hash    = @_;
    
    my $meta; my $inst;
    my $tmpl = {
        meta    => { required => 1, store => \$meta, allow => ISA_JIB_META },
        installation
                => { required => 1, store => \$inst,
                    allow => ISA_JIB_INSTALLATION 
        },
    };
    
    my $args = check( $tmpl, \%hash ) 
                or error( Params::Check->last_error ), return;

    $self->mk_accessors( qw[files alternative installation] );
    
    while( my($acc,$val) = each %$args ) {
        $self->$acc( $val );
    }
    
    $self->package( $self->meta->package );
    
    ### XXX flesh out this format better
    {   my @files = eval {
            ### XXX CONFIG!!!!
            my $file = $inst->files_list( $meta->package );
            open my $fh, $file or die "Could not open '$file': $!";
            map { chomp; $_ } <$fh>;
        };
        $@ and error( "Could not load files list: $@" ), return;
        $self->files( \@files );
        
        my ($alt) = grep { $_->package eq $meta->package }
                        @{ $inst->registered_alternatives };
        $self->alternative( $alt );
    }
    
    return $self;
}

=head2 $pkg->install( ... )

=cut

sub install {
    my $self = shift;
    msg("Package already installed");
    return $self;
}    

=head2 $bool = $inst_pkg->uninstall

=cut

sub uninstall {
    my $self = shift;
    my $inst = $self->installation;
    
    return $inst->unregister( package => $self );
}

1;


# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
