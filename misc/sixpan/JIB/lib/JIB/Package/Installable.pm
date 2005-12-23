package JIB::Package::Installable;

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
    
    my($meta, $inst, $file);
    my $tmpl = {
        meta => { 
            required    => 1, 
            store       => \$meta, 
            allow       => ISA_JIB_META, 
        },
        repository => { 
            required    => 1, 
            store       => \$inst,
            allow       => ISA_JIB_REPOSITORY,
        },
        ### XXX this won't work over http mirrors etc!
        file => {
            required    => 1,
            store       => \$file,
            allow       => FILE_EXISTS,
        }
    };
    
    my $args = check( $tmpl, \%hash ) 
                or error( Params::Check->last_error ), return;

    $self->mk_accessors( qw[repository] );
    
    while( my($acc,$val) = each %$args ) {
        $self->$acc( $val );
    }
    
    $self->package( $self->meta->package );
    
    return $self;
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
