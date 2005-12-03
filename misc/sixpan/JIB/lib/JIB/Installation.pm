package JIB::Installation;

use strict;
use warnings;

use JIB::Config;
use JIB::Utils;



use YAML                    qw[LoadFile];
use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];

use base 'Object::Accessor';

=head1 ACCESSORS

=head1 METHODS

=head2 $inst = JIB::Installation->new;

=cut

{   my $config  = JIB::Config->new;

    sub new { 
        my $class   = shift;
        my $obj     = $class->SUPER::new;
    
        ### XXX tidy up
        {   $obj->mk_accessors( qw[_available _registered_alternatives] );
            
            my @avail = eval { LoadFile( $config->available ) };
            $@ and error( "Could not load available file: $@" ), return;
            $obj->_available( \@avail );
            
            my $href = eval { LoadFile( $config->registered_alternatives ) };
            $@ and error( "Could not load registered alts file: $@" ), return;
            $obj->_registered_alternatives( $href );
        }
        
        return $obj;
    }
}
