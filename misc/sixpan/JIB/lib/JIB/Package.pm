package JIB::Package;

use strict;
use warnings;
use base 'Object::Accessor';

use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];

my $Package_re = qr/^(\w+)     - # prefix
                    ([\w-]+?)  - # package name
                    ([\d.]+)   - # version
                    (\w+\+\S+) $ # authority
                /smx;

=head1 ACCESSORS 

=head2 package

Set the name of the full package package. For example:

    p5-foo-1-cpan+kane

=head1 METHODS

=head2 $pkg = JIB::Package->new({ key => val, ....})

=cut

{   my %Acc = (
        package => $Package_re,
    );        

    sub new {
        my $class   = shift;
        my %hash    = @_;
        
        my $tmpl = {
            package => { required => 1, allow => $Package_re },
        };
        
        my $args = check( $tmpl, \%hash ) 
                    or error( Params::Check->last_error ), return;

        ### set up the object + accessors        
        my $self = $class->SUPER::new;
        $self->mk_accessors( \%Acc );
        
        while( my($acc,$val) = each %$args ) {
            $self->$acc( $val );
        }
        
        return $self;
    }
}


=head2 prefix

=head2 name

=head2 version

=head2 authority

=cut

### XXX could autogenerate
{   

    sub prefix {
        return $1 if shift->package() =~ $Package_re;
    }

    sub name {
        return $2 if shift->package() =~ $Package_re;
    }

    sub version {
        return $3 if shift->package() =~ $Package_re;
    }
    
    sub authority {
        return $4 if shift->package() =~ $Package_re;
    }
}    

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
