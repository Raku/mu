package JIB::Package;

use strict;
use warnings;
use base 'Object::Accessor';

=head1 ACCESSORS 

=head2 package

Set the name of the full package package. For example:

    p5-foo-1-cpan+kane

=head1 METHODS

=head2 $pkg = JIB::Package->new({ key => val, ....})

=cut

{   my @Acc = qw[package];
    sub new {
        my $class   = shift;
        my $self    = $class->SUPER::new;
        
        $self->mk_accessors( @Acc );
        
        return $self->_init(@_) ? $self : undef;
    }
}


sub _init {
    my ($self, $args) = @_;
    if ($args && ref $args eq 'HASH') {
        for my $arg (keys %{$args}) {
            next unless $arg;
            die __PACKAGE__ . " doesn't have an accessor method $arg" 
                 unless $self->can($arg);
            $self->$arg($args->{$arg});
        }
    }

    return 1;
}

=head2 prefix

=head2 name

=head2 version

=head2 authority

=cut

### XXX could autogenerate
{   my $regex = qr/ ^(\w+)     - # prefix
                    ([\w-]+?)  - # package name
                    ([\d.]+)   - # version
                    (\w+\+\S+) $ # authority
                /smx;

    sub prefix {
        return $1 if shift->package() =~ $regex;
    }

    sub name {
        return $2 if shift->package() =~ $regex;
    }

    sub version {
        return $3 if shift->package() =~ $regex;
    }
    
    sub authority {
        return $4 if shift->package() =~ $regex;
    }
}    

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
