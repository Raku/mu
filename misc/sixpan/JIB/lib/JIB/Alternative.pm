package JIB::Alternative;

use strict;
use warnings;

use JIB::Config;
use JIB::Utils;

use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];
use Data::Dumper;

use base 'Object::Accessor';

=head1 ACCESSORS

=head1 METHODS

=head $alt = JIB::Alternative->new_from_struct( struct => $struct );

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
    
    return $obj;
}

### XXX c/p'd from meta.pm -- subclass/inherit
=head2 $struct = $alt->to_struct;

=cut

sub to_struct {
    my $self = shift;

    my %struct = map { $_ => $self->$_ } $self->ls_accessors;

    return \%struct;
}

### XXX c/p'd this from JIB::Package -- subclass/inherit!
my $Package_re = qr/^(\w+)     - # prefix
                    ([\w-]+?)  - # package name
                    ([\d.]+)   - # version
                    (\w+\+\S+) $ # authority
                /smx;

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

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:

1;

















