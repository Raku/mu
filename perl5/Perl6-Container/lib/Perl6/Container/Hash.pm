
# This is a Perl5 file

# ChangeLog
#
# 2005-08-18
# * New Perl5 class "Perl6::Container::Hash::Object"
#   implements a hash in which the keys can be objects
#
# 2005-08-14
# * added functions clone(), elems(), buckets()

# TODO - Finish 'Hash'
# TODO - change accessors to lower case

# Notes:
# * Cell is implemented in the Perl6::Container::Scalar package

use strict;

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Value;
use Perl6::Container::Scalar;

my $class_description = '-0.0.1-cpan:FGLOCK';

class 'Hash'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ [ '$:cell' => { 
                        access => 'rw', 
                        build => sub { die "Not implemented"; Perl6::Cell->new } } ] ],
        DESTROY => sub {
            $_[0]->{'instance_data'}{'$:cell'} = undef;  
        },
        methods => { 
            'access' => sub {
                die "access must be 'ro' or 'rw'"
                    if $_[1] ne 'ro' && $_[1] ne 'rw';
                _('$:cell')->{ro} = $_[1] eq 'ro';
                return ::SELF;
            },
            # @a := @b 
            'bind' => sub {
                my ( $self, $thing ) = @_;
                die "argument to bind() must be a Hash"
                    unless $thing->isa( 'Hash' );
                _('$:cell', $thing->_cell);
                return $self;
            },
            '_cell' =>   sub { _('$:cell') },  # _cell() is used by bind()
            'id' =>      sub { _('$:cell')->{id} },  

            'set_tieable' => sub { _('$:cell')->{tieable} = 1 },
            'tieable' => sub { _('$:cell')->{tieable} != 0 },
            'tie' =>     sub { shift; _('$:cell')->tie(@_) },
            'untie' =>   sub { _('$:cell')->untie },

             # See perl5/Perl6-MetaModel/t/14_AUTOLOAD.t  
            'isa' => sub { ::next_METHOD() },
            'AUTOLOAD' => sub {
                my ($self, @param) = @_;
                my $method = ::AUTOLOAD($self);
                my $tmp = _('$:cell')->fetch;
                return $tmp->$method( @param );
            },
        },
    }
};

# ----- unboxed functions

package Perl6::Container::Hash::Object;

our $obj_id = '**ObJecT**' . rand;

sub STORE {
    my ( $this, $key, $value ) = @_;
    my $s = $key;
    if ( UNIVERSAL::can( $key, 'id' ) ) {
        $s = $obj_id . $key->id
    }
    $s = $obj_id unless defined $key;
    $this->{$s} = [ $key, $value ];
}
sub FETCH {
    my ( $this, $key ) = @_;
    my $s = $key;
    if ( UNIVERSAL::can( $key, 'id' ) ) {
        $s = $obj_id . $key->id
    }
    $s = $obj_id unless defined $key;
    $this->{$s}[1];
}
sub FIRSTKEY {
    my ( $this ) = @_;
    keys %$this;  # force reset the iterator
    my $s = each %$this;
    $this->{$s}[0];
}
sub NEXTKEY {
    my ( $this, $key ) = @_;
    my $s = each %$this;
    $this->{$s}[0];
}
sub EXISTS {
    my ( $this, $key ) = @_;
    my $s = $key;
    if ( UNIVERSAL::can( $key, 'id' ) ) {
        $s = $obj_id . $key->id
    }
    $s = $obj_id unless defined $key;
    exists $this->{$s};
}
sub DELETE {
    my ( $this, $key ) = @_;
    my $s = $key;
    if ( UNIVERSAL::can( $key, 'id' ) ) {
        $s = $obj_id . $key->id
    }
    $s = $obj_id unless defined $key;
    delete $this->{$s};
}
sub CLEAR {
    my ( $this ) = @_;
    %$this = ();
}
sub SCALAR {
    my ( $this ) = @_;
    0 + %$this;
}

package Perl6::Container::Hash;

sub clone { 
    my $tmp = { %{ $_[0] } };
    $tmp;
}

sub elems {
    my @tmp = %{ $_[0] };
    @tmp / 2
}

sub buckets { scalar %{ $_[0] } }

1;
__END__

=head1 NAME

Perl6::Container::Hash - Perl extension for Perl6 "Hash" class

=head1 SYNOPSIS

  use Perl6::Container::Hash;

  ...

=head1 DESCRIPTION

...


=head1 SEE ALSO

Pugs

=head1 AUTHOR

Flavio S. Glock, E<lt>fglock@Egmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Flavio S. Glock

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.


=cut
