
=head1 NAME

Container.pm

=head1 SYNOPSIS

...

=head1 DESCRIPTION

This perl file (not package) contains the global classes for $::Container and
other global classes that have $meta_Container for a parent.

 * $::Container
 * $::Scalar
 * $::ArrayContainer
 * $::HashContainer
 * $::Routine

=cut

use v5;
use strict 'vars';
use Data::Dumper;
use Carp;

=head2 ::DISPATCH_VAR

similar to _dispatch, except calls $invocant->_dispatch_VAR()

=cut

sub ::DISPATCH_VAR {
    my $invocant = shift;

    confess "DISPATCH_VAR: invalid object: ", Dumper($invocant), "\n"
        unless $invocant->{_dispatch_VAR};
    $invocant->{_dispatch_VAR}( $invocant, @_ );
}

=head2 $::dispatch_VAR

this closure is used in various places for making Containers ("variables") work like plain
objects.

That is, if a method is dispatched through $::dispatch_VAR, it gets executed
on the $variable, not on the $variable contents.

used in KindaPerl6/Runtime/Perl5/
 Hash.pm
 List.pm
 Array.pm
 GLOBAL.pm
 MOP.pm

The &GLOBAL::VAR function in GLOBAL.pm provides a Perl 6 API for dispatch_VAR

=cut

$::dispatch_VAR = sub {

    # VAR() is just like CALL(), but it doesn't call FETCH
    # $method_name is unboxed
    my ( $self, $method_name ) = ( shift, shift );
    my $meth = KindaPerl6::Runtime::Perl5::MOP::get_method_from_object( $self, $method_name );

    die "no method '$method_name' in Class '", $self->{_isa}[0]{_value}{class_name}, "'\n"
        unless $meth;
    die "malformed Method object"
        if ( ref( $meth->{_value} ) ne 'HASH' || !exists $meth->{_value}{code} );

    return $meth->{_value}{code}->( $self, @_ );
};


#--- Containers

=head2 $::Container

$::Container is a $::Class object

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

=over

=item FETCH

=item BIND

=item STORE

dies, Read only

=back

=cut

$::Container = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Container,
    name    => 'Container',
    methods => {
        FETCH => sub {
            # XXX - see LOOKUP below
            $_[0]{_value}{cell} ? $_[0]{_value}{cell} : ::DISPATCH( $::Undef, "new", 0 );
        },
        STORE => sub {
            die "attempt to modify a read-only value"
                if $_[0]{_roles}{readonly};
            $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
            $_[0]{_value}{cell} = $_[1];
        },
        BIND => sub {
            # XXX - see old 'Type.pm'
            $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
            $_[1]{_value}{modified}{ $_[1]{_value}{name} } = 1;
            if ( $_[1]{_roles}{container} ) {
                # Container := Container
                $_[0]{_value} = $_[1]{_value};
                $_[0]{_roles}{readonly} = $_[1]{_roles}{readonly};
            }
            else {
                # Container := Object
                # - add the read-only trait
                $_[0]{_value}{cell}     = $_[1];
                $_[0]{_roles}{readonly} = 1;
            }
            $_[0];
        },
        LOOKUP => sub {
            # XXX - this should never happen: FETCH should have returned a Hash instead
            my $self = shift;
            if ( ! defined $self->{_value}{_hash} ) {
                $self = ::DISPATCH( $::HashProxy, "new", {
                    cell => $self->{_value},
                });
            }
            ::DISPATCH( $self->{_value}, 'LOOKUP', @_ );
        },
        INDEX => sub {
            # XXX - this should never happen: FETCH should have returned an Array instead
            my $self = shift;
            if ( ! defined $self->{_value}{_array} ) {  # XXX - will not always work!
                $self = ::DISPATCH( $::ArrayProxy, "new", {
                    cell => $self->{_value},
                });
            }
            ::DISPATCH( $self->{_value}, 'INDEX', @_ );
        },
    },
);

# meta_Container is used in Runtime::Perl5::Hash & Runtime::Perl5::Array
# do not localize
$::meta_Container = ::DISPATCH( $::Container, 'HOW' );

=head2 $::Scalar

$::Scalar is a $::Class object

=head3 Parent:

=over

=item $::meta_Container

=back

=head3 Attributes:

none

=head3 Methods:

=over

=item new

=back

=cut

$::Scalar = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Scalar,
    parents => [$::meta_Container],
    name    => 'Scalar',
    methods => {
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1],   # { %{$_[1]}, cell => undef },
                _roles        => { container => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
    }
);

my $meta_Scalar = ::DISPATCH( $::Scalar, 'HOW' );

=head2 $::ArrayContainer

$::ArrayContainer is a $::Class object

=head3 Parent:

=over

=item $::meta_Container

=back

=head3 Attributes:

none

=head3 Methods:

=over

=item new

=back

=cut

$::ArrayContainer = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::ArrayContainer,
    parents => [$::meta_Container],
    methods => {
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1], # { %{$_[1]}, cell => undef },
                _roles        => { container => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
            $v->{_value}{cell} = ::DISPATCH( $::Array, "new" )
                unless exists $v->{_value}{cell};
            $v;
        },
    }
);

$::HashContainer = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::HashContainer,
    parents => [$::meta_Container],
    methods => {
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1], # { %{$_[1]}, cell => undef },
                _roles        => { container => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
            $v->{_value}{cell} = ::DISPATCH( $::Hash, "new" )
                unless exists $v->{_value}{cell};
            $v;
        },
    }
);

=head2 $::Routine

$::Routine is a $::Class object

=head3 Parent:

=over

=item $::meta_Routine

=back

=head3 Attributes:

none

=head3 Methods:

=over

=item new

=item APPLY

=item perl

=back

=cut

$::Routine = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Routine,
    parents => [$::meta_Container],
    methods => {
        APPLY => sub {
            my $self = shift;
            local $::ROUTINE = $self->{_value}{cell};
            $self->{_value}{cell}{_value}{code}->(@_);
        },
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1],                                     # { cell => undef },
                _roles        => { container => 1, 'auto_apply' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
        perl => sub {
            ::DISPATCH( $::Str, 'new', $_[0]{_value}{cell}{_value}{src} );
        },
        STORE => sub {
            die "attempt to modify a read-only value";
        },
    }
);

my $meta_Routine = ::DISPATCH( $::Routine, 'HOW' );

##############################################################################
# add $meta_Routine as a parent to $meta_Method

# Method isa Routine
::DISPATCH(
    ::DISPATCH( $::Method, 'HOW' ),
    'add_parent',
    $meta_Routine );

# tests if a variable was initialized at all
# we need this because defined() always return false with prototype objects
$GLOBAL::Code_VAR_defined = ::DISPATCH(
    $::Code, 'new',
    {   code => sub {

            #print "(MOP)DEFINED? \n";
            return ::DISPATCH( $::Bit, 'new', ( defined $_[0] ? 1 : 0 ) );
        },
        src => '&GLOBAL::VAR_defined'
    }
);

1;

__END__
