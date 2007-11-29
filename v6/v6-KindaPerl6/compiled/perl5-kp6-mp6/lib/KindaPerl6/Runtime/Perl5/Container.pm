
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

    confess "DISPATCH_VAR: calling .$_[0] on an invalid object: ", Dumper($invocant), "\n"
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

dies, Read only - if the Container does the 'readonly' Role

=back

=cut


$::Container = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Container,
    name    => 'Container',
    methods => {
        new => sub {
            my $v = {
                %{ $_[0] },
                _roles        => { container => 1, 'auto_deref' => 1 },
                _value        => {},
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
        FETCH => sub {
            #warn "Container.FETCH $_[0]";
            my $self = shift;
            return $self->{_value}{cell}
                if exists $self->{_value}{cell};
            return ::DISPATCH(
                        $::ValueProxy,
                        "new",
                        $self,
                    );
        },
        STORE => sub {
            #warn "Container.STORE $_[0]";
            die "attempt to modify a read-only value"
                if $_[0]{_roles}{readonly};
            $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
            $_[0]{_value}{cell} = $_[1];
        },
        BIND => sub {
            #warn "Container.BIND";
            # XXX - see old 'Type.pm'
            $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
            $_[1]{_value}{modified}{ $_[1]{_value}{name} } = 1;
            if ( $_[1]{_roles}{container} ) {
                # Container := Container
                #if ( ! exists $_[1]{_value}{cell} ) {
                    # bindind to proxy container
                    # force autovivify
                    #warn "autovivify";
                    #::DISPATCH_VAR( $_[1], 'STORE', $::Undef );
                    #$_[1]{_value} = {};
                #}

                $_[0]{_value} = $_[1]{_value};
                #$_[0]->{_value}{$_} = $_[1]->{_value}{$_} for keys %{ $_[1]->{_value} };

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
        exists => sub {
            ::DISPATCH( $::Bit, 'new', 1 );
        },
    },
);

my $meta_Container = ::DISPATCH( $::Container, 'HOW' );

=head2 $::Scalar

$::Scalar is a $::Class object

=head3 Parent:

=over

=item $meta_Container

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
    parents => [$meta_Container],
    name    => 'Scalar',
    methods => {
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1] || {},   # { %{$_[1]}, cell => undef },
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

=item $meta_Container

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
    parents => [$meta_Container],
    methods => {
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1] || {}, # { %{$_[1]}, cell => undef },
                _roles        => { array_container => 1, container => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
            $v->{_value}{cell} = ::DISPATCH( $::Array, "new" )
                unless exists $v->{_value}{cell};
            $v;
        },
        STORE => sub {
            my $self = shift;
            #warn "\@Array.STORE";
            die "attempt to modify a read-only array"
                if $self->{_roles}{readonly};
            $self->{_value}{cell} = ::DISPATCH( $::Array, "new", { _array => [ @_ ] } );
            $self;
        },
        BIND => sub {
            #warn "Array.BIND";

            $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
            $_[1]{_value}{modified}{ $_[1]{_value}{name} } = 1;

            if ( exists $_[1]->{_roles}{array_container} ) {
                # t/todo/69-list-binding.t - test 3
                #print "# BIND \@Array to \@Array\n";
                return ::DISPATCH( $_[0], 'STORE', $_[1] );
            }
            if ( ::DISPATCH( $_[1], 'does', $::List )->{_value} ) {
                # t/todo/69-list-binding.t - test 1
                #print "# BIND \@Array to \@List\n";
                $_[0]{_value}{cell} = ::DISPATCH( $::Array, "new", { _array => [ 
                            ::DISPATCH( $_[1], 'INDEX', 
                                    ::DISPATCH( $::Int, 'new', 0 )
                                )
                        ] } );
                return $_[0];
            }
            if ( ::DISPATCH( $_[1], 'does', $::Array )->{_value} ) {
                #print "# BIND \@Array to [...]\n";
                $_[0]{_value}{cell} = $_[1];
                return $_[0];
                
                
                # XXX infinite loop! ???
                $_[0]{_value}{cell} = ::DISPATCH( $::Array, "new", { _array => [ 
                        ] } );
                $_[0]{_value}{cell}{_value}{_array}[0] = $_[1];
            }

            die "invalid type on Array.BIND";
        },
    }
);

$::HashContainer = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::HashContainer,
    parents => [$meta_Container],
    methods => {
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1] || {}, # { %{$_[1]}, cell => undef },
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

=item $::Container

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
    parents => [$meta_Container],
    methods => {
        APPLY => sub {   # XXX this should be handled by normal FETCH+APPLY
            my $self = shift;
            ::DISPATCH(::DISPATCH_VAR( $self, "FETCH" ),"APPLY",@_);
        },
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1] || {},                                     # { cell => undef },
                _roles        => { container => 1, 'auto_apply' => 1, 'readonly' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
        perl => sub {    # XXX this should be handled by normal FETCH+perl
            ::DISPATCH( $::Str, 'new', $_[0]{_value}{cell}{_value}{src} );
        },
        #STORE => sub {
        #    die "attempt to modify a read-only value";
        #},
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


# ValueProxy is created when an uninitialized Container is FETCH'ed

$::ValueProxy = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::ValueProxy,
    name    => "ValueProxy",
    parents => [ ::DISPATCH( $::Value, 'HOW' ) ],
    methods => {
        new => sub {
            #warn "ValueProxy.new\n";
            my $dispatch = $::Value->{_dispatcher};
            my $v = {
                %{ $_[0] },
                _parent_container    => $_[1], 
                #   _value must not exist, because this is an Undef
                _dispatch => sub {
                        my $self = shift;
                        if ( ! exists $self->{_parent_container}{_value}{cell} ) {
                            return $::Value->{_dispatch}( $self, @_ )
                                if $_[0] eq "INDEX" || $_[0] eq "LOOKUP" || $_[0] eq "exists";
                            return ::DISPATCH( $::Str, "new", "" )
                                if $_[0] eq "Str";
                            return ::DISPATCH( $::Undef, @_ );
                        }
                        ::DISPATCH( $self->{_parent_container}{_value}{cell}, @_ );
                    },
            };
            return $v;
        },
        # FETCH => sub { die "ValueProxy.FETCH !!!\n"; },
        LOOKUP => sub {
            #warn "ValueProxy.LOOKUP";
            my ( $self, @key ) = @_;
            my $parent_container = $self->{_parent_container};
            if ( ! exists $parent_container->{_value}{cell} ) {
                return ::DISPATCH(
                        $::ContainerProxy,
                        "new",
                        sub {
                                if ( ! exists $parent_container->{_value}{cell} ) {
                                    ::DISPATCH_VAR( $parent_container, 'STORE', 
                                        ::DISPATCH( $::Hash, 'new' )
                                    );
                                }
                                return $parent_container
                                    unless @key;
                                ::DISPATCH( $parent_container, 'LOOKUP', @key );
                            },
                );
            }
            return $parent_container
                unless @key;
            return ::DISPATCH( $parent_container, 'LOOKUP', @key );
        },
        INDEX => sub {
            #warn "ValueProxy.INDEX";
            my ( $self, @key ) = @_;
            my $parent_container = $self->{_parent_container};
            if ( ! exists $parent_container->{_value}{cell} ) {
                return ::DISPATCH(
                        $::ContainerProxy,
                        "new",
                        sub {
                                if ( ! exists $parent_container->{_value}{cell} ) {
                                    ::DISPATCH_VAR( $parent_container, 'STORE', 
                                        ::DISPATCH( $::Array, 'new' )
                                    );
                                }
                                return $parent_container
                                    unless @key;
                                ::DISPATCH( $parent_container, 'INDEX', @key );
                            },
                );
            }
            return $parent_container
                unless @key;
            return ::DISPATCH( $parent_container, 'INDEX', @key );
        },
        exists => sub {
            my $parent_container = $_[0]{_parent_container};
            ::DISPATCH( $::Bit, 'new',
                exists $parent_container->{_value}{cell}
                ? 1 : 0 );
        },
    }
);

# ContainerProxy is created when an uninitialized Hash/Array is LOOKUP/INDEX'ed

$::ContainerProxy = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::ContainerProxy,
    name    => "ContainerProxy",
    parents => [ ::DISPATCH( $::Container, 'HOW' ) ],
    methods => {
        new => sub {
            #warn "ContainerProxy.new\n";
            my $v = {
                %{ $_[0] },
                _autovivify   => $_[1],   #  sub
                _roles        => { container => 1, 'auto_deref' => 1 },
                _value        => {},
                _dispatch_VAR => $::dispatch_VAR,
            };
            return $v;
        },
        FETCH => sub {
            my $self = shift;
            return $self->{_value}{cell}
                if exists $self->{_value}{cell};
            return ::DISPATCH(
                $::ValueProxy,
                "new",
                $self,
            );
        },
        STORE => sub {
            my $self = shift;
            my $cell = $self->{_autovivify}( $self );
            ::DISPATCH_VAR( $cell, 'STORE', @_ );
            $self->{_value} = $cell->{_value};
            $cell;
        },
        BIND => sub {
            my $self = shift;
            my $cell = $self->{_autovivify}( $self );
            ::DISPATCH_VAR( $cell, 'BIND', @_ );
            $self->{_value} = $cell->{_value};
            $cell;
        },
        exists => sub {
            my $self = shift;
            ::DISPATCH( $::Bit, 'new',
                exists $self->{_value}{cell}
                ? 1 : 0 );
        },
    }
);


1;

__END__

=begin

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
