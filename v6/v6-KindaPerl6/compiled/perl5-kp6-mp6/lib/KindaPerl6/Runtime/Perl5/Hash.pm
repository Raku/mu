use strict;

=head2 $::Hash

=head3 Parents:

$::Object

=head3 Attributes:

none

=head3 Methods:

=over

=item LOOKUP

=item elems

=item pairs

=item p5landish

=back

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

=cut


$::Hash = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Hash,
    name    => 'Hash',
    parents => [ ],
    methods => {

        new => sub {
            my ( $proto, $param ) = @_;
            my $self = {
                %{$proto},
                _value => { _hash => {} },
            };
            if ($param) {
                for my $key ( keys %{ $param->{_hash} } ) {
                    ::DISPATCH_VAR( ::DISPATCH( $self, 'LOOKUP', $key ), 'STORE', ${ $param->{_hash} }{$key}, );
                }
            }
            return $self;
        },
        LOOKUP => sub {
            my $self = shift;
            return $self
                unless @_;
            my $key
                = ref( $_[0] )
                ? ::DISPATCH( ::DISPATCH( $_[0], "Str" ), "p5landish" )
                : $_[0];
            return $self->{_value}{_hash}{$key}
                if exists $self->{_value}{_hash}{$key};
            return ::DISPATCH(
                $::ContainerProxy,
                "new",
                sub {
                        if ( ! exists $self->{_value}{_hash}{$key} ) {
                            $self->{_value}{_hash}{$key} = ::DISPATCH( $::Container, 'new' );
                        }
                        $self->{_value}{_hash}{$key};
                    },
            );
        },
        elems => sub {
            ::DISPATCH( $::Int, "new", scalar( keys( %{ $_[0]{_value}{_hash} } ) ) );
        },
        pairs => sub {
            ::DISPATCH(
                $::Array, 'new',
                {   _array => [
                        map { ::DISPATCH( $::Pair, 'new', { key => ::DISPATCH( $::Str, 'new', $_ ), value => $_[0]{_value}{_hash}{$_}, } ) }
                            keys %{ $_[0]{_value}{_hash} }
                    ],
                }
            );
        },
        p5landish => sub { $_[0]{_value}{_hash} }
    }
);

=head2 $::HashProxy

=head3 Parents:

$::Hash

head3 Attributes:

none

=head3 Methods:

=over

=item LOOKUP

=item elems

=item pairs

=item p5landish

=cut


$::HashProxy = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::HashProxy,
    name    => 'HashProxy',
    parents => [ ::DISPATCH($::Hash,'HOW') ],
    methods => {

        new => sub {
            my $v = {
                %{ $_[0] },
                _value => $_[1],    # undef or hash
            };
        },
        LOOKUP => sub {
            my $key = ::DISPATCH( ::DISPATCH( $_[1], "Str" ), "p5landish" );
            return ::DISPATCH(
                $::HashProxyCell,
                "new",
                {   cell => $_[0]{_value},
                    key  => $key,
                }
            );
        },
        elems => sub {
            my $self = shift;
            if ( defined $self->{_value} ) {
                return ::DISPATCH( $self->{_value}, "elems" );
            }
            return ::DISPATCH( $::Int, "new", 0 );
        },
        pairs => sub {
            my $self = shift;
            if ( defined $self->{_value} ) {
                return ::DISPATCH( $self->{_value}, "pairs" );
            }
            return ::DISPATCH( $::Array, "new", { _array => [] } );
        },
        p5landish => sub {
            my $self = shift;
            if ( defined $self->{_value} ) {
                return ::DISPATCH( $self->{_value}, "p5landish" );
            }
            return [];
        },
    }
);

1;
