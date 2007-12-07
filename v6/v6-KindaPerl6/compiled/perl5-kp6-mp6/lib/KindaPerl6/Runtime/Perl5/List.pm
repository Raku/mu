use strict;

=head2 $::List

=head3 Parents:

$::Value

=head3 Attributes:

none

=head3 Methods:

=over

=item FETCH

=item STORE

=item elems

=item push

=item pop

=item shift

=item unshift

=item sort

=item for

=item eager

=item INDEX

=item map

=item new

=cut

my $meta_Value = ::DISPATCH($::Value,'HOW');

$::List = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::List,
    name    => 'List',
    parents => [$meta_Value],
    methods => {

        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1],
                _dispatch_VAR => $::dispatch_VAR,
            };
            $v->{_value}{_array} = []
                unless defined $v->{_value}{_array};
            $v;
        },
        INDEX => sub {
            my $key = ::DISPATCH( ::DISPATCH( $_[1], 'Int' ), 'p5landish' );
            return ::DISPATCH( $::Undef, 'new' )
                unless exists $_[0]{_value}{_array}[$key];
            return $_[0]{_value}{_array}[$key];
        },
        FETCH => sub {
            $_[0];
        },
        STORE => sub {
            die "Cannot STORE to a List";
        },
        eager => sub {
            $_[0];
        },
        elems => sub {
            ::DISPATCH( $::Int, 'new', scalar @{ $_[0]{_value}{_array} } );
        },
        push => sub {
            die "Cannot push to a List";
        },
        pop => sub {
            my $self = shift;
            pop @{ $self->{_value}{_array} };
        },
        shift => sub {
            my $self = shift;
            shift @{ $self->{_value}{_array} }, @_;    # XXX process List properly
        },
        unshift => sub {
            die "Cannot unshift to a List";
        },
        sort => sub {
            my $sub = $_[1];
            ::DISPATCH( $::List, 'new', { _array => [ sort { ::DISPATCH( $sub, "APPLY", $a, $b )->{_value}; } @{ $_[0]{_value}{_array} } ], } );
        },
        map =>sub {
            local $_;
            my $sub = $_[1];
            # arity: http://en.wikipedia.org/wiki/Arity, the number of arguments a function takes
            my $arity = ::DISPATCH( ::DISPATCH( $sub, 'signature' ), 'arity' )->{_value};
            #print "List.map arity: $arity\n";
            my $result = ::DISPATCH( $::List, 'new' );
            my @list = @{$_[0]{_value}{_array}};
            my @params;
            while ( @list ) {
                if ( $arity ) {
                    @params = splice( @list, 0, $arity );
                }
                else {
                    $_ = shift @list;   # ???
                }
                push @{ $result->{_value}{_array} },
                    ::DISPATCH(
                        $sub,
                        "APPLY",
                        @params,
                    );
            };
            $result;
        },
    }
);

1;

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
