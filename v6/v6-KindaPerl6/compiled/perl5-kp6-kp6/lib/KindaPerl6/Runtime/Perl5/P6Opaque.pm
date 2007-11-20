use strict;

=head2 $::P6Opaque

could be used for creating a metamodel in p6

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

=over

=item new

=cut

#
$::P6Opaque = make_class(
    name    => 'P6Opaque',
    methods => {
        new => sub {

            #print join ',',map {GLOBAL::_str($_)} @_,"\n";
            my $self    = shift;
            my $capture = ::CAPTURIZE( \@_ );
            print GLOBAL::_str($capture), "\n";
            my $dispatcher = ::DISPATCH( $capture, "LOOKUP", ::DISPATCH( $::Str, 'new', 'dispatcher' ) );
            my $data       = ::DISPATCH( $capture, "LOOKUP", ::DISPATCH( $::Str, 'new', 'data' ) );
            return {
                _dispatch => sub {
                    my $self   = shift;
                    my $method = shift;
                    ::DISPATCH( $dispatcher, 'APPLY', $data, ::DISPATCH( $::Str, 'new', $method ), ::CAPTURIZE( \@_ ) );
                    }
            };
            }
    }
);
