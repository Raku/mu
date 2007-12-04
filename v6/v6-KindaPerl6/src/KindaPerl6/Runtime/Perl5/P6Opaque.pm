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

=back

=cut

$::P6Opaque = make_class(
    name    => 'P6Opaque',
    methods => {
        new => sub {

            #print join ',',map {GLOBAL::_str($_)} @_,"\n";
            my $self    = shift;
            my $capture = ::CAPTURIZE( \@_ );
            print GLOBAL::_str($capture), "\n";
            my $dispatcher = ::DISPATCH( $capture, 'LOOKUP', ::DISPATCH( $::Str, 'new', 'dispatcher' ) );
            my $data       = ::DISPATCH( $capture, 'LOOKUP', ::DISPATCH( $::Str, 'new', 'data' ) );
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

1;

__END__

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
