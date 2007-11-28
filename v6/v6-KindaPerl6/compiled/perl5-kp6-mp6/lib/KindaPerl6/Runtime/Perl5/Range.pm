use strict;

=head2 $::Range

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

=cut

my $meta_Value = ::DISPATCH($::Value,'HOW');

$::Range = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Range,
    name    => "Range",
    parents => [$meta_Value],
    methods => {
        eager => sub {
                my $self = shift;
                ::DISPATCH( $::List, 'new', { _array => [ 
                        map { 
                                ::DISPATCH( $::Int, 'new', $_ ) 
                            }
                            ::DISPATCH( ::DISPATCH( $self, 'start' ), 'p5landish' ) .. 
                            ::DISPATCH( ::DISPATCH( $self, 'end' ),   'p5landish' ) 
                    ], } );
            },
    }
);

1;

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
