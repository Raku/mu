use strict;

=head2 $::Pair

=head3 Parents:

$::Value

=head3 Attributes:

none

=head3 Methods:

=over

=item hash

=cut

my $meta_Value = ::DISPATCH($::Value,'HOW');

$::Pair = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Pair,
    name    => "Pair",
    parents => [$meta_Value],
    methods => {
        hash => sub {
            my $key = ::DISPATCH( ::DISPATCH( $_[0]{_value}{key}, "Str" ), "p5landish" );

            #print "value = ",::DISPATCH(::DISPATCH( $_[0]{_value}{value}, "Str" ),"p5landish")," ";
            #print "PAIR: $key => $_[0]{_value}{value} \n";
            my $h = ::DISPATCH( $::Hash, "new", { _hash => { $key => $_[0]{_value}{value} } } );
            return $h;
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
