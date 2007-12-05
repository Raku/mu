# use strict; $meta_Value is not defined

=head2 $::Kp6Security

=head3 Parents:

meta_Value

=head3 Attributes:

none

=head3 Methods:

=over

=item guard_insecure_code

This will die if Main::KP6_DISABLE_INSECURE_CODE is true.

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

my $meta_Value = ::DISPATCH($::Value,'HOW');

$::Kp6Security = KindaPerl6::Runtime::Perl5::MOP::make_class(
    name    => 'Kp6Security',
    parents => [$meta_Value],
    methods => {
        guard_insecure_code => sub {
            if (Main::KP6_DISABLE_INSECURE_CODE) {
                my ( $package, $filename, $line ) = caller(5);
                my $msg = "forbidden code at $filename line $line\n";
                $msg .= "              ";
                die $msg;
            }
            else {
                ::DISPATCH( $::Int, 'new', 1 );
            }
        },
    }
);

1;
