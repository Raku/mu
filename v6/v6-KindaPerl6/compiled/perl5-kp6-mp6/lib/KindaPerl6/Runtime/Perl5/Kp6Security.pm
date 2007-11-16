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

=cut

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
