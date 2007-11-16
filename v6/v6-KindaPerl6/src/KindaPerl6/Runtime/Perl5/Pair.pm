use strict;

=head2 $::Pair

=head3 Parents:

$::meta_Value

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
