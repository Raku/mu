use strict;
use Coro;
use Scalar::Util;

=head2 $::Gather

%::GATHER holds the inside-out-ish lazy list instances

take() is defined in GLOBAL.pm

=head3 Parents:

$::Array

=head3 Attributes:

none

=head3 Methods:

=over

=item more

=cut

my $meta_Array = ::DISPATCH($::Array,'HOW');

$::Gather = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Gather,
    name    => "Gather",
    parents => [$meta_Array],
    methods => {

        new => sub {
            my $code            = $_[1];
            my $gather_finished = ::DISPATCH( $::Bit, 'new', 0 );
            my $gather_coro     = Coro::async {
                ::DISPATCH( $code, "APPLY" );

                # cleanup the pointer to the lazy buffer
                delete $::GATHER{ Scalar::Util::refaddr($Coro::current) };
                $gather_finished->{_value} = 1;
                return;
            };

            #print "# Coro started at ", Scalar::Util::refaddr( $gather_coro ), "\n";
            my $buf = ::DISPATCH( $::Array, 'new' );
            $::GATHER{ Scalar::Util::refaddr($gather_coro) } = $buf->{_value}{_array};
            my $v = {
                %{ $_[0] },
                _value => {
                    code     => $code, # used by .perl
                    buf      => $buf,
                    finished => $gather_finished,
                    _coro    => Scalar::Util::refaddr($gather_coro),
                },
            };
        },
        _more => sub { Coro::cede() },
    },
);

