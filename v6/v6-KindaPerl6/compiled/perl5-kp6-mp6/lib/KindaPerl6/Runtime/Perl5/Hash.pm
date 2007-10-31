use strict;

$::Hash = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto => $::Hash, 
    name=>"Hash",parent=>[$::meta_Container],methods=>
    {
    
    new => sub {
            my $v = {
                %{ $_[0] },
                _value => ( $_[1] || { _hash => {} } ),    
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
    STORE=>sub {
            $_[0]{_value}{_hash} = ::DISPATCH($_[1],"hash")->{_value}{_hash};
            #$_[0]{_value}{_hash} = $_[1]->{_value}{_hash};
            $_[0];
        },
    LOOKUP=>sub {
            my $key = ::DISPATCH(::DISPATCH($_[1],"Str"),"p5landish");
            $_[0]{_value}{_hash} = {}
                unless defined $_[0]{_value}{_hash};  # XXX 
            return ::DISPATCH($::HashCell,"new",{
                    cell=> $_[0]{_value}{_hash},
                    key => $key,
                });
        },
    elems => sub {
            ::DISPATCH($::Int,"new",scalar(keys(%{$_[0]{_value}{_hash}})));
        },
    pairs => sub {
            ::DISPATCH( $::Array, 'new', 
                    { _array => [
                          map {
                                ::DISPATCH( $::Pair, 'new', {
                                        key   => ::DISPATCH( $::Str, 'new', $_ ),
                                        value => $_[0]{_value}{_hash}{$_},
                                    } 
                                )
                            } 
                            keys %{ $_[0]{_value}{_hash} }
                        ],
                    }
                );
        },
    p5landish=> sub { $_[0]{_value}{_hash} }
});
=head2 $::HashCell

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

=over

=item new

=item STORE

=item FETCH

=item exists

=back

=cut

$::HashCell = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::HashCell,
    name    => "HashCell",
    parent  => [$::meta_Container],
    methods => {
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1],
                _roles        => { 'container' => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
        STORE => sub {
            ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} } = $_[1];
        },
        FETCH => sub {
            exists ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} }
                ? ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} }
                : ::DISPATCH( $::Undef, 'new', 0 );
        },
        exists => sub {
            ::DISPATCH( $::Bit, 'new', exists ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} } ? 1 : 0 );
        },
    }
);

1;
