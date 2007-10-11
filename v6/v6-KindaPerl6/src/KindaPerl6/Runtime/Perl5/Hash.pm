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
            my $key = ::DISPATCH(::DISPATCH($_[1],"str"),"p5landish");
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

1;
