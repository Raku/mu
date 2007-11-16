use strict;

$::Hash = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto => $::Hash,
    name=>"Hash",parents=>[$::meta_Container],methods=>
    {

    new => sub {
            my ( $proto, $param ) = @_;
            my $self = {
                %{ $proto },
                _value => { _hash => {} },            # ( $_[1] || { _hash => {} } ),
                _dispatch_VAR => $::dispatch_VAR,     # XXX
            };
            if ( $param ) {
                for my $key ( keys %{ $param->{_hash} } ) {
                    ::DISPATCH_VAR(
                        ::DISPATCH( $self, 'LOOKUP', $key ),
                        'STORE',
                        ${ $param->{_hash} }{$key},
                    );
                }
            }
            return $self;
        },
    LOOKUP=>sub {
            my $self = shift;
            my $key  = ref($_[0])
                    ? ::DISPATCH( ::DISPATCH($_[0], "Str" ), "p5landish" )
                    : $_[0];

            return $self->{_value}{_hash}{$key}
                if exists $self->{_value}{_hash}{$key};

            return ::DISPATCH(
                $::ContainerProxy,
                "new",
                {
                    FETCH   => sub {
                            #warn "Hash.{}.FETCH!";
                            my $self = shift;
                            return $self->{_value}{cell}
                                if exists $self->{_value}{cell};
                            return ::DISPATCH(
                                        $::ValueProxy,
                                        "new",
                                        {
                                            _parent => $self,
                                            _parent_exists => sub { exists $self->{_value}{cell} },
                                            INDEX  => $::INDEX_sub,
                                            LOOKUP => $::LOOKUP_sub,
                                        }
                                    );
                    },
                    STORE   => sub {
                        #warn "Hash.{x}.STORE!";
                        shift;
                        my $cell = exists $self->{_value}{_hash}{$key}
                                ? $self->{_value}{_hash}{$key}
                                : ( $self->{_value}{_hash}{$key} = ::DISPATCH( $::Container, 'new' ) );
                        ::DISPATCH_VAR(
                            $cell,
                            'STORE',
                            @_
                        );
                    },
                    BIND    => sub { die "BIND!"  },
                }
            );
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


$::HashProxy = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto => $::HashProxy,
    name=>"HashProxy",parents=>[$::meta_Hash],methods=>
    {

    new => sub {
            my $v = {
                %{ $_[0] },
                _value => $_[1],    # undef or hash
            };
        },
    LOOKUP => sub {
            my $key = ::DISPATCH(::DISPATCH($_[1],"Str"),"p5landish");
            return ::DISPATCH($::HashProxyCell,"new",{
                    cell=> $_[0]{_value},
                    key => $key,
                });
        },
    elems => sub {
            my $self = shift;
            if ( defined $self->{_value} ) {
                return ::DISPATCH( $self->{_value} ,"elems" );
            }
            return ::DISPATCH( $::Int, "new", 0 );
        },
    pairs => sub {
            my $self = shift;
            if ( defined $self->{_value} ) {
                return ::DISPATCH( $self->{_value} ,"pairs" );
            }
            return ::DISPATCH( $::Array, "new", { _array => [] } );
        },
    p5landish=> sub {
            my $self = shift;
            if ( defined $self->{_value} ) {
                return ::DISPATCH( $self->{_value} ,"p5landish" );
            }
            return [];
        },
});

1;
