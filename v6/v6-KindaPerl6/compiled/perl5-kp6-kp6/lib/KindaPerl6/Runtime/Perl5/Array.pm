use strict;

$::Array = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto => $::Array, 
    name=>"Array",
    parent=>[ 
            # $::meta_List ???
        ],
    methods=>
    {
    
    new => sub {
            my $v = {
                %{ $_[0] },
                _value => $_[1],  
                _dispatch_VAR => $::dispatch_VAR,
            };
            $v->{_value}{_array} = [] 
                unless defined $v->{_value}{_array};
            $v;
        },
    INDEX=>sub {
            my $key = ::DISPATCH(::DISPATCH($_[1],"Int"),"p5landish");
            $_[0]{_value}{_array} = []
                unless defined $_[0]{_value}{_array};  # XXX 
            return ::DISPATCH($::ArrayCell,"new",{
                    cell=> $_[0]{_value}{_array},
                    key => $key,
                });
        },
    elems =>sub {
            ::DISPATCH($::Int, "new", scalar @{ $_[0]{_value}{_array} } );
        },
    push =>sub {
            my $self = shift;
            my @param = map { ::DISPATCH( $_, 'FETCH' ) } @_;
            ::DISPATCH($::Int, 'new', push @{ $self->{_value}{_array} }, @param);
        },
    pop =>sub {
            my $self = shift;
            pop @{ $self->{_value}{_array} };
        },

    shift =>sub {
            my $self = shift;
            shift @{ $self->{_value}{_array} }, @_;  # XXX process List properly
        },
    unshift =>sub {
            my $self = shift;
            my @param = map { ::DISPATCH( $_, 'FETCH' ) } @_;
            ::DISPATCH($::Int, 'new', unshift @{ $self->{_value}{_array} }, @param);
        },
    sort =>sub {
            my $sub = $_[1];
            ::DISPATCH( $::Array, 'new', 
                    { _array => [
                            sort {
                                ::DISPATCH(
                                        $sub,
                                        "APPLY", 
                                        $a, $b 
                                    )->{_value};
                            } @{$_[0]{_value}{_array}}
                        ],
                    }
            );
        },
    map =>sub {
            my $sub = $_[1];
            my $arity = ::DISPATCH( ::DISPATCH( $sub, 'signature' ), 'arity' )->{_value};
            #print "Array.map arity: $arity\n";
            my $result = ::DISPATCH( $::Array, 'new' );
            my @list = @{$_[0]{_value}{_array}};
            my @params;
            while ( @list ) {
                if ( $arity ) {
                    @params = splice( @list, 0, $arity );
                }
                else {
                    $_ = shift @list;   # ???
                }
                ::DISPATCH( $result, 'push',
                    ::DISPATCH(
                        $sub,
                        "APPLY", 
                        @params,
                    )
                );
            };
            $result;
        },
});

1;
