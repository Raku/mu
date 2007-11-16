use strict;

$::List = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto => $::List,
    name=>"List",parents=>[$::meta_Value],methods=>
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
            # XXX TODO - readonly!
            my $key = ::DISPATCH(::DISPATCH($_[1],"Int"),"p5landish");
            $_[0]{_value}{_array} = []
                unless defined $_[0]{_value}{_array};  # XXX
            return ::DISPATCH($::ArrayCell,"new",{
                    cell=> $_[0]{_value}{_array},
                    key => $key,
                });
        },
    FETCH=>sub {
            $_[0];
        },
    STORE=>sub {
            die "can't STORE to a List";
        },
    elems =>sub {
            ::DISPATCH($::Int, "new", scalar @{ $_[0]{_value}{_array} } );
        },
    push =>sub {
            die "can't push to a List";
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
            die "can't unshift to a List";
        },
    sort =>sub {
            my $sub = $_[1];
            ::DISPATCH( $::List, 'new',
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
    for =>sub {
            my $sub = $_[1];
            my $arity = ::DISPATCH( ::DISPATCH( $sub, 'signature' ), 'arity' )->{_value};
            #print "ARITY: $arity\n";
            my @list = @{$_[0]{_value}{_array}};
            while ( @list ) {
                my @params = splice( @list, 0, $arity );
                ::DISPATCH(
                    $sub,
                    "APPLY",
                    @params
                );
            }
        },
});

1;
