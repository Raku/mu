use strict;

$::Array = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto => $::Array, 
    name=>"Array",parent=>[$::meta_Container],methods=>
    {
    
    new => sub {
            my $v = {
                %{ $_[0] },
                _value => ( $_[1] || { _array => [] } ),  
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
    INDEX=>sub {
            my $key = ::DISPATCH(::DISPATCH($_[1],"int"),"p5landish");
            return ::DISPATCH($::Cell,"new",{cell=>\$_[0]{_value}{_array}[$key]});
        },
    FETCH=>sub {
            $_[0];
        },
    STORE=>sub {
            $_[0]{_value}{_array} = [ 
                @{ ::DISPATCH($_[1],"array")->{_value}{_array} }
            ];
            $_[0];
        },
    elems =>sub {
            ::DISPATCH($::Int, "new", scalar @{ $_[0]{_value}{_array} } );
        },
    push =>sub {
            my $self = shift;
            ::DISPATCH($::Int, 'new', push @{ $self->{_value}{_array} }, @_);  # XXX process List properly
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
            ::DISPATCH($::Int, 'new', unshift @{ $self->{_value}{_array} }, @_);
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
});

1;
