use strict;

=head2 $::Array

=head3 Parents:

none; however $::meta_List is being suggested

=head3 Attributes:

none

=head3 Methods:

=over

=item new

=item INDEX

=item elems

=item push

=item pop

=item shift

=item unshift

=item sort

=item map

=cut

$::Array = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto => $::Array,
    name=> 'Array',
    parents=>[
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
            # arity: http://en.wikipedia.org/wiki/Arity, the number of arguments a function takes
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

=head2 $::ArrayCell

=head3 Parents:

$::meta_Container

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

$::ArrayCell = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::ArrayCell,
    name    => 'ArrayCell',
    parents  => [$::meta_Container],
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
            ${ $_[0]{_value}{cell} }[ $_[0]{_value}{key} ] = $_[1];
        },
        FETCH => sub {
            exists ${ $_[0]{_value}{cell} }[ $_[0]{_value}{key} ]
                ? ${ $_[0]{_value}{cell} }[ $_[0]{_value}{key} ]
                : ::DISPATCH( $::Undef, 'new', 0 );
        },
        exists => sub {
            ::DISPATCH( $::Bit, 'new', exists ${ $_[0]{_value}{cell} }[ $_[0]{_value}{key} ] ? 1 : 0 );
        },
    }
);

1;
