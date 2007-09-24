
use v5;
use strict 'vars';

# Usage:
# my $meth = ::DISPATCH( $::Method, 'new', sub { 'hi' } );
# my $obj =  ::DISPATCH( $::Object, 'new', $candidate );

package KindaPerl6::Runtime::Perl5::MOP;
use KindaPerl6::Runtime::Perl5::DispatchSugar;
use Data::Dumper;
use Carp qw(confess);
use FindBin;
use UNIVERSAL;

=for some possible later use
    {
        package P6opaque;
        # This is the most minimal object system, which could be used by the high-level implementation

        my $methods  = {};
        my $dispatch = sub {
            # $self, $method
        };
        $methods->{new} = sub { my $class = shift;  my $v = sugar { _dispatch => $dispatch, $_[0]{_value}, @_ } };
    }
=cut

# sugar routines

sub ::DISPATCH {
    my $invocant = shift;
    unless ($invocant->{_dispatch}) {
        confess "DISPATCH: calling @_ on invalid object:",Dumper($invocant),"\n" 
    }
    $invocant->{_dispatch}($invocant,@_);
}

sub ::DISPATCH_VAR {
    my $invocant = shift;
    confess "DISPATCH_VAR:calling @_ on invalid object:",Dumper($invocant),"\n" 
        unless $invocant->{_dispatch_VAR};
    $invocant->{_dispatch_VAR}($invocant,@_);
}

sub make_class {
    my %args = @_;
    my $proto = delete $args{proto};
    my $meta = (
            defined( $proto ) && ::DISPATCH( $proto, 'HOW' )
        )
        || ::DISPATCH( $::Class, 'new', $args{name});
    my %methods = %{$args{methods}};
    while (my ($method_name,$sub) = each %methods) {
        ::DISPATCH($meta,"redefine_method",$method_name,::DISPATCH( $::Method, 'new', { code => $sub } ));
    }
    for my $attribute_name (@{$args{attributes}}) {
        ::DISPATCH($meta,"add_attribute",$attribute_name);
    }
    for my $parent (@{$args{parents}}) {
        ::DISPATCH($meta,"add_parent",$parent);
    }
    return ::DISPATCH($meta,"PROTOTYPE");
}


# MOP implementation

my $meta_Object;

sub get_method_from_metaclass {
    my ( $self, $method_name ) = ( shift, shift );
    #print "looking in $self\n", Dumper($self);
    return $self->{_value}{methods}{$method_name}
        if exists $self->{_value}{methods}{$method_name};
    for my $parent ( @{ $self->{_value}{isa} } ) {
        #print "trying $parent ",$parent->{_isa}[0]{_value}{class_name},"\n", Dumper($parent);
        #print "available $method_name ? @{[ keys %{$parent->{_value}{methods}} ]}\n";
        my $m = get_method_from_metaclass( $parent, $method_name );
        return $m
            if $m;
    }
    return undef;
}
sub get_method_from_object {
    my ( $self, $method_name ) = ( shift, shift );
    # lookup local methods
    return $self->{_methods}{$method_name}
      if exists $self->{_methods}{$method_name};
    # lookup method in the metaclass
    for my $parent ( @{ $self->{_isa} }, $meta_Object ) {
        my $m = get_method_from_metaclass( $parent, $method_name );
        #print "found\n" if $m;
        return $m
          if $m;
    }
    return undef;
}

my $dispatch = sub {
    # $method_name is unboxed
    my ( $self, $method_name ) = ( shift, shift );
    #print "lookup $method_name in $self\n";

    unless ( ref($self) eq 'HASH'
        or ref($self) eq 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch' )
    {
        warn "internal error: wrong object format";
        print Dumper($self);
        return ::DISPATCH( $::Str, 'new', 'Error');
    }

    if ( $self->{_roles}{auto_deref} ) {
        # this object requires FETCH
        my $value = ::DISPATCH_VAR( $self, 'FETCH' );
        return ::DISPATCH( $value, $method_name, @_ );
    }

    if (  !defined $self->{_value} 
       && $method_name eq 'str'
       ) 
    {
        # 'self' is a prototype object
        # it stringifies to the class name
        #print "Class.str: ",$self->{_isa}[0]{_value}{class_name},"\n";
        return ::DISPATCH( $::Str, 'new',  $self->{_isa}[0]{_value}{class_name} )
    }

    my $meth = get_method_from_object( $self, $method_name );
    confess "no method '$method_name' in Class '", $self->{_isa}[0]{_value}{class_name}, "'\n"
        unless $meth;
        
    if ( ref( $meth->{_value} ) eq 'HASH' && exists $meth->{_value}{code} ) {
        # a properly boxed Method
        return ::DISPATCH( $meth, 'APPLY', $self, @_ );
        #return $meth->{_value}{code}->( $self, @_ );
    }
    
    # low-level Method - APPLY can't dispatch itself!
    # warn 'LOW-LEVEL APPLY '.$method_name."\n".join("\n", map { join ",", caller($_) } 1..6)."\n";
    return $meth->{_value}->( $self, @_ );
};

$::dispatch_VAR = sub {
    # VAR() is just like CALL(), but it doesn't call FETCH
    # $method_name is unboxed
    my ( $self, $method_name ) = ( shift, shift );
    my $meth = get_method_from_object( $self, $method_name );

    die "no method '$method_name' in Class '", $self->{_isa}[0]{_value}{class_name}, "'\n"
        unless $meth;
    die "malformed Method object"
        if ( ref( $meth->{_value} ) ne 'HASH' || ! exists $meth->{_value}{code} );

    return $meth->{_value}{code}->( $self, @_ );
};

%::PROTO = (
    _methods  => undef,    # hash
    _roles    => undef,    # hash
    # _modified => undef,
    _value    => undef,       # whatever | %attributes
    _isa      => undef,       # array
    _dispatch => $dispatch,
);

#--- Method

# XXX 'Method' is actually a container, it should probably inherit from Routine,
#     and it would be better if the internals matched Routine's

my $method_new = sugar {
    %::PROTO,
    _value => { code => 
            sub {
                my $v = sugar {
                    %{ $_[0] },
                    _value => $_[1]
                };
                for my $arg_count(1..$#_) {
                    my $arg = $_[$arg_count];
                    if ($::NamedArgument &&
                        ref $arg &&
                        UNIVERSAL::isa($arg,'HASH') &&
                        $arg->{_isa} &&
                        @{$arg->{_isa}} &&
                        grep { $_ == ::DISPATCH($::NamedArgument, 'HOW') } @{$arg->{_isa}}
                       ) {
                        my $key = GLOBAL::_str(::DISPATCH($arg, '_argument_name_'));
                        my $value = ::DISPATCH($arg, 'value');
                        $v->{_value} = {} unless ref $v->{_value} eq 'HASH';
                        $v->{_value}{$key} = $value;
                    }
                }
                $v;
            } 
    },
};
my $method_APPLY = sugar {
    %::PROTO,
    _value => # { code => 
            sub { 
                my $meth = shift;
                $meth->{_value}{code}->( @_ );
            },
        # },
};

my $meta_Method = sugar {
    %::PROTO,
    _value => {
        methods  => { 
            new   => $method_new,
            APPLY => $method_APPLY,
        },
        class_name => 'Method',
    },
};
$::Method = sugar {
    %::PROTO,
    _isa => [$meta_Method],
};
push @{ $method_new->{_isa} },   $meta_Method;
push @{ $method_APPLY->{_isa} }, $meta_Method;
$meta_Method->{_value}{methods}{WHAT} = ::DISPATCH( $::Method, 'new',  sub { $::Method } );
$meta_Method->{_value}{methods}{HOW}  = ::DISPATCH( $::Method, 'new',  sub { $meta_Method } );

#--- Object

# my $meta_Object;
$meta_Object = sugar {
    %::PROTO,

    # _name     => $_[3],
    _value => { class_name => 'Object', },
};
$meta_Object->{_value}{methods}{WHAT} = ::DISPATCH( $::Method, 'new',  sub { $::Object } );
$meta_Object->{_value}{methods}{HOW}  = ::DISPATCH( $::Method, 'new',  sub { $meta_Object } );
$meta_Object->{_value}{methods}{new}  = $method_new;


$::Object                             = sugar {
    %::PROTO,

    # _name     => '$::Object',
    #_isa      => [ $meta_Object ],
};

#--- Class

my $meta_Class = sugar {
    %::PROTO,
      _value => {
        methods    => {},
        roles      => {},
        class_name => 'Class',
      },
};
push @{ $meta_Class->{_isa} }, $meta_Class;
$meta_Class->{_value}{methods}{add_method} = 
    ::DISPATCH( $::Method, 'new', 
    { code => sub {
        my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
        warn "redefining method $_[0]{_value}{class_name}.$meth_name"
          if exists $_[0]{_value}{methods}{$meth_name};
        $_[0]{_value}{methods}{$meth_name} = $_[2];
    } }
);
$meta_Class->add_method(
    'redefine_method',
    ::DISPATCH( $::Method, 'new', 
        { code => sub {
            my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
            $_[0]{_value}{methods}{$meth_name} = $_[2];
        } }
    )
);
$meta_Class->add_method(
    'add_role',
    ::DISPATCH( $::Method, 'new', 
        { code => sub {
            my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
            warn "redefining role $_[0]{_value}{class_name}.$meth_name"
              if exists $_[0]{_value}{roles}{$meth_name};
            $_[0]{_value}{roles}{$meth_name} = $_[2];
        } }
    )
);

# TODO - "get attributes" ???
$meta_Class->add_method(
    'add_attribute',
    ::DISPATCH( $::Method, 'new', 
        { code => sub {
            my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
            $_[0]{_value}{attributes}{$meth_name} = sub { 1 };  # TODO ???
            #$_[0]{_value}{methods}{$meth_name} = sub : lvalue { $_[0]{_value}{$meth_name} };
            $_[0]->add_method( 
                $meth_name, 
                ::DISPATCH( $::Method, 'new',  
                    { code => sub { 
                        # : lvalue is not needed, because we use .STORE() instead
                        
                        #print "# accessing attribute $meth_name\n";
                        
                        # XXX this should come from the Pad, at compile-time !!!
                        our $_MODIFIED;
                        # XXX - when is the right time to initialize attributes?
                        $_[0]{_value}{$meth_name} = 
                          $::Scalar->{_dispatch}( $::Scalar, 'new',
                            { 
                                modified => $_MODIFIED, 
                                name => '...',    # XXX name??? - get name from 'self'
                            } )
                          unless defined $_[0]{_value}{$meth_name};

                        # do we have more parameters? we should store it as the value.
                        if ($_[1]) {
                            ::DISPATCH_VAR( $_[0]{_value}{$meth_name}, 'STORE', $_[1] );
                        }
                        
                        $_[0]{_value}{$meth_name};
                    } } 
                ) 
            );
        } }
    )
);
$meta_Class->add_method( 'WHAT', ::DISPATCH( $::Method, 'new', { code => sub { $::Class    } } ) );
$meta_Class->add_method( 'HOW',  ::DISPATCH( $::Method, 'new', { code => sub { $meta_Class } } ) );
$meta_Class->add_method( 'add_parent',
    ::DISPATCH( $::Method, 'new', { code => sub { push @{ $_[0]{_value}{isa} }, $_[1] } } ) );

$meta_Class->add_method( 'methods',
    ::DISPATCH( $::Method, 'new', 
        { code => sub {
            # TODO - show inherited methods
            # ??? - should this return the Methods and they stringify to method name ???
            ::DISPATCH( $::Array, 'new', 
                    { _array => [ 
                            map { ::DISPATCH( $::Str, 'new', $_ ) } 
                                keys %{ $_[0]{_value}{methods} } 
                        ] }  
            );
        } } )
);
$meta_Class->add_method(
    'new',
    ::DISPATCH( $::Method, 'new', 
        { code => sub {
            # new Class( $class_name )
            my $meta_class = $_[0];
            my $class_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
            my $self_meta = sugar {
                      %::PROTO,
                      _isa => [$meta_Class],
                      _value => {
                          class_name => $class_name,   # XXX should be ::Str
                      },
                };
            my $proto = sugar {
                      %::PROTO,
                      _isa => [$self_meta],
                };
            $self_meta->{_value}{methods}{WHAT} = ::DISPATCH( $::Method, 'new', sub { $proto } );
            $self_meta->{_value}{methods}{HOW}  = ::DISPATCH( $::Method, 'new', sub { $self_meta } );
            $self_meta->{_methods}{PROTOTYPE}   = ::DISPATCH( $::Method, 'new', sub { $proto } );
            $self_meta;
        } }
    )
);
$::Class = sugar {
    %::PROTO,
      _isa => [$meta_Class],
};
push @{ $meta_Method->{_isa} }, $meta_Class;
push @{ $meta_Object->{_isa} }, $meta_Class;
#push @{$meta_Class->{_isa}}, $meta_Object;

#--- Roles

my $meta_Role = ::DISPATCH( $::Class, 'new', "Role");
$::Role = $meta_Role->PROTOTYPE();

# copy Class methods
$meta_Role->{_value}{methods} = { %{ $meta_Class->{_value}{methods} } };

#--- Values

my $meta_Value = ::DISPATCH( $::Class, 'new', "Value");
$::Value = $meta_Value->PROTOTYPE();
$meta_Value->add_method( 'WHICH', ::DISPATCH( $::Method, 'new', { code => sub { ::DISPATCH( $::Str, 'new', "$_[0]{_value}" ) } } ) );
$meta_Value->add_method( 'p5landish', ::DISPATCH( $::Method, 'new', { code => sub { $_[0]{_value} } } ) );
$meta_Value->add_method(
    'say',
    ::DISPATCH( $::Method, 'new', 
        { code => sub { print $_[0]{_value}, "\n" } }
    )
);
$meta_Value->add_method(
    'print',
    ::DISPATCH( $::Method, 'new', 
        { code => sub { print $_[0]{_value} } }
    )
);

# $meta_Value->add_method( 'IS_ARRAY',     ::DISPATCH( $::Method, 'new',  sub { 0 } ) );
# $meta_Value->add_method( 'IS_HASH',      ::DISPATCH( $::Method, 'new',  sub { 0 } ) );
# $meta_Value->add_method( 'IS_CONTAINER', ::DISPATCH( $::Method, 'new',  sub { 0 } ) );
# -- FETCH is implemented in Object
$meta_Value->add_method( 'FETCH',        ::DISPATCH( $::Method, 'new',  sub { $_[0] } ) );

my $meta_Str = ::DISPATCH( $::Class, 'new', "Str");
$::Str = $meta_Str->PROTOTYPE();
$meta_Str->add_parent($meta_Value);
$meta_Str->add_method(
    'perl',
    ::DISPATCH( $::Method, 'new', 
        { code => sub { my $v = ::DISPATCH( $::Str, 'new',  '\'' . $_[0]{_value} . '\'' ) } }
    )
);
$meta_Str->add_method( 'str', ::DISPATCH( $::Method, 'new', { code => sub { $_[0] } } ) );
$meta_Str->add_method(
    'true',
    ::DISPATCH( $::Method, 'new', 
        { code => sub {
            ::DISPATCH( $::Bit, 'new', 
                ( $_[0]{_value} ne '' && $_[0]{_value} ne '0' ) ? 1 : 0 );
        } }
    )
);
$meta_Str->add_method(
    'chars',
    ::DISPATCH( $::Method, 'new', 
        { code => sub {
            ::DISPATCH( $::Int, 'new', length( $_[0]{_value} ) )
        } }
    )
);

my $meta_Int = ::DISPATCH( $::Class, 'new', "Int");
$::Int = $meta_Int->PROTOTYPE();
$meta_Int->add_parent($meta_Value);
$meta_Int->add_method( 'perl',
    ::DISPATCH( $::Method, 'new',  sub { my $v = $::Str->new( $_[0]{_value} ) } ) );
# XXX - Bind to attributes fail, so to move on, a increment is interesting
$meta_Int->add_method( 'increment',
    ::DISPATCH( $::Method, 'new',  sub { $_[0]{_value}++; $_[0] } ) );
$meta_Int->add_method( 'str',
    ::DISPATCH( $::Method, 'new',  sub { my $v = $::Str->new( $_[0]{_value} ) } ) );
$meta_Int->add_method( 'true',
    ::DISPATCH( $::Method, 'new',  sub { $::Bit->new( $_[0]{_value} == 0 ? 0 : 1 ) } ) );

my $meta_Num = ::DISPATCH( $::Class, 'new', "Num");
$::Num = $meta_Num->PROTOTYPE();
$meta_Num->add_parent($meta_Value);
$meta_Num->add_method( 'perl',
    ::DISPATCH( $::Method, 'new',  sub { my $v = $::Str->new( $_[0]{_value} ) } ) );
$meta_Num->add_method( 'str',
    ::DISPATCH( $::Method, 'new',  sub { my $v = $::Str->new( $_[0]{_value} ) } ) );
$meta_Num->add_method( 'true',
    ::DISPATCH( $::Method, 'new',  sub { $::Bit->new( $_[0]{_value} == 0 ? 0 : 1 ) } ) );


my $meta_Bit = ::DISPATCH( $::Class, 'new', "Bit");
$::Bit = $meta_Bit->PROTOTYPE();
$meta_Bit->add_parent($meta_Value);
$meta_Bit->add_method(
    'perl',
    ::DISPATCH( $::Method, 'new', 
        sub { my $v = ::DISPATCH( $::Str, 'new',  $_[0]{_value} ? 'True' : 'False' ) }
    )
);
$meta_Bit->add_method( 'str',
    ::DISPATCH( $::Method, 'new',  sub { my $v = $::Str->new( $_[0]{_value} ) } ) );
$meta_Bit->add_method( 'true',
    ::DISPATCH( $::Method, 'new',  sub { $::Bit->new( $_[0]{_value} ) } ) );

#--- finish Object

sub meta_isa {
    my $meta = shift;
    my $obj  = shift;
    return 1
        if $meta->{_value}{class_name} eq $obj->{_value};
    for my $parent ( @{ $meta->{_value}{isa} } ) {
        return 1
            if meta_isa( $parent, $obj );    
    }
    return 0;
}

$meta_Object->add_method(
    'isa',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $self = shift;
            my $obj  = shift;   # Proto, Subset, Str  ???
            $obj = ::DISPATCH( $obj, 'str' );
            my $meta = ::DISPATCH( $self, 'HOW' );
            return ::DISPATCH( $::Bit, 'new', 
                (  meta_isa( $meta, $obj ) 
                || $obj->{_value} eq 'Object'   # XXX
                ? 1 : 0 
                )
            );
        }
    )
);

$meta_Object->add_method(
    'does',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $self = shift;
            my $obj  = shift;             

            if ( ::DISPATCH( $obj, 'isa', 
                    ::DISPATCH( $::Str, 'new', 'Str' ) 
                 )->{_value} 
            ) {
                # Str                
                # the call to .str is needed in order to stringify the ::Str prototype
                $obj = eval '$::' . ::DISPATCH( $obj, "str" )->{_value};
            }

            if ( ::DISPATCH( $obj, 'isa', 
                    ::DISPATCH( $::Str, 'new', 'Subset' ) 
                 )->{_value} 
            ) {
                # Subset
                my $base_class = $obj->{_value}{base_class};
                my $block      = $obj->{_value}{block};
                my $does = ::DISPATCH( $self, 'does', $base_class );
                #print "does == ", $does->{_value},"\n";
                return $does 
                    unless $does->{_value};

                # XXX TODO - Subset.block should be a ::Code
                return ::DISPATCH( $::Bit, 'new', 
                    ( $block->( $self )->{_value} ? 1 : 0 )
                );

            }
            #print "Testing not-Subset\n";
            $obj = ::DISPATCH( $obj, 'str' );   # Proto or Str  XXX
            return ::DISPATCH( $::Bit, 'new', 1 )
                if exists( $self->{_roles}{ $obj->{_value} } );
            return ::DISPATCH( $self, 'isa', $obj );
        }
    )
);
$meta_Object->add_method(
    'str',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $v = ::DISPATCH( $::Str, 'new', 
                '::' . $_[0]{_isa}[0]{_value}{class_name} . '(...)' );
        }
    )
);
$meta_Object->add_method(
    'int',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $v = ::DISPATCH( $::Int, 'new',  0 + $_[0]{_value} );  # ???
        }
    )
);
$meta_Object->add_method(
    'true',
    ::DISPATCH( $::Method, 'new', 
        sub {
            ::DISPATCH( $::Bit, 'new',  1 );  # ???
        }
    )
);
$meta_Object->add_method(
    'defined',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $v = ::DISPATCH( $::Bit, 'new', ( defined $_[0]{_value} ? 1 : 0 ) );
        }
    )
);

# Object.FETCH is a no-op
$meta_Object->add_method( 'FETCH',        ::DISPATCH( $::Method, 'new',  sub { $_[0] } ) );
# Object.STORE is forbidden
my $method_readonly = ::DISPATCH( $::Method, 'new', 
    { code => sub {
        die "attempt to modify a read-only value";
    } }
);
$meta_Object->add_method( 'STORE', $method_readonly );

#--- back to Value

$::Undef = make_class( proto => $::Undef, name=>"Undef",parents=>[$meta_Value],methods=>{
    perl    => sub { ::DISPATCH($::Str,'new','undef') },
    str     => sub { ::DISPATCH($::Str,'new','') },
    true    => sub { ::DISPATCH($::Bit,'new',0) },
    defined => sub { ::DISPATCH($::Bit,'new',0) },
});

my $meta_Code = ::DISPATCH( $::Class, 'new', "Code");
$::Code = $meta_Code->PROTOTYPE();
$meta_Code->add_parent($meta_Value);
$meta_Code->add_method( 'perl',
    ::DISPATCH( $::Method, 'new',  sub { my $v = $::Str->new( $_[0]{_value}{src} ) } ) );
my $_apply;
$meta_Code->add_method( 'APPLY',
    ::DISPATCH( $::Method, 'new',  
    (
        $_apply = sub { 
           my $self = shift; # the Code object
           my @param = @_;
           #print "param: \n"; #,Dumper(\@param);
           # is there a Junction class?
           if ( $::Junction ) {
              #print "we've got junctions\n";
              for my $index ( 0 .. $#param ) {
                my $j = $param[$index];
                next unless ref $j; 
                if ( ::DISPATCH( $j, 'does', $::Junction )->{_value} ) {
                    my @things = @{ ::DISPATCH( ::DISPATCH( $j, 'things' ), 'array' )->{_value}{_array} };
                    return ::DISPATCH( $::Junction, 'new', 
                      { 
                        type => $j->{_value}{type}, 
                        things => ::DISPATCH( $::Array, 'new', { _array => [ 
                                map {  
                                    $param[$index] = $_;
                                    $_apply->( $self, @param ); 
                                } @things 
                            ],
                        } ),
                      } );
                }
              } 
            }

           $self->{_value}{code}->(@_);
        } 
    ) 
) );
$meta_Code->add_method( 'signature',
    ::DISPATCH( $::Method, 'new',  sub { $_[0]{_value}{signature} } ) );
$meta_Code->add_method( 'code',
    ::DISPATCH( $::Method, 'new',  sub { $_[0] } ) );
$meta_Code->add_method( 'p5landish',
    ::DISPATCH( $::Method, 'new',  sub { $_[0]{_value}{code} } ) );

my $meta_List = ::DISPATCH( $::Class, 'new', "List");
$::List = $meta_List->PROTOTYPE();
$meta_List->add_parent($meta_Value);
# TODO - finish List implementation ...


#--- Subset 
# TODO - hierarchical constraints - Array of Foo
#    - use a linked list of Subsets ???
# -> you can't subclass a subset
my $meta_Subset = ::DISPATCH( $::Class, 'new', "Subset");
$::Subset = $meta_Subset->PROTOTYPE();
$meta_Subset->add_parent($meta_Value);

$meta_Subset->add_attribute( 'base_class' );  # Class
$meta_Subset->add_attribute( 'block' );       # Code

# -> if you instantiate a subset type you get an object of its base type
#    ??? how to implement this?
#    $subset->new() creates a Subset, for now
#$meta_Subset->add_method(
#    'new',
#    ::DISPATCH( $::Method, 'new', 
#        sub {
#            my $self = shift;
#            my $base_type = $self->{_value}{base_type};
#            ::DISPATCH( $base_type, 'new', @_ );
#        }
#    )
#);
$meta_Subset->add_method( 'perl',
    ::DISPATCH( $::Method, 'new',  sub { $::Str->new( '::Subset( base_class => "...", block => "..." )' ) } ) );


#--- Containers

$::meta_Container = ::DISPATCH( $::Class, 'new', "Container");
$::Container = $::meta_Container->PROTOTYPE();
$::meta_Container->add_method(
    'FETCH',
    ::DISPATCH( $::Method, 'new', 
        { code => sub {
            $_[0]{_value}{cell} ? $_[0]{_value}{cell} : ::DISPATCH($::Undef,"new",0);
        } }
    )
);
$::meta_Container->add_method(
    'STORE',
    ::DISPATCH( $::Method, 'new', 
        { code => sub {
            die "attempt to modify a read-only value"
              if $_[0]{_roles}{readonly};
            $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
            $_[0]{_value}{cell} = $_[1];
        } }
    )
);
sub ::MODIFIED {
    $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
}
$::meta_Container->add_method(
    'BIND',
    ::DISPATCH( $::Method, 'new', 
        { code => sub {
            # XXX - see old 'Type.pm'
            $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
            $_[1]{_value}{modified}{ $_[1]{_value}{name} } = 1;
            if ( $_[1]{_roles}{container} ) {
                # Container := Container
                $_[0]{_value} = $_[1]{_value};
                $_[0]{_roles}{readonly} = $_[1]{_roles}{readonly};
            }
            else {
                # Container := Object
                # - add the read-only trait
                $_[0]{_value}{cell}     = $_[1];
                $_[0]{_roles}{readonly} = 1;
            }
            $_[0];
        } }
    )
);

my $meta_Scalar = ::DISPATCH( $::Class, 'new', "Scalar");
$::Scalar = $meta_Scalar->PROTOTYPE();
$meta_Scalar->add_parent($::meta_Container);
$meta_Scalar->add_method(
    'new',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $v = {
                %{ $_[0] },
                _value => $_[1],    # { %{$_[1]}, cell => undef },
                _roles        => { 'container' => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
    )
);

my $meta_Routine = ::DISPATCH( $::Class, 'new', "Routine");
$::Routine = $meta_Routine->PROTOTYPE();
$meta_Routine->add_parent($::meta_Container);
$meta_Routine->add_method( 'STORE', $method_readonly );
$meta_Routine->add_method(
    'APPLY',
    ::DISPATCH( $::Method, 'new', 
        sub { my $self = shift; $self->{_value}{cell}{_value}{code}->(@_) }
    )
);
$meta_Routine->add_method(
    'new',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $v = sugar {
                %{ $_[0] },
                _value => $_[1],    # { cell => undef },
                _roles        => { 'container' => 1, 'auto_apply' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
    )
);
$meta_Routine->add_method(
    'perl',
    ::DISPATCH( $::Method, 'new', 
        sub {
            ::DISPATCH( $::Str, 'new',  $_[0]{_value}{cell}{_value}{src} );
        },
    )
);

# Method isa Routine
$meta_Method->add_parent( $meta_Routine );
# XXX should not need this!
$meta_Method->add_method( 'signature',
    ::DISPATCH( $::Method, 'new',  sub { 
        #print "SIG ", keys %{ $_[0]{_value} }, "\n";
        $_[0]{_value}{signature};
    } ) );



# tests if a variable was initialized at all
# we need this because defined() always return false with prototype objects
$GLOBAL::Code_VAR_defined = ::DISPATCH( $::Code, 'new', 
    { 
        code =>  sub {
            #print "(MOP)DEFINED? \n";
            return ::DISPATCH( $::Bit, 'new',
                ( defined $_[0] ? 1 : 0 )
            );
        }, 
        src => '&GLOBAL::VAR_defined'
    } 
);

# class prototypes are needed because we have circular dependencies
$::Signature
    = make_class( proto => $::Signature, name=>"Signature", methods=>{} );
$::Signature::Item
    = make_class( proto => $::Signature::Item, name=>"Signature::Item", methods=>{} );
$::Array
    = make_class( proto => $::Array, name=>"Array", methods=>{} );
$::Hash
    = make_class( proto => $::Hash, name=>"Hash", methods=>{} );

require KindaPerl6::Runtime::Perl6::Pair;
require KindaPerl6::Runtime::Perl6::NamedArgument;

require KindaPerl6::Runtime::Perl6::Pair;
require KindaPerl6::Runtime::Perl6::NamedArgument;


$::Cell = make_class( proto => $::Cell, name=>"Cell",parent=>[$::meta_Container],methods=>{
    new=>sub {
            my $v = {
                %{ $_[0] },
                _value => $_[1],
                _roles        => { 'container' => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
    STORE=>sub {
           ${$_[0]{_value}{cell}} = $_[1];
        },
    FETCH=>sub {
           return ${$_[0]{_value}{cell}} || ::DISPATCH($::Undef,'new',0);
        },
});
$::HashCell = make_class( proto => $::HashCell, name=>"HashCell",parent=>[$::meta_Container],methods=>{
    new=>sub {
            my $v = {
                %{ $_[0] },
                _value => $_[1],
                _roles        => { 'container' => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
    STORE=>sub {
           ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} } = $_[1];
        },
    FETCH=>sub {
           exists ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} } 
                ? ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} }
                : ::DISPATCH($::Undef,'new',0);
        },
    exists => sub {
           ::DISPATCH( $::Bit, 'new', exists ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} } ? 1 : 0 )
        },
});

require KindaPerl6::Runtime::Perl6::Hash;
require KindaPerl6::Runtime::Perl5::Hash;

require KindaPerl6::Runtime::Perl6::Array;
require KindaPerl6::Runtime::Perl5::Array;

require KindaPerl6::Runtime::Perl6::Capture;
require KindaPerl6::Runtime::Perl6::Signature;
require KindaPerl6::Runtime::Perl6::Match;


sub ::CAPTURIZE {
    my @array;
    my %hash;
    for my $p ( @{ $_[0] } ) {
        #print "param @{[ keys( %{ $p->{_value} } ) ]} \n";
        if (

            # XXX .does bug?
            # ::DISPATCH( $p, 'does', ::DISPATCH( $::Str, 'new', 'Pair' ) ) 
            # ::DISPATCH( $p, 'does', $::Pair ) 

               eval { exists( $p->{_value}{_argument_name_} ) }
            
           ) {
                my $key = ::DISPATCH( ::DISPATCH( $p, '_argument_name_' ), 'str' )->{_value};
                #print "named: $key \n";
                my $value = ::DISPATCH( $p, 'value' );
                #print "value: $value \n";
                $hash{ $key } = $value;
                #print "return\n";
        }
        else {
                push @array, $p;
        }
    }
    ::DISPATCH( $::Capture, 'new', { 
            invocant => undef,  # TODO
            array => 
                ::DISPATCH( $::Array, 'new', { 
                        _array => \@array,
                    }
                ),
            hash => 
                ::DISPATCH( $::Hash, 'new', { 
                        _hash => \%hash,    
                    }
                ),
        } 
    )
}
$::Multi = make_class( proto => $::Multi, name=>"Multi",parent=>[$meta_Code],methods=>{
    APPLY =>sub {
            my $self = shift; 
            my $code = ::DISPATCH( $self, 'select',::CAPTURIZE(\@_));
            ::DISPATCH( $code, 'APPLY', @_ );
        },
});

# load the runtime

my $libpath  = $FindBin::Bin."/lib";
my $runtime5 = 'KindaPerl6/Runtime/Perl5';
my $runtime6 = 'KindaPerl6/Runtime/Perl6';
my @runtime5 = <$libpath/$runtime5/{IO,Math,Kp6Security}.pm>;
my @runtime6 = <$libpath/$runtime6/{IO,Math,Multi,Junction,Range}.pm>;

foreach (map { s,^.*($runtime5/.*)\.pm,$1,; s,/,::,g; $_ } @runtime5) {
    eval "require $_";
    warn "*** Could not load runtime class $_" if $@;
}
foreach (map { s,^.*($runtime6/.*)\.pm,$1,; s,/,::,g; $_ } @runtime6) {
    eval "require $_";
    warn "*** Could not load runtime class $_" if $@;
}

require KindaPerl6::Runtime::Perl6::Prelude;



1;
