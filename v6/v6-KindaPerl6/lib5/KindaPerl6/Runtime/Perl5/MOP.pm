
use v5;

# my $meth = ::DISPATCH( $::Method, 'new', sub { 'hi' } );
# my $obj =  ::DISPATCH( $::Object, 'new', $candidate );

package KindaPerl6::Runtime::Perl5::MOP;
use KindaPerl6::Runtime::Perl5::DispatchSugar;
use Data::Dumper;
use Carp qw(confess);

#print "# Initializing MOP.pm\n";

{

    package P6opaque;

    my $methods  = {};
    my $dispatch = sub {

        # $self, $method

    };
    $methods->{new} = sub { my $v = sugar { _dispatch => $dispatch, $_[0]{_value} } };
}

sub ::DISPATCH {
    my $invocant = shift;
    confess "DISPATCH: calling @_ on invalid object:",Dumper($invocant),"\n" unless $invocant->{_dispatch};
    $invocant->{_dispatch}($invocant,@_);
}

sub ::DISPATCH_VAR {
    my $invocant = shift;
    confess "DISPATCH_VAR:calling @_ on invalid object:",Dumper($invocant),"\n" unless $invocant->{_dispatch_VAR};
    $invocant->{_dispatch_VAR}($invocant,@_);
}

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

my $meta_Object;

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

    if ( !defined $self->{_value} ) {

        # 'self' is a prototype object
        # it stringifies to the class name
        #print "Class.str: ",$self->{_isa}[0]{_value}{class_name},"\n";
        return ::DISPATCH( $::Str, 'new',  $self->{_isa}[0]{_value}{class_name} )
          if $method_name eq 'str';
    }

    # lookup local methods
    return $self->{_methods}{$method_name}{_value}->( $self, @_ )
      if exists $self->{_methods}{$method_name};

    # lookup method in the metaclass
    #print "# lookup '$method_name' in Class '", $self->{_isa}[0]{_value}{class_name}, "'\n";
    for my $parent ( @{ $self->{_isa} }, $meta_Object ) {
        my $m = get_method_from_metaclass( $parent, $method_name );

        #print $m ? "found\n" : "not found\n";
        #print "Method: ",Dumper($m);
        return $m->{_value}->( $self, @_ )
          if $m;
    }
    die "no method '$method_name' in Class '", $self->{_isa}[0]{_value}{class_name}, "'\n";
};

# backwards compatibility - remove !!!
#sub ::CALL { $dispatch->(@_) }

my $dispatch_VAR = sub {

    # VAR() is just like CALL(), but it doesn't call FETCH
    # $method_name is unboxed
    my ( $self, $method_name ) = ( shift, shift );

    # lookup local methods
    return $self->{_methods}{$method_name}{_value}->( $self, @_ )
      if exists $self->{_methods}{$method_name};

    # lookup method in the metaclass
    for my $parent ( @{ $self->{_isa} }, $meta_Object ) {
        my $m = get_method_from_metaclass( $parent, $method_name );

        #print "found\n" if $m;
        return $m->{_value}->( $self, @_ )
          if $m;
    }
    die "no VAR() method: $method_name\n";
};

%::PROTO = (
    _methods  => undef,    # hash
    _roles    => undef,    # hash

    # _modified => undef,
    # _name     => '',
    _value    => undef,       # whatever | %attributes
    _isa      => undef,       # array
    _dispatch => $dispatch,
);

#--- Method

my $method_new = sugar {
    %::PROTO,

      # _name     => '$method_new',
      _value => sub {

        #print "Calling new from @{[ caller ]} \n";
        my $v = sugar {
            %{ $_[0] },
            _value => $_[1],    # || 0,
                                # _name  => '',
        };
      },
};

my $meta_Method = sugar {
    %::PROTO,

      # _name     => '$meta_Method',
      _value => {
        methods    => { new => $method_new },
        class_name => 'Method',
      },
};
$::Method = sugar {
    %::PROTO,

      # _name     => '$::Method',
      _isa => [$meta_Method],
};
push @{ $method_new->{_isa} }, $meta_Method;
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

      # _name     => '$meta_Class',
      _value => {
        methods    => {},
        roles      => {},
        class_name => 'Class',
      },
};
push @{ $meta_Class->{_isa} }, $meta_Class;
$meta_Class->{_value}{methods}{add_method} = ::DISPATCH( $::Method, 'new', 
    sub {
        my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
        warn "redefining method $_[0]{_value}{class_name}.$meth_name"
          if exists $_[0]{_value}{methods}{$meth_name};
        $_[0]{_value}{methods}{$meth_name} = $_[2];
    }
);
$meta_Class->add_method(
    'redefine_method',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
            $_[0]{_value}{methods}{$meth_name} = $_[2];
        }
    )
);

$meta_Class->add_method(
    'add_role',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
            warn "redefining role $_[0]{_value}{class_name}.$meth_name"
              if exists $_[0]{_value}{roles}{$meth_name};
            $_[0]{_value}{roles}{$meth_name} = $_[2];
        }
    )
);

# TODO - "get attributes" ???
$meta_Class->add_method(
    'add_attribute',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
            $_[0]{_value}{attributes}{$meth_name} = sub { 1 };  # TODO ???
            #$_[0]{_value}{methods}{$meth_name} = sub : lvalue { $_[0]{_value}{$meth_name} };
            $_[0]->add_method( 
                $meth_name, 
                ::DISPATCH( $::Method, 'new',  
                    sub { 
                        # : lvalue is not needed, because we use .STORE() instead
                        
                        #print "accessing attribute $meth_name\n";
                        
                        # XXX - when is the right time to initialize attributes?
                        $_[0]{_value}{$meth_name} = 
                          $::Scalar->{_dispatch}( $::Scalar, 'new',
                            { 
                                modified => $_MODIFIED, 
                                name => '...',    # XXX name??? - get name from 'self'
                            } )
                          unless defined $_[0]{_value}{$meth_name};
                        
                        $_[0]{_value}{$meth_name};
                    } 
                ) 
            );
        }
    )
);
$meta_Class->add_method( 'WHAT', ::DISPATCH( $::Method, 'new',  sub { $::Class } ) );
$meta_Class->add_method( 'HOW',  ::DISPATCH( $::Method, 'new',  sub { $meta_Class } ) );
$meta_Class->add_method( 'add_parent',
    ::DISPATCH( $::Method, 'new',  sub { push @{ $_[0]{_value}{isa} }, $_[1] } ) );

$meta_Class->add_method(
    'new',
    ::DISPATCH( $::Method, 'new', 
        sub {

#print "Calling Class.new from @{[ caller ]} \n";
# new Class( $prototype_container, $prototype_container_name, $meta_container, $meta_container_name, $class_name )

            my $meta_class = $_[0];

            my $class_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];


            my $self_meta = sugar {
                %::PROTO,

                  _value => {

                    class_name => $class_name,
                  },
                  _isa => [$meta_Class],
            };
            $self_meta->{_value}{methods}{WHAT} = ::DISPATCH( $::Method, 'new', 
                sub {

                    $self;
                }
            );
            $self_meta->{_value}{methods}{HOW} =
              ::DISPATCH( $::Method, 'new',  sub { $self_meta } );

            $self_meta->{_methods}{PROTOTYPE} =
              ::DISPATCH( $::Method, 'new',  sub {  
                $self = sugar {
                  %::PROTO,
                  _isa => [$self_meta],
                }
            });

            $self_meta;
        }
    )
);
$::Class = sugar {
    %::PROTO,

      # _name     => '$::Class',
      _isa => [$meta_Class],
};

#print "CLASS = ",Dumper($meta_Class);

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
$meta_Value->add_method( 'p5landish', ::DISPATCH( $::Method, 'new',  sub { $_[0]{_value} } ) );

$meta_Value->add_method(
    'say',
    ::DISPATCH( $::Method, 'new', 
        sub { print $_[0]{_value}, "\n" }
    )
);
$meta_Value->add_method(
    'print',
    ::DISPATCH( $::Method, 'new', 
        sub { print $_[0]{_value} }
    )
);

# $meta_Value->add_method( 'IS_ARRAY',     ::DISPATCH( $::Method, 'new',  sub { 0 } ) );
# $meta_Value->add_method( 'IS_HASH',      ::DISPATCH( $::Method, 'new',  sub { 0 } ) );
# $meta_Value->add_method( 'IS_CONTAINER', ::DISPATCH( $::Method, 'new',  sub { 0 } ) );
# -- FETCH is implemented in Object
# $meta_Value->add_method( 'FETCH',        ::DISPATCH( $::Method, 'new',  sub { $_[0] } ) );

my $meta_Str = ::DISPATCH( $::Class, 'new', "Str");
$::Str = $meta_Str->PROTOTYPE();
$meta_Str->add_parent($meta_Value);
$meta_Str->add_method(
    'perl',
    ::DISPATCH( $::Method, 'new', 
        sub { my $v = ::DISPATCH( $::Str, 'new',  '\'' . $_[0]{_value} . '\'' ) }
    )
);
$meta_Str->add_method( 'str', ::DISPATCH( $::Method, 'new',  sub { $_[0] } ) );
$meta_Str->add_method(
    'true',
    ::DISPATCH( $::Method, 'new', 
        sub {
            ::DISPATCH( $::Bit, 'new', 
                ( $_[0]{_value} ne '' && $_[0]{_value} ne '0' ) ? 1 : 0 );
        }
    )
);
# $meta_Str->add_method( 'p5landish', ::DISPATCH( $::Method, 'new',  sub { $_[0]{_value} } ) );

my $meta_Int = ::DISPATCH( $::Class, 'new', "Int");
$::Int = $meta_Int->PROTOTYPE();
$meta_Int->add_parent($meta_Value);
$meta_Int->add_method( 'perl',
    ::DISPATCH( $::Method, 'new',  sub { my $v = $::Str->new( $_[0]{_value} ) } ) );
$meta_Int->add_method( 'str',
    ::DISPATCH( $::Method, 'new',  sub { my $v = $::Str->new( $_[0]{_value} ) } ) );
$meta_Int->add_method( 'true',
    ::DISPATCH( $::Method, 'new',  sub { $::Bit->new( $_[0]{_value} == 0 ? 0 : 1 ) } ) );
# $meta_Int->add_method( 'p5landish', ::DISPATCH( $::Method, 'new',  sub { $_[0]{_value} } ) );

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
# $meta_Bit->add_method( 'p5landish', ::DISPATCH( $::Method, 'new',  sub { $_[0]{_value} } ) );

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
                #print "Now testing object: $obj->{_value}\n";
                # $obj = eval '$::' . $obj->{_value};
                # print ::DISPATCH( $obj, "str" )->{_value},"\n";
                
                # the call to .str is needed in order to stringify the ::Str prototype
                $obj = eval '$::' . ::DISPATCH( $obj, "str" )->{_value};
            }

            if ( ::DISPATCH( $obj, 'isa', 
                    ::DISPATCH( $::Str, 'new', 'Subset' ) 
                 )->{_value} 
            ) {
                # Subset
                #print "Testing Subset\n";
                #print "type: @{[ %{$obj->{_value}} ]} \n";
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

                #return ::DISPATCH( $::Bit, 'new', 
                #    ::DISPATCH( $block, 'APPLY', $self )
                #);
            }
            #print "Testing not-Subset\n";
            $obj = ::DISPATCH( $obj, 'str' );   # Proto or Str  XXX
            #print "Class == ", $obj->{_value},"\n";
            return ::DISPATCH( $::Bit, 'new', 1 )
                if exists( $self->{_roles}{ $obj->{_value} } );
            return ::DISPATCH( $self, 'isa', $obj );
        }
    )
);

# implement Object.str
$meta_Object->add_method(
    'str',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $v = ::DISPATCH( $::Str, 'new', 
                '::' . $_[0]{_isa}[0]{_value}{class_name} . '(...)' );
        }
    )
);

# implement Object.int
$meta_Object->add_method(
    'int',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $v = ::DISPATCH( $::Int, 'new',  0 + $_[0]{_value} );  # ???
        }
    )
);

# implement Object.defined
$meta_Object->add_method(
    'defined',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $v = ::DISPATCH( $::Bit, 'new', ( defined $_[0]{_value} ? 1 : 0 ) );
        }
    )
);

# Object.FETCH is a no-op
# $meta_Object->add_method( 'FETCH',        ::DISPATCH( $::Method, 'new',  sub { $_[0] } ) );
# Object.STORE is forbidden
my $method_readonly = ::DISPATCH( $::Method, 'new', 
    sub {
        die "attempt to modify a read-only value";
    }
);
$meta_Object->add_method( 'STORE', $method_readonly );

#--- back to Value

my $meta_Undef = ::DISPATCH( $::Class, 'new', "Undef");
$::Undef = $meta_Undef->PROTOTYPE();
$meta_Undef->add_parent($meta_Value);
$meta_Undef->add_method( 'perl',
    ::DISPATCH( $::Method, 'new',  sub { my $v = $::Str->new('undef') } ) );
$meta_Undef->add_method( 'str',
    ::DISPATCH( $::Method, 'new',  sub { my $v = $::Str->new('') } ) );

my $meta_Code = ::DISPATCH( $::Class, 'new', "Code");
$::Code = $meta_Code->PROTOTYPE();
$meta_Code->add_parent($meta_Value);
$meta_Code->add_method( 'perl',
    ::DISPATCH( $::Method, 'new',  sub { my $v = $::Str->new( $_[0]{_value}{src} ) } ) );
$meta_Code->add_method( 'APPLY',
    ::DISPATCH( $::Method, 'new',  sub { my $self = shift; $self->{_value}{code}->(@_) } ) );


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

my $meta_Container = ::DISPATCH( $::Class, 'new', "Container");
$::Container = $meta_Container->PROTOTYPE();
$meta_Container->add_method(
    'FETCH',
    ::DISPATCH( $::Method, 'new', 
        sub {
            $_[0]{_value}{cell} ? $_[0]{_value}{cell} : $GLOBAL::undef;
        }
    )
);
$meta_Container->add_method(
    'STORE',
    ::DISPATCH( $::Method, 'new', 
        sub {
            die "attempt to modify a read-only value"
              if $_[0]{_roles}{readonly};
            $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
            $_[0]{_value}{cell} = $_[1];
        }
    )
);
$meta_Container->add_method(
    'BIND',
    ::DISPATCH( $::Method, 'new', 
        sub {
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
        }
    )
);

my $meta_Scalar = ::DISPATCH( $::Class, 'new', "Scalar");
$::Scalar = $meta_Scalar->PROTOTYPE();
$meta_Scalar->add_parent($meta_Container);
$meta_Scalar->add_method(
    'new',
    ::DISPATCH( $::Method, 'new', 
        sub {
            my $v = {
                %{ $_[0] },
                _value => $_[1],    # { %{$_[1]}, cell => undef },
                _roles        => { 'container' => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $dispatch_VAR,
            };
        },
    )
);

my $meta_Routine = ::DISPATCH( $::Class, 'new', "Routine");
$::Routine = $meta_Routine->PROTOTYPE();
$meta_Routine->add_parent($meta_Container);
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
                _dispatch_VAR => $dispatch_VAR,
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


# tests is a variable was initialized at all
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


require KindaPerl6::Runtime::Perl6::Pair;

=for 'Pair' bootstrap - do not delete until the image gets more stable!
    my $meta_Pair = ::DISPATCH( $::Class, 'new', "Pair");
    $::Pair = $meta_Pair->PROTOTYPE();
    $meta_Pair->add_parent($meta_Value);
    $meta_Pair->add_attribute( 'key' ); 
    $meta_Pair->add_attribute( 'value' );  
    $meta_Pair->add_method( 'true',
        ::DISPATCH( $::Method, 'new',  sub { $::Bit->new( 1 ) } ) );
=cut


sub make_class {
    my %args = @_;
    my $meta = ::DISPATCH( $::Class, 'new', $args{name});
    my %methods = %{$args{methods}};
    while (my ($method_name,$sub) = each %methods) {
        ::DISPATCH($meta,"add_method",$method_name,::DISPATCH( $::Method, 'new', $sub));
    }
    for my $attribute_name (@{$args{attributes}}) {
        ::DISPATCH($meta,"add_attribute",$attribute_name);
    }
    for my $parent (@{$args{parents}}) {
        ::DISPATCH($meta,"add_parent",$parent);
    }
    return ::DISPATCH($meta,"PROTOTYPE");
}

my $Hash_Cell = make_class(name=>"HashCell",parent=>[$meta_Container],methods=>{
        new=>sub {
            my $v = {
                %{ $_[0] },
                _value => $_[1],
                _roles        => { 'container' => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $dispatch_VAR,
            };
        },
        STORE=>sub {
           ${$_[0]{_value}{cell}} = $_[1];
        },
        FETCH=>sub {
            use Data::Dump::Streamer;
            return ${$_[0]{_value}{cell}};
        },
});

$::Hash = make_class(name=>"Hash",parent=>[$meta_Value],methods=>{
    LOOKUP=>sub {
            $_[0]{_value}{_hash} ||= {};
            my $key = ::DISPATCH(::DISPATCH($_[1],"str"),"p5landish");
            return ::DISPATCH($Hash_Cell,"new",{cell=>\$_[0]{_value}{_hash}{$key}});
        },
    elems => sub {
             $_[0]{_value}{_hash} ||= {};
             return ::DISPATCH($::Int,"new",scalar(keys(%{$_[0]{_value}{_hash}})));
        },
    pairs => sub {
             $_[0]{_value}{_hash} ||= {};
             return 
                ::DISPATCH( $::Array, 'new', 
                    { _array => [
                          map {
                                ::DISPATCH( $::Pair, 'new', {
                                        key   => $_,
                                        value => $_[0]{_value}{_hash}{$_},
                                    } 
                                )
                            } 
                            keys %{ $_[0]{_value}{_hash} }
                        ],
                    }
                );
        },
    keys => sub {
             $_[0]{_value}{_hash} ||= {};
             return 
                ::DISPATCH( $::Array, 'new', 
                    { _array => [
                          map {
                                # XXX str keys only
                                ::DISPATCH( $::Str, 'new', $_ )
                            } 
                            keys %{ $_[0]{_value}{_hash} }
                        ],
                    }
                );
        },
    values => sub {
             $_[0]{_value}{_hash} ||= {};
             return 
                ::DISPATCH( $::Array, 'new', 
                    { _array => [
                          map {
                                $_[0]{_value}{_hash}{$_}
                            } 
                            keys %{ $_[0]{_value}{_hash} }
                        ],
                    }
                );
        },
});

require KindaPerl6::Runtime::Perl6::Hash;

$::Array = make_class(name=>"Array",parent=>[$meta_Value],methods=>{
    INDEX=>sub {
             $_[0]{_value}{_array} ||= [];
             my $key = ::DISPATCH(::DISPATCH($_[1],"int"),"p5landish");
             return ::DISPATCH($Hash_Cell,"new",{cell=>\$_[0]{_value}{_array}[$key]});
        },
    elems =>sub {
             $_[0]{_value}{_array} ||= [];
             return ::DISPATCH($::Int, "new", scalar @{ $_[0]{_value}{_array} } );
        },
    join =>sub {
            $_[0]{_value}{_array} ||= [];
            return ::DISPATCH($::Str, "new", join( 
                ::DISPATCH(::DISPATCH($_[1],"str"),"p5landish"), 
                map {
                    ::DISPATCH(::DISPATCH($_,"str"),"p5landish")
                    }
                    @{ $_[0]{_value}{_array} } 
            ) );
        },
});

require KindaPerl6::Runtime::Perl6::Array;


1;

