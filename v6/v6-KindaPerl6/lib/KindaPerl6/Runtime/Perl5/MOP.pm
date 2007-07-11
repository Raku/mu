
use v5;

# my $meth = $::Method->{_dispatch}( $::Method, 'new', sub { 'hi' } );
# my $obj =  $::Object->{_dispatch}( $::Object, 'new', $candidate );

use Data::Dumper;

{
    package P6opaque;

    my $methods = {};
    my $dispatch = sub {
        # $self, $method
        
    };
    $methods->{new} = sub { my $v = { _dispatch => $dispatch, $_[0]{_value} } };
}

sub get_method_from_metaclass {
        my ($self, $method_name) = (shift, shift);
        #print "looking in $self\n", Dumper($self);
        return $self->{_value}{methods}{$method_name}
            if exists $self->{_value}{methods}{$method_name};
        for my $parent ( @{$self->{_value}{isa}} ) {
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
        my ($self, $method_name) = (shift, shift);
        #print "lookup $method_name in $self\n";

        unless ( ref($self) eq 'HASH' ) {
            warn "internal error: wrong object format";
            print Dumper($self);
            return $::Str->{_dispatch}( $::Str, 'new', 'Error' );
        }

        if ( $self->{_roles}{auto_deref} ) {
            # this object requires FETCH
            my $value = $self->{_dispatch_VAR}( $self, 'FETCH' );
            return $value->{_dispatch}($value,$method_name,@_);

        }

        if ( ! defined $self->{_value} ) {
            # 'self' is a prototype object
            # it stringifies to the class name
            #print "Class.str: ",$self->{_isa}[0]{_value}{class_name},"\n";
            return $::Str->{_dispatch}( $::Str, 'new', $self->{_isa}[0]{_value}{class_name} )
                if $method_name eq 'str'; 
        }
        # lookup local methods
        return $self->{_methods}{$method_name}{_value}->( $self, @_ )
            if exists $self->{_methods}{$method_name};
        # lookup method in the metaclass
        for my $parent ( @{$self->{_isa}}, $meta_Object ) {
            my $m = get_method_from_metaclass( $parent, $method_name );
            #print $m ? "found\n" : "not found\n";
            #print "Method: ",Dumper($m);
            return $m->{_value}->( $self, @_ ) 
                if $m;
        }
        print "in Class: ",$self->{_isa}[0]{_value}{class_name},"\n";
        die "no method: $method_name\n";
};

# backwards compatibility - remove !!!
#sub ::CALL { $dispatch->(@_) }

my $dispatch_VAR = sub {
        # VAR() is just like CALL(), but it doesn't call FETCH
        # $method_name is unboxed
        my ($self, $method_name) = (shift, shift);
        # lookup local methods
        return $self->{_methods}{$method_name}{_value}->( $self, @_ )
            if exists $self->{_methods}{$method_name};
        # lookup method in the metaclass
        for my $parent ( @{$self->{_isa}}, $meta_Object ) {
            my $m = get_method_from_metaclass( $parent, $method_name );
            #print "found\n" if $m;
            return $m->{_value}->( $self, @_ ) 
                if $m;
        }
        die "no VAR() method: $method_name\n";
};

%::PROTO = ( 
    _methods  => undef, # hash
    _roles    => undef,
    # _modified => undef,
    # _name     => '',
    _value    => undef, # whatever
    _isa      => undef, # array
    _dispatch => $dispatch,
);

#--- Method

my $method_new = {
    %::PROTO,
    # _name     => '$method_new',
    _value    => sub { 
                #print "Calling new from @{[ caller ]} \n";
                my $v = { 
                    %{$_[0]},
                    _value => $_[1], # || 0,
                    # _name  => '',
                } },
};

my $meta_Method = {
    %::PROTO,
    # _name     => '$meta_Method',
    _value    => {
        methods => {
            new => $method_new
        },
        class_name => 'Method',
    },
};
$::Method = {
    %::PROTO,
    # _name     => '$::Method',
    _isa      => [ $meta_Method ],
};
push @{$method_new->{_isa}}, $meta_Method;
$meta_Method->{_value}{methods}{WHAT}   = $::Method->{_dispatch}( $::Method, 'new', sub { $::Method } );
$meta_Method->{_value}{methods}{HOW}    = $::Method->{_dispatch}( $::Method, 'new', sub { $meta_Method } );

#--- Object

        # my $meta_Object;
        $meta_Object = {
            %::PROTO,
            # _name     => $_[3],
            _value    => {
                class_name => 'Object',
            },
        };
        $meta_Object->{_value}{methods}{WHAT}   = $::Method->{_dispatch}( $::Method, 'new', sub { $::Object } );
        $meta_Object->{_value}{methods}{HOW}    = $::Method->{_dispatch}( $::Method, 'new', sub { $meta_Object } );
        $meta_Object->{_value}{methods}{new}    = $method_new;
        $::Object = {
            %::PROTO,
            # _name     => '$::Object',
            #_isa      => [ $meta_Object ],
        };

#--- Class

my $meta_Class = {
    %::PROTO,
    # _name     => '$meta_Class',
    _value    => {
        methods    => {},
        class_name => 'Class',
    }, 
};
push @{$meta_Class->{_isa}}, $meta_Class;
$meta_Class->{_value}{methods}{add_method} = $::Method->{_dispatch}( $::Method, 'new',
    sub {
        my $meth_name = ref($_[1]) ? $_[1]{_value} : $_[1];
        warn "redefining method $_[0]{_value}{class_name}.$meth_name"
            if exists $_[0]{_value}{methods}{$meth_name};
        $_[0]{_value}{methods}{$meth_name} = $_[2];
    }
);
$meta_Class->{_dispatch}( $meta_Class, 'add_method', 'redefine_method', $::Method->{_dispatch}( $::Method, 'new', 
    sub {
        my $meth_name = ref($_[1]) ? $_[1]{_value} : $_[1];
        $_[0]{_value}{methods}{$meth_name} = $_[2];
    }
) );
$meta_Class->{_dispatch}( $meta_Class, 'add_method', 'WHAT', $::Method->{_dispatch}( $::Method, 'new', sub { $::Class } ) );
$meta_Class->{_dispatch}( $meta_Class, 'add_method', 'HOW',  $::Method->{_dispatch}( $::Method, 'new', sub { $meta_Class } ) );
$meta_Class->{_dispatch}( $meta_Class, 'add_method', 'add_parent',  $::Method->{_dispatch}( $::Method, 'new', 
    sub { push @{$_[0]{_value}{isa}}, $_[1] } ) );
$meta_Class->{_dispatch}( $meta_Class, 'add_method', 'new',  $::Method->{_dispatch}( $::Method, 'new', 
    sub { 
        #print "Calling Class.new from @{[ caller ]} \n";
        # new Class( $prototype_container, $prototype_container_name, $meta_container, $meta_container_name, $class_name )
 
        my $meta_class = $_[0];

        my $class_name = ref($_[1]) ? $_[1]{_value} : $_[1];

        #print "Creating Class: $class_name\n";
        #print Dumper(\@_);
        my $self_meta;
        my $self;
        my $class_prototype;

        $self_meta = {
            %::PROTO,
            # _name     => '$self_meta',
            _value    => {
                #isa => [ $meta_Object ],
                class_name => $class_name,
            },
            _isa      => [ $meta_Class ],
        };
        $self = {
            %::PROTO,
            # _name     => '$self',
            _isa      => [ $self_meta ],
        };
        $self_meta->{_value}{methods}{WHAT}   = $::Method->{_dispatch}( $::Method, 'new', 
            sub { 
                #print "WHAT: ",Dumper($self->{_value});
                #print "WHAT: ", $self->{_isa}[0]{_value}{class_name}, "\n";
                $self;      
            } );
        $self_meta->{_value}{methods}{HOW}    = $::Method->{_dispatch}( $::Method, 'new', sub { $self_meta } );

        # ???
        # create the "prototype", which is an 'empty' instance 
        $class_prototype = $self->{_dispatch}( $self, 'new', () ); 
        
        ${"::$class_name"} = $class_prototype
            if $class_name;
        $class_prototype;  # return the prototype
    } ) );
$::Class = {
    %::PROTO,
    # _name     => '$::Class',
    _isa      => [ $meta_Class ],
};
#print "CLASS = ",Dumper($meta_Class);


push @{$meta_Method->{_isa}}, $meta_Class;
push @{$meta_Object->{_isa}}, $meta_Class;
#push @{$meta_Class->{_isa}}, $meta_Object;


#--- Roles

$::Class->{_dispatch}( $::Class, 'new', 'Role' );
my $meta_Role = $::Role->{_dispatch}( $::Role, 'HOW' );
# copy Class methods
$meta_Role->{_value}{methods} = { %{ $meta_Class->{_value}{methods} } };


#--- Values

$::Class->{_dispatch}( $::Class, 'new', 'Value' );  
my $meta_Value = $::Value->{_dispatch}( $::Value, 'HOW' );
# $meta_Value->{_dispatch}( $meta_Value, 'add_method', 'IS_ARRAY',     $::Method->{_dispatch}( $::Method, 'new', sub { 0 } ) );
# $meta_Value->{_dispatch}( $meta_Value, 'add_method', 'IS_HASH',      $::Method->{_dispatch}( $::Method, 'new', sub { 0 } ) );
# $meta_Value->{_dispatch}( $meta_Value, 'add_method', 'IS_CONTAINER', $::Method->{_dispatch}( $::Method, 'new', sub { 0 } ) );
# -- FETCH is implemented in Object
# $meta_Value->{_dispatch}( $meta_Value, 'add_method', 'FETCH',        $::Method->{_dispatch}( $::Method, 'new', sub { $_[0] } ) );

$::Class->{_dispatch}( $::Class, 'new', 'Str' );  #   $::Str, '$::Str',    $meta_Str, '$meta_Str',    'Str');
my $meta_Str = $::Str->{_dispatch}( $::Str, 'HOW' );
$meta_Str->{_dispatch}( $meta_Str, 'add_parent', $meta_Value );
$meta_Str->{_dispatch}( $meta_Str, 'add_method', 'perl',           $::Method->{_dispatch}( $::Method, 'new', 
    sub { my $v = $::Str->{_dispatch}( $::Str, 'new', '\'' . $_[0]{_value} . '\'' ) } ) );
$meta_Str->{_dispatch}( $meta_Str, 'add_method', 'str',            $::Method->{_dispatch}( $::Method, 'new',
    sub { $_[0] } ) );
$meta_Str->{_dispatch}( $meta_Str, 'add_method', 'true',           $::Method->{_dispatch}( $::Method, 'new',
    sub { $::Bit->{_dispatch}( $::Bit, 'new', ( $_[0]{_value} ne '' && $_[0]{_value} ne '0' ) ? 1 : 0 ) } ) );
$meta_Str->{_dispatch}( $meta_Str, 'add_method', 'p5landish', $::Method->{_dispatch}( $::Method, 'new',sub {$_[0]{_value}}));

$::Class->{_dispatch}( $::Class, 'new',  'Int' );  
my $meta_Int = $::Int->{_dispatch}( $::Int, 'HOW' );
$meta_Int->{_dispatch}( $meta_Int, 'add_parent', $meta_Value );
$meta_Int->{_dispatch}( $meta_Int, 'add_method', 'perl',           $::Method->{_dispatch}( $::Method, 'new', 
    sub { my $v = $::Str->{_dispatch}( $::Str, 'new', $_[0]{_value} ) } ) );
$meta_Int->{_dispatch}( $meta_Int, 'add_method', 'str',            $::Method->{_dispatch}( $::Method, 'new', 
    sub { my $v = $::Str->{_dispatch}( $::Str, 'new', $_[0]{_value} ) } ) );
$meta_Int->{_dispatch}( $meta_Int, 'add_method', 'true',           $::Method->{_dispatch}( $::Method, 'new',
    sub { $::Bit->{_dispatch}( $::Bit, 'new', $_[0]{_value} == 0 ? 0 : 1 ) } ) );
$meta_Int->{_dispatch}( $meta_Int, 'add_method', 'p5landish', $::Method->{_dispatch}( $::Method, 'new',sub {$_[0]{_value}}));


#--- finish Object

# implement Object.str 
$meta_Object->{_dispatch}( $meta_Object, 'add_method', 'str',         $::Method->{_dispatch}( $::Method, 'new',
    sub { 
        my $v = $::Str->{_dispatch}( $::Str, 'new', '::' . $_[0]{_isa}[0]{_value}{class_name} .'(...)' );
    } ) );
# implement Object.int 
$meta_Object->{_dispatch}( $meta_Object, 'add_method', 'int',         $::Method->{_dispatch}( $::Method, 'new',
    sub { 
        # XXX
        my $v = $::Int->{_dispatch}( $::Int, 'new', 0 + $_[0]{_value} );
    } ) );
# Object.FETCH is a no-op
# $meta_Object->{_dispatch}( $meta_Object, 'add_method', 'FETCH',        $::Method->{_dispatch}( $::Method, 'new', sub { $_[0] } ) );
# Object.STORE is forbidden
my $method_readonly = $::Method->{_dispatch}( $::Method, 'new',
    sub { 
        die "attempt to modify a read-only value"; 
    } 
);
$meta_Object->{_dispatch}( $meta_Object, 'add_method', 'STORE',     $method_readonly );


#--- back to Value

$::Class->{_dispatch}( $::Class, 'new', 'Undef' );   #   $::Undef, '$::Undef',    $meta_Undef, '$meta_Undef',    'Undef');
my $meta_Undef = $::Undef->{_dispatch}( $::Undef, 'HOW' );
$meta_Undef->{_dispatch}( $meta_Undef, 'add_parent', $meta_Value );  
$meta_Undef->{_dispatch}( $meta_Undef, 'add_method', 'perl',         $::Method->{_dispatch}( $::Method, 'new', 
    sub { my $v = $::Str->{_dispatch}( $::Str, 'new', 'undef' ) } ) );
$meta_Undef->{_dispatch}( $meta_Undef, 'add_method', 'str',         $::Method->{_dispatch}( $::Method, 'new', 
    sub { my $v = $::Str->{_dispatch}( $::Str, 'new', '' ) } ) );

$::Class->{_dispatch}( $::Class, 'new',  'Bit' ); 
my $meta_Bit = $::Bit->{_dispatch}( $::Bit, 'HOW' );
$meta_Bit->{_dispatch}( $meta_Bit, 'add_parent', $meta_Value );
$meta_Bit->{_dispatch}( $meta_Bit, 'add_method', 'perl',           $::Method->{_dispatch}( $::Method, 'new', 
    sub { my $v = $::Str->{_dispatch}( $::Str, 'new', $_[0]{_value} ? 'True' : 'False' ) } ) );
$meta_Bit->{_dispatch}( $meta_Bit, 'add_method', 'str',            $::Method->{_dispatch}( $::Method, 'new', 
    sub { my $v = $::Str->{_dispatch}( $::Str, 'new', $_[0]{_value} ) } ) );

$meta_Bit->{_dispatch}( $meta_Bit, 'add_method', 'true',           $::Method->{_dispatch}( $::Method, 'new',
    sub { $::Bit->{_dispatch}( $::Bit, 'new', $_[0]{_value}) } ) );

$::Class->{_dispatch}( $::Class, 'new', 'Code' ); 
my $meta_Code = $::Code->{_dispatch}( $::Code, 'HOW' );
$meta_Code->{_dispatch}( $meta_Code, 'add_parent', $meta_Value );
$meta_Code->{_dispatch}( $meta_Code, 'add_method', 'perl',           $::Method->{_dispatch}( $::Method, 'new', 
    sub { my $v = $::Str->{_dispatch}( $::Str, 'new', $_[0]{_value}{src} ) } ) );
$meta_Code->{_dispatch}( $meta_Code, 'add_method', 'APPLY',           $::Method->{_dispatch}( $::Method, 'new', 
    sub { my $self = shift; $self->{_value}{code}->( @_ ) } ) );

#--- Containers

$::Class->{_dispatch}( $::Class, 'new', 'Container' );
my $meta_Container = $::Container->{_dispatch}( $::Container, 'HOW' );
# $meta_Container->{_dispatch}( $meta_Container, 'add_method', 'IS_ARRAY',     $::Method->{_dispatch}( $::Method, 'new', sub { 0 } ) );
# $meta_Container->{_dispatch}( $meta_Container, 'add_method', 'IS_HASH',      $::Method->{_dispatch}( $::Method, 'new', sub { 0 } ) );
# $meta_Container->{_dispatch}( $meta_Container, 'add_method', 'IS_CONTAINER', $::Method->{_dispatch}( $::Method, 'new', sub { 1 } ) );
$meta_Container->{_dispatch}( $meta_Container, 'add_method', 'FETCH',        $::Method->{_dispatch}( $::Method, 'new', 
    sub { 
        #print "Container FETCH: $_[0]{_value}{cell}{_isa}[0]{_value}{class_name}\n";
        #print Dumper( $_[0]{_value}{cell} );
        $_[0]{_value}{cell} ? $_[0]{_value}{cell} : $GLOBAL::undef; 
    } 
) );
$meta_Container->{_dispatch}( $meta_Container, 'add_method', 'STORE',        $::Method->{_dispatch}( $::Method, 'new', 
    sub { 
        #print "Container STORE: $_[1]{_isa}[0]{_value}{cell}{class_name}\n";
        #print "Container STORE: value = ", Dumper( $_[0]{_value} );

        die "attempt to modify a read-only value" 
            if $_[0]{_roles}{readonly};

        $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
        $_[0]{_value}{cell} = $_[1]; 
    } 
) );
$meta_Container->{_dispatch}( $meta_Container, 'add_method', 'BIND',        $::Method->{_dispatch}( $::Method, 'new', 
    sub { 
        #print "Container BIND: $_[1]{_isa}[0]{_value}{class_name}\n";

        # XXX - see old 'Type.pm'
        $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
        $_[1]{_value}{modified}{ $_[1]{_value}{name} } = 1;

        if ( $_[1]{_roles}{container} ) {
            # Container := Container
            $_[0]{_value}   = $_[1]{_value}; 
            $_[0]{_roles}{readonly} = $_[1]{_roles}{readonly};
        }
        else {
            # Container := Object
            # - add the read-only trait
            $_[0]{_value}{cell} = $_[1]; 
            $_[0]{_roles}{readonly} = 1;
        }
        $_[0];
    } 
) );

$::Class->{_dispatch}( $::Class, 'new', 'Scalar' );  
my $meta_Scalar = $::Scalar->{_dispatch}( $::Scalar, 'HOW' );
$meta_Scalar->{_dispatch}( $meta_Scalar, 'add_parent', $meta_Container );
$meta_Scalar->{_dispatch}( $meta_Scalar, 'add_method', 'new',  $::Method->{_dispatch}( $::Method, 'new',
    sub { 
        my $v = { 
            %{$_[0]},
            _value => $_[1],    # { %{$_[1]}, cell => undef },
            _roles => { 'container' => 1, 'auto_deref' => 1 },
            _dispatch_VAR => $dispatch_VAR,
        } 
    },
) );

$::Class->{_dispatch}( $::Class, 'new', 'Routine' );  
my $meta_Routine = $::Routine->{_dispatch}( $::Routine, 'HOW' );
$meta_Routine->{_dispatch}( $meta_Routine, 'add_parent', $meta_Container );
$meta_Routine->{_dispatch}( $meta_Routine, 'add_method', 'STORE', $method_readonly );
$meta_Routine->{_dispatch}( $meta_Routine, 'add_method', 'APPLY',           $::Method->{_dispatch}( $::Method, 'new', 
    sub { my $self = shift; $self->{_value}{cell}{_value}{code}->( @_ ) } ) );
$meta_Routine->{_dispatch}( $meta_Routine, 'add_method', 'new',  $::Method->{_dispatch}( $::Method, 'new',
    sub { 
        my $v = { 
            %{$_[0]},
            _value => $_[1],    # { cell => undef },
            _roles => { 'container' => 1, 'auto_apply' => 1 },
            _dispatch_VAR => $dispatch_VAR,
        } 
    },
) );
$meta_Routine->{_dispatch}( $meta_Routine, 'add_method', 'perl',  $::Method->{_dispatch}( $::Method, 'new',
    sub { 
        $::Str->{_dispatch}( $::Str, 'new', $_[0]{_value}{cell}{_value}{src} )
    },
) );

#print "Scalar parents:\n";
#for ( @{ $meta_Scalar->{_value}{isa} } ) {
#    print " ",$_->{_value}{class_name},"\n";
#}

1;

