
use v5;

# my $meth = ::CALL( $::Method, 'new', sub { 'hi' } );
# my $obj = ::CALL( $::Object, 'new', $candidate );

use Data::Dumper;

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

sub ::CALL { 
        # $method_name is unboxed
        my ($self, $method_name) = (shift, shift);
        #print "lookup $method_name in $self\n";
        #print Dumper($self);
        if ( ! defined $self->{_value} ) {
            # 'self' is a prototype object
            # it stringifies to the class name
            #print "Class.str: ",$self->{_isa}[0]{_value}{class_name},"\n";
            return ::CALL( $::Str, 'new', $self->{_isa}[0]{_value}{class_name} )
                if $method_name eq 'str'; 
        }
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
        die "no method: $method_name\n";
}   

%::PROTO = ( 
    _methods  => undef, # hash
    _roles    => undef,
    _modified => undef,
    _name     => '',
    _value    => undef, # whatever
    _isa      => undef, # array
);

#--- Method

my $method_new = {
    %::PROTO,
    _name     => '$method_new',
    _value    => sub { 
                #print "Calling new from @{[ caller ]} \n";
                my $v = { 
                    %{$_[0]},
                    _value => $_[1], # || 0,
                    _name  => '',
                } },
};

my $meta_Method = {
    %::PROTO,
    _name     => '$meta_Method',
    _value    => {
        methods => {
            new => $method_new
        },
        class_name => 'Method',
    },
};
$::Method = {
    %::PROTO,
    _name     => '$::Method',
    _isa      => [ $meta_Method ],
};
push @{$method_new->{_isa}}, $meta_Method;
$meta_Method->{_value}{methods}{WHAT}   = ::CALL( $::Method, 'new', sub { $::Method } );
$meta_Method->{_value}{methods}{HOW}    = ::CALL( $::Method, 'new', sub { $meta_Method } );

#--- Object

        # my $meta_Object;
        $meta_Object = {
            %::PROTO,
            _name     => $_[3],
            _value    => {
                class_name => 'Object',
            },
        };
        $meta_Object->{_value}{methods}{WHAT}   = ::CALL( $::Method, 'new', sub { $::Object } );
        $meta_Object->{_value}{methods}{HOW}    = ::CALL( $::Method, 'new', sub { $meta_Object } );
        $meta_Object->{_value}{methods}{new}    = $method_new;
        $::Object = {
            %::PROTO,
            _name     => '$::Object',
            #_isa      => [ $meta_Object ],
        };

#--- Class

my $meta_Class = {
    %::PROTO,
    _name     => '$meta_Class',
    _value    => {
        methods    => {},
        class_name => 'Class',
    }, 
};
push @{$meta_Class->{_isa}}, $meta_Class;
$meta_Class->{_value}{methods}{add_method} = ::CALL( $::Method, 'new',
    sub {
        warn "redefining method $_[0]{_value}{class_name}.$_[1]"
            if exists $_[0]{_value}{methods}{$_[1]};
        $_[0]{_value}{methods}{$_[1]} = $_[2];
    }
);
::CALL( $meta_Class, 'add_method', 'redefine_method', ::CALL( $::Method, 'new', 
    sub {
        $_[0]{_value}{methods}{$_[1]} = $_[2];
    }
) );
::CALL( $meta_Class, 'add_method', 'WHAT', ::CALL( $::Method, 'new', sub { $::Class } ) );
::CALL( $meta_Class, 'add_method', 'HOW',  ::CALL( $::Method, 'new', sub { $meta_Class } ) );
::CALL( $meta_Class, 'add_method', 'add_parent',  ::CALL( $::Method, 'new', 
    sub { push @{$_[0]{_value}{isa}}, $_[1] } ) );
::CALL( $meta_Class, 'add_method', 'new',  ::CALL( $::Method, 'new', 
    sub { 
        #print "Calling Class.new from @{[ caller ]} \n";
        # new Class( $prototype_container, $prototype_container_name, $meta_container, $meta_container_name, $class_name )
 
        my $meta_class = $_[0];

        my $class_name = ref($_[1]) ? $_[1]{_value} : $_[1];

        #print "Creating Class: $class_name\n";
        #print Dumper(\@_);
        my $self_meta;
        my $self;

        $self_meta = {
            %::PROTO,
            _name     => '$self_meta',
            _value    => {
                #isa => [ $meta_Object ],
                class_name => $class_name,
            },
            _isa      => [ $meta_Class ],
        };
        $self = {
            %::PROTO,
            _name     => '$self',
            _isa      => [ $self_meta ],
        };
        $self_meta->{_value}{methods}{WHAT}   = ::CALL( $::Method, 'new', 
            sub { 
                #print "WHAT: ",Dumper($self->{_value});
                #print "WHAT: ", $self->{_isa}[0]{_value}{class_name}, "\n";
                $self;      
            } );
        $self_meta->{_value}{methods}{HOW}    = ::CALL( $::Method, 'new', sub { $self_meta } );
        ${"::$class_name"} = $self 
            if $class_name;
        $self;  # return the prototype
    } ) );
$::Class = {
    %::PROTO,
    _name     => '$::Class',
    _isa      => [ $meta_Class ],
};
#print "CLASS = ",Dumper($meta_Class);


push @{$meta_Method->{_isa}}, $meta_Class;
push @{$meta_Object->{_isa}}, $meta_Class;
#push @{$meta_Class->{_isa}}, $meta_Object;


#--- Values

::CALL( $::Class, 'new', 'Value' );  # $::Value, '$::Value',    $meta_Value, '$meta_Value',    'Value');
my $meta_Value = ::CALL( $::Value, 'HOW' );
::CALL( $meta_Value, 'add_method', 'IS_ARRAY',     ::CALL( $::Method, 'new', sub { 0 } ) );
::CALL( $meta_Value, 'add_method', 'IS_HASH',      ::CALL( $::Method, 'new', sub { 0 } ) );
::CALL( $meta_Value, 'add_method', 'IS_CONTAINER', ::CALL( $::Method, 'new', sub { 0 } ) );
# -- FETCH is implemented in Object
# ::CALL( $meta_Value, 'add_method', 'FETCH',        ::CALL( $::Method, 'new', sub { $_[0] } ) );

::CALL( $::Class, 'new', 'Str' );  #   $::Str, '$::Str',    $meta_Str, '$meta_Str',    'Str');
my $meta_Str = ::CALL( $::Str, 'HOW' );
::CALL( $meta_Str, 'add_parent', $meta_Value );
::CALL( $meta_Str, 'add_method', 'perl',           ::CALL( $::Method, 'new', 
    sub { my $v = ::CALL( $::Str, 'new', '\'' . $_[0]{_value} . '\'' ) } ) );
::CALL( $meta_Str, 'add_method', 'str',            ::CALL( $::Method, 'new',
    sub { $_[0] } ) );

::CALL( $::Class, 'new',  'Int' );  #  $::Int, '$::Int',    $meta_Int, '$meta_Int',    'Int');
my $meta_Int = ::CALL( $::Int, 'HOW' );
::CALL( $meta_Int, 'add_parent', $meta_Value );
::CALL( $meta_Int, 'add_method', 'perl',           ::CALL( $::Method, 'new', 
    sub { my $v = ::CALL( $::Str, 'new', $_[0]{_value} ) } ) );
::CALL( $meta_Int, 'add_method', 'str',            ::CALL( $::Method, 'new', 
    sub { my $v = ::CALL( $::Str, 'new', $_[0]{_value} ) } ) );


#--- finish Object

# implement Object.str 
::CALL( $meta_Object, 'add_method', 'str',         ::CALL( $::Method, 'new',
    sub { 
        my $v = ::CALL( $::Str, 'new', '::' . $_[0]{_isa}[0]{_value}{class_name} .'(...)' );
    } ) );
# implement Object.int 
::CALL( $meta_Object, 'add_method', 'int',         ::CALL( $::Method, 'new',
    sub { 
        # XXX
        my $v = ::CALL( $::Int, 'new', 0 + $_[0]{_value} );
    } ) );
# Object.FETCH is a no-op
::CALL( $meta_Object, 'add_method', 'FETCH',        ::CALL( $::Method, 'new', sub { $_[0] } ) );
# Object.STORE is forbidden
my $method_readonly = ::CALL( $::Method, 'new',
    sub { 
        die "attempt to modify a read-only value"; 
    } 
);
::CALL( $meta_Object, 'add_method', 'STORE',     $method_readonly );


#--- back to Value

::CALL( $::Class, 'new', 'Undef' );   #   $::Undef, '$::Undef',    $meta_Undef, '$meta_Undef',    'Undef');
my $meta_Undef = ::CALL( $::Undef, 'HOW' );
::CALL( $meta_Undef, 'add_parent', $meta_Value );  
::CALL( $meta_Undef, 'add_method', 'perl',         ::CALL( $::Method, 'new', 
    sub { my $v = ::CALL( $::Str, 'new', 'undef' ) } ) );
::CALL( $meta_Undef, 'add_method', 'str',         ::CALL( $::Method, 'new', 
    sub { my $v = ::CALL( $::Str, 'new', '' ) } ) );

::CALL( $::Class, 'new',  'Bit' ); 
my $meta_Bit = ::CALL( $::Bit, 'HOW' );
::CALL( $meta_Bit, 'add_parent', $meta_Value );
::CALL( $meta_Bit, 'add_method', 'perl',           ::CALL( $::Method, 'new', 
    sub { my $v = ::CALL( $::Str, 'new', $_[0]{_value} ) } ) );

::CALL( $::Class, 'new', 'Code' ); 
my $meta_Code = ::CALL( $::Code, 'HOW' );
::CALL( $meta_Code, 'add_parent', $meta_Value );
::CALL( $meta_Code, 'add_method', 'perl',           ::CALL( $::Method, 'new', 
    sub { my $v = ::CALL( $::Str, 'new', $_[0]{_value}{src} ) } ) );
::CALL( $meta_Code, 'add_method', 'APPLY',           ::CALL( $::Method, 'new', 
    sub { my $self = shift; $self->{_value}{code}->( @_ ) } ) );

#--- Containers

::CALL( $::Class, 'new', 'Container' );
my $meta_Container = ::CALL( $::Container, 'HOW' );
::CALL( $meta_Container, 'add_method', 'IS_ARRAY',     ::CALL( $::Method, 'new', sub { 0 } ) );
::CALL( $meta_Container, 'add_method', 'IS_HASH',      ::CALL( $::Method, 'new', sub { 0 } ) );
::CALL( $meta_Container, 'add_method', 'IS_CONTAINER', ::CALL( $::Method, 'new', sub { 1 } ) );
::CALL( $meta_Container, 'add_method', 'FETCH',        ::CALL( $::Method, 'new', 
    sub { 
        #print "Container FETCH: $_[0]{_value}{cell}{_isa}[0]{_value}{class_name}\n";
        #print Dumper( $_[0]{_value}{cell} );
        $_[0]{_value}{cell} ? $_[0]{_value}{cell} : $GLOBAL::undef; 
    } 
) );
::CALL( $meta_Container, 'add_method', 'STORE',        ::CALL( $::Method, 'new', 
    sub { 
        #print "Container STORE: $_[1]{_isa}[0]{_value}{cell}{class_name}\n";
        $_[0]{_value}{cell} = $_[1]; 
    } 
) );
::CALL( $meta_Container, 'add_method', 'BIND',        ::CALL( $::Method, 'new', 
    sub { 
        #print "Container STORE: $_[1]{_isa}[0]{_value}{class_name}\n";
        if ( exists $_[1]{_value}{cell} ) {
            # Container := Container
            $_[0]{_value}   = $_[1]{_value}; 
            $_[0]{_methods} = $_[1]{_methods};
            $_[0]{_roles}   = $_[1]{_roles};
        }
        else {
            # Container := Object
            # - add the read-only trait
            $_[0]{_value}{cell} = $_[1]; 
            $_[0]{_methods} = [ $method_readonly ];
            $_[0]{_roles}   = [ ];
        }
    } 
) );

::CALL( $::Class, 'new', 'Scalar' );  
my $meta_Scalar = ::CALL( $::Scalar, 'HOW' );
::CALL( $meta_Scalar, 'add_parent', $meta_Container );

::CALL( $::Class, 'new', 'Routine' );  
my $meta_Routine = ::CALL( $::Routine, 'HOW' );
::CALL( $meta_Routine, 'add_parent', $meta_Container );
::CALL( $meta_Routine, 'add_method', $method_readonly );

#print "Scalar parents:\n";
#for ( @{ $meta_Scalar->{_value}{isa} } ) {
#    print " ",$_->{_value}{class_name},"\n";
#}

1;

