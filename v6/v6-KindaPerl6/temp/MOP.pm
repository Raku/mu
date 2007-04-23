
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
            #print "trying $parent ",$parent->{_class_name},"\n", Dumper($parent);
            #print "available $method_name ? @{[ keys %{$parent->{_value}{methods}} ]}\n";
            my $m = get_method_from_metaclass( $parent, $method_name );
            return $m 
                if $m;
        }
        return undef;
}

sub ::CALL { 
        # $method_name is unboxed
        my ($self, $method_name) = (shift, shift);
        #print "lookup $method_name in $self\n";
        print Dumper($self);
        if ( ! defined $self->{_value} ) {
            # 'self' is a prototype object
            # it stringifies to the class name
            return $self->{_class_name} if $method_name eq 'str'; 
        }
        # lookup local methods
        return $self->{_methods}{$method_name}{_value}->( $self, @_ )
            if exists $self->{_methods}{$method_name};
        # lookup method in the metaclass
        for my $parent ( @{$self->{_isa}} ) {
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
    _class_name => '',
);

#--- Method

my $method_new = {
    %::PROTO,
    _name     => '$method_new',
    _value    => sub { 
                print "Calling new from @{[ caller ]} \n";
                my $v = { 
                    %{$_[0]},
                    _value => $_[1],
                    _name  => '',
                } },
    _class_name => 'Method',
};

my $meta_Method = {
    %::PROTO,
    _name     => '$meta_Method',
    _value    => {
        methods => {
            new => $method_new
        },
    },
    _class_name => 'Class',
};
$::Method = {
    %::PROTO,
    _name     => '$::Method',
    _value    => undef,  # prototype object
    _isa      => [ $meta_Method ],
    _class_name => 'Method',
};
push @{$method_new->{_isa}}, $meta_Method;
$meta_Method->{_value}{methods}{WHAT}   = ::CALL( $::Method, 'new', sub { $::Method } );
$meta_Method->{_value}{methods}{HOW}    = ::CALL( $::Method, 'new', sub { $meta_Method } );

#--- Object

        my $meta_Object = {
            %::PROTO,
            _name     => $_[3],
            _value    => {},
            _class_name => 'Class',
        };
        $meta_Object->{_value}{methods}{WHAT}   = ::CALL( $::Method, 'new', sub { $::Object } );
        $meta_Object->{_value}{methods}{HOW}    = ::CALL( $::Method, 'new', sub { $meta_Object } );
        $meta_Object->{_value}{methods}{new}    = $method_new;
        $::Object = {
            %::PROTO,
            _name     => '$::Object',
            _value    => {},
            _isa      => [ $meta_Object ],
            _class_name => 'Object',
        };

#--- Class

my $meta_Class = {
    %::PROTO,
    _name     => '$meta_Class',
    _value    => {
        methods => {
            new => $method_new
        }
    }, 
    _class_name => 'Class',
};
push @{$meta_Class->{_isa}}, $meta_Class;
$meta_Class->{_value}{methods}{add_method} = ::CALL( $::Method, 'new',
    sub {
        warn "redefining method $_[0]{_class_name}.$_[1]"
            if exists $_[0]{_value}{methods}{$_[1]};
        $_[0]{_value}{methods}{$_[1]} = $_[2];
    }
);
::CALL( $meta_Class, 'add_method', 'WHAT', ::CALL( $::Method, 'new', sub { $::Class } ) );
::CALL( $meta_Class, 'add_method', 'HOW',  ::CALL( $::Method, 'new', sub { $meta_Class } ) );
::CALL( $meta_Class, 'add_method', 'add_parent',  ::CALL( $::Method, 'new', 
    sub { push @{$_[0]{_value}{isa}}, $_[1] } ) );
::CALL( $meta_Class, 'add_method', 'new',  ::CALL( $::Method, 'new', 
    sub { 
        print "Calling Class.new from @{[ caller ]} \n";
        # new Class( $prototype_container, $prototype_container_name, $meta_container, $meta_container_name, $class_name )
        shift;
        $_[2] = {
            %::PROTO,
            _name     => $_[3],
            _value    => {
                isa => [ $meta_Object ],
            },
            _isa      => [ $meta_Class ],
            _class_name => 'Class',
        };
        $_[2]->{_value}{methods}{WHAT}   = ::CALL( $::Method, 'new', sub { $_[0] } );
        $_[2]->{_value}{methods}{HOW}    = ::CALL( $::Method, 'new', sub { $_[2] } );
        $_[0] = {
            %::PROTO,
            _name     => $_[1],
            _value    => {},
            _isa      => [ $_[2] ],
            _class_name => $_[4],
        };
    } ) );
$::Class = {
    %::PROTO,
    _name     => '$::Class',
    _value    => undef,  # prototype object
    _isa      => [ $meta_Class ],
    _class_name => 'Class',
};
#print "CLASS = ",Dumper($meta_Class);


push @{$meta_Method->{_isa}}, $meta_Class;
push @{$meta_Object->{_isa}}, $meta_Class;
push @{$meta_Class->{_isa}}, $meta_Object;


#--- Values

my $meta_Value;
::CALL( $::Class, 'new',    $::Value, '$::Value',    $meta_Value, '$meta_Value',    'Value');
::CALL( $meta_Value, 'add_method', 'IS_ARRAY',     ::CALL( $::Method, 'new', sub { 0 } ) );
::CALL( $meta_Value, 'add_method', 'IS_HASH',      ::CALL( $::Method, 'new', sub { 0 } ) );
::CALL( $meta_Value, 'add_method', 'IS_CONTAINER', ::CALL( $::Method, 'new', sub { 0 } ) );
::CALL( $meta_Value, 'add_method', 'FETCH',        ::CALL( $::Method, 'new', sub { $_[0] } ) );


my $meta_Undef;
::CALL( $::Class, 'new',    $::Undef, '$::Undef',    $meta_Undef, '$meta_Undef',    'Undef');
::CALL( $meta_Undef, 'add_parent', $meta_Value );  
::CALL( $meta_Undef, 'add_method', 'perl',         ::CALL( $::Method, 'new', 
    sub { my $v = { %{$_[0]}, _value => 'undef' } } ) );

my $meta_Str;
::CALL( $::Class, 'new',    $::Str, '$::Str',    $meta_Str, '$meta_Str',    'Str');
::CALL( $meta_Str, 'add_parent', $meta_Value );
::CALL( $meta_Str, 'add_method', 'perl',           ::CALL( $::Method, 'new', 
    sub { my $v = { %{$_[0]}, _value => '\'' . $_[0]{_value} . '\'' } } ) );

my $meta_Int;
::CALL( $::Class, 'new',    $::Int, '$::Int',    $meta_Int, '$meta_Int',    'Int');
::CALL( $meta_Int, 'add_parent', $meta_Value );
::CALL( $meta_Int, 'add_method', 'perl',           ::CALL( $::Method, 'new', 
    sub { my $v = { %{$_[0]}, _value => $_[0]{_value} } } ) );


#--- Containers

my $meta_Container;
::CALL( $::Class, 'new',    $::Container, '$::Container',    $meta_Container, '$meta_Container',    'Container');
::CALL( $meta_Container, 'add_method', 'IS_ARRAY',     ::CALL( $::Method, 'new', sub { 0 } ) );
::CALL( $meta_Container, 'add_method', 'IS_HASH',      ::CALL( $::Method, 'new', sub { 0 } ) );
::CALL( $meta_Container, 'add_method', 'IS_CONTAINER', ::CALL( $::Method, 'new', sub { 1 } ) );
::CALL( $meta_Container, 'add_method', 'FETCH',        ::CALL( $::Method, 'new', 
    sub { $_[0]{_value} ? $_[0]{_value} : $::Undef } 
) );

my $meta_Scalar;
::CALL( $::Class, 'new',    $::Scalar, '$::Scalar',    $meta_Scalar, '$meta_Scalar',    'Scalar');
::CALL( $meta_Scalar, 'add_parent', $meta_Container );


1;

