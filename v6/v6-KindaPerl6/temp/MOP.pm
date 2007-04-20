
use v5;

# my $meth = ::CALL( $::Method, 'new', sub { 'hi' } );

# my $obj = ::CALL( $::Object, 'new', $candidate );

=head Synopsis

    # Class

    my $class = KindaPerl6::Class.new( ::Val::Buf('MyClass') );   # the name is '' for anon classes
    $class.HOW.add_method( ::Val::Buf('my_meth'), sub { ... } );
    $class.HOW.add_attribute( ::Val::Buf('my_attr') );

    # Instantiation
    
    my $obj = $class.new( pairs... );

    # Role

    my $role = KindaPerl6::Role.new( ::Val::Buf('MyRole') );   # the name is '' for anon roles
    $role.HOW.add_method( ::Val::Buf('my_meth'), sub { ... } );
    $role.HOW.add_attribute( ::Val::Buf('my_attr') );

    # adding a Role

    $role.add_role_to( $class );
    $role.add_role_to( $obj );
    $role.add_role_to( $role );

    # a Class 'does' a Role
    $class.HOW.add_role( $role );
    # a Class 'is' a parent class
    $class.HOW.add_parent( $class );

=cut

# TODO - $x.HOW should know about the roles that were applied to $x 
use Data::Dumper;

sub ::CALL { 
        # $method_name is unboxed
        my ($self, $method_name) = (shift, shift);
        #print "lookup $method_name in $self\n";

        if ( ! defined $self->{_value} ) {
            # 'self' is a prototype object
            # it stringifies to the class name
            return ref($self) if $method_name eq 'str'; 
        }

        return $self->{_methods}{$method_name}{_value}->( $self, @_ )
            if exists $self->{_methods}{$method_name};
        # do an ISA lookup
        for my $parent ( @{$self->{_isa}} ) {
            #print "trying $parent\n";
            return $parent->{_methods}{$method_name}{_value}->( $self, @_ )
                if exists $parent->{_methods}{$method_name};
        }
        die "no method: $method_name\n";
}   


$::Method = bless {
    _methods => {
        new => bless {
                %{$_[0]},
                _value => sub { 
                        bless { 
                                %{$_[0]},
                                _value => $_[1],
                                _name  => '',
                            }, ref($_[0])
                    },
                _name  => '',
            }, 'Method',
    },
    _roles => {
    },
    _modified => {},
    _name  => '$::Method',
    _value => undef,  # prototype object
    _isa   => [ ],
}, 'Method';
$::Method->{_methods}{WHAT} = ::CALL( $::Method, 'new', sub { $::Method } );

$::Object = bless {
    _methods  => {},
    _roles    => {},
    _modified => {},
    _name     => '$::Object',
    _value    => undef,
    _isa   => [ ],
}, 'Object';

push @{$::Method->{_isa}}, $::Object;

$::Object->{_methods}{WHAT} = ::CALL( $::Method, 'new', sub { $::Object } );
$::Object->{_methods}{new} = ::CALL( $::Method, 'new', 
        sub { 
            bless { 
                %{$_[0]},
                _value => $_[1],
                _name  => '',
            }, ref($_[0])
        },
    );

$::Class = bless {
    _methods  => {},
    _roles    => {},
    _modified => {},
    _name     => '$::Class',
    _value    => undef,
    _isa   => [ $::Object ],
}, 'Class';
$::Class->{_methods}{WHAT} = ::CALL( $::Method, 'new', sub { $::Class } );

$::Object->{_methods}{HOW} = ::CALL( $::Method, 'new', 
        sub { 
            ::CALL( $::Class, 'new', $_[0] ) 
        } 
    );

1;
__END__
use KindaPerl6::Perl5::Type;

{
package Class; # virtual
}

{
package Role; # virtual
}

{
package KindaPerl6::Class;

    our @ISA = ( 'Type_Constant', 'Class' );
    our %Classes;   # XXX Class names are global
    
    sub _mangle {
        my $name = shift;
        $name =~ s/ ([^a-zA-Z0-9_:] | (?<!:):(?!:)) / '_'.ord($1).'_' /xge;
        $name;
    }
    sub perl { 
        bless [ 
            'class { ... }' 
        ], 'Type_Constant_Buf' 
    }
    sub HOW  { $_[0] }
    sub new {
        my ( $self, $name ) = @_;
        # ??? Type_...
        my $unboxed_name = $name->FETCH->[0];
        $native_name = _mangle( $name->FETCH->[0] || 'Class_ANON_' . rand );
        #print "Class.create $native_name\n";

        # return the prototype object
        # XXX should be lexical
        return $Classes{ $unboxed_name }
            if exists $Classes{ $unboxed_name };
            
        my $class = $Classes{ $unboxed_name } = bless {
                  class_name => $name,
                  class_native_name => $native_name,
                  methods    => { }, 
                  attributes => { },
                  parents    => { },  # is Class
                  roles      => { },  # does Role
              }, __PACKAGE__;
        $class->add_method(
            bless( [ 
                    'HOW' 
                ], 
                'Type_Constant_Buf' 
            ),
            sub { $class },
        );
        $class->add_method(
            bless( [ 
                    'new' 
                ], 
                'Type_Constant_Buf' 
            ),
            sub { 
                #require Data::Dump::Streamer;
                #print "new: ", Data::Dump::Streamer::Dump( @_ );
                # new() inherits the roles from the class, plus extra roles from the prototype object
                my $self = shift; 
                my %data;
                my $class;
                if ( ref( $self ) ) {   # $prototype->new
                    %data  = %$self;
                    $class = ref( $self ); 
                }
                else {                  # Class->new
                    $class = $self;
                    %data  = ( _role_methods => { } );   # , _roles => { } );
                    # %data  = {
                    #     _roles => $class->HOW->{roles}, ...
                    # };
                }
                while ( @_ ) {
                    my ( $key, $value ) = ( shift, shift );
                    $data{ $key->FETCH->[0] } = $value->FETCH;
                }
                #print "new: ", Data::Dump::Streamer::Dump( X->HOW );
                my $new = bless \%data, $class; 
                # optimize ???
                $_->add_role_to( $new ) 
                    for values %{ $class->HOW->{roles} };
                $new;
            },
        );
        eval "
            push \@${native_name}::ISA, 'Type_Constant';
            \$::Class_$native_name = \$native_name->new()
            " or warn $@;
        #print "# Created proto \$::Class_$native_name = ${'$::Class_' . $native_name}\n"; 
        return $class;
    }
    sub add_method {
        my ( $class, $name, $code ) = @_;
        my $unboxed_name = $name->FETCH->[0];
        my $native_name = _mangle( $name->FETCH->[0] );
        #print "Class.add_method $class->{class_native_name} $native_name\n";
        $class->{methods}{$unboxed_name} = {
            method_name => $name,
            method_native_name => $native_name,
            code        => $code,
        };
        eval "
            package $class->{class_native_name};
            *$native_name = \$code;
            " or warn $@;
    }
    sub add_attribute {
        my ( $class, $name ) = @_;
        my $unboxed_name = $name->FETCH->[0];
        my $native_name = _mangle( $name->FETCH->[0] );
        #print "Class.add_attribute $class->{class_native_name} $native_name\n";
        my $code = sub {
            @_ == 1 ? ( $_[0]->{$unboxed_name} ) : ( $_[0]->{$unboxed_name} = $_[1] ) 
        };
        $class->{attributes}{$unboxed_name} = {
            attribute_name => $name,
            attribute_native_name => $native_name,
            code => $code,
        };
        eval "
            package $class->{class_native_name};
            *$native_name = \$code;
            " or warn $@;
    }
    sub add_parent {
        # implements '$class is $super'
        my ( $class, $super ) = @_;
        my $unboxed_name = eval { $super->HOW->{class_name} } || $super->FETCH->[0];
        my $native_name  = eval { $super->HOW->{class_native_name} } || _mangle( $super->FETCH->[0] );
        my $superclass   = $Classes{ $unboxed_name } || Carp::carp "No class like $unboxed_name\n";
        #print "Class.add_parent $class->{class_native_name} $native_name\n";
        $class->{parents}{$unboxed_name} = $superclass;
        eval "
            push \@$class->{class_native_name}::ISA, '$native_name';
            " or warn $@;
        # inherit the parent's roles ???
        # problem - our conventional methods will not override methods from a parent's role
        $class->{roles} = {
            %{ $superclass->{roles} },
            %{ $class->{roles} },
        };
        $class;
    }
    sub add_role {
        # implements '$class does $role'
        # $class_object->HOW->add_role( $role )
        #   - implements Class.does($role)
        #   - adds roles to the class
        #   - the roles apply to new objects and to subclasses
        # $role.add_role_to( $class_object ) 
        #   - adds roles to the prototype object
        #   - the roles do not apply to subclasses
        #   - but: the roles apply to objects created with $class_object->new()
        # $role.add_role_to( $class_object->HOW ) 
        #   - adds roles to the metaclass
        #   - the roles do not apply to objects of that class
        #   - the roles do not apply to subclasses
        my ( $class, $role ) = @_;
        my $unboxed_name = eval { $role->{role_name} } || $role->FETCH->[0];
        #my $native_name  = eval { $role->{role_native_name} } || _mangle( $role->FETCH->[0] );
        $role = $KindaPerl6::Role::Roles{ $unboxed_name } || Carp::carp "No role like $unboxed_name\n";
        #print "Class.add_parent $class->{class_native_name} $native_name\n";
        $class->{roles}{$unboxed_name} = $role;

        #print "Add role to class: ", Main::Dump( $class );

        # - apply the role to the prototype 'Class' object
        eval "
            \$role->add_role_to( \$::Class_$class->{class_native_name} )
            " or warn $@;

        $class;
    }

    # prototype 'Class' object
    $::Class_KindaPerl6::Class = KindaPerl6::Class->new(
        bless [ '' ], 'Type_Constant_Buf'
    );
    #print "# Created proto \$::Class_KindaPerl6::Class\n"; 

}

{
package KindaPerl6::Role;

    our $class = KindaPerl6::Class->new( 
                bless( [ 
                        'KindaPerl6::Role' 
                    ], 
                    'Type_Constant_Buf' 
                ),
    );
    our @ISA = ( 'Type_Constant', 'Role' );
    our %Roles;   # XXX Role names are global
    
    $class->HOW->add_method(
        bless( [ 
                'perl' 
            ], 
            'Type_Constant_Buf' 
        ),
        sub { 
           bless [ 
               'role { ... }' 
           ], 'Type_Constant_Buf' 
       },
    );

    $class->HOW->add_method(
        bless( [ 
                'new' 
            ], 
            'Type_Constant_Buf' 
        ),
        sub { 
            my ( $self, $name ) = @_;
            # ??? Type_...
            my $unboxed_name = $name->FETCH->[0];
            #$native_name = KindaPerl6::Class::_mangle( $name->FETCH->[0] || 'Role_ANON_' . rand );
            #print "Role.create $native_name\n";
            # return the prototype role object (a role is a singleton ???)
            return $Roles{ $unboxed_name }
                if exists $Roles{ $unboxed_name };

            print "Role new: $unboxed_name\n";
            # ??? - Role uses a native namespace                
            my $role_class = KindaPerl6::Class->new( $name );

            # remove new() and HOW() from the role_class method list
            delete $role_class->HOW->{methods}{new};
            delete $role_class->HOW->{methods}{HOW};
                
            # methods and attributes are aliased to the metaclass list
            my $role = $Roles{ $unboxed_name } = bless {
                      role_name  => $name,
                      role_unboxed_name => $unboxed_name,
                      role_native_name  => $role_class->HOW->{class_native_name},
                      methods           => $role_class->HOW->{methods}, 
                      attributes        => $role_class->HOW->{attributes}, 
                  }, __PACKAGE__;
            $role;                
        },
    );
    
    $class->HOW->add_method(
        bless( [ 
                'add_method' 
            ], 
            'Type_Constant_Buf' 
        ),
        \&KindaPerl6::Class::add_method,
    );
    $class->HOW->add_method(
        bless( [ 
                'add_attribute' 
            ], 
            'Type_Constant_Buf' 
        ),
        \&KindaPerl6::Class::add_attribute,
    );
    $class->HOW->add_method(
        bless( [ 
                'add_role_to' 
            ], 
            'Type_Constant_Buf' 
        ),
        sub  {
            my ( $self, $object ) = @_;
            $object = $object->FETCH;
            #print "Role.add_role_to $self $object\n";
            #require Data::Dump::Streamer;
            #print Data::Dump::Streamer::Dump( @_ );
            $object->{_role_methods} = {
                %{ $object->{_role_methods} },    
                %{ $self->{attributes} },
                %{ $self->{methods} },
            };
            $object->{_roles}{ $self->{role_unboxed_name} } = $self;
            #print "object+role = ",Main::Dump( @_ );
            $self;                    
        }
    );

}

{
package Type_Constant;

    our $class = KindaPerl6::Class->new( 
        bless( [ 'KindaPerl6::Constant' ], 'Type_Constant_Buf' ),
    );
    #our @ISA = ( 'Role' );
    
    $class->HOW->add_method(
        bless( [ 'perl' ], 'Type_Constant_Buf' ),
        sub { 
           bless [ 
               $_[0][0] 
           ], 'Type_Constant_Buf' 
       },
    );

    $class->HOW->add_method(
        bless( [ 'new' ], 'Type_Constant_Buf' ),
        sub { 
            bless( $_[1]->FETCH->[0], ref( $_[0] ) );
        },
    );

} 

1;

__END__

{
    package KindaPerl6::MOP;
    use Class::MOP;
    use base 'Class::MOP::Class';
    
    eval {
        # respond to p6 'autobox'
        $_->add_method("FETCH" => sub { @_ } );
        #$_->alias_method("HOW" => sub { (shift)->meta }) 
    } foreach Class::MOP::get_all_metaclass_instances;
    
    sub create {
        my $class = shift;
        my $new_class = shift;
        $new_class = $$new_class
            if ref $new_class; # unbox to p5 if needed
        my $meta = $class->SUPER::create( $new_class );
        $meta->alias_method("HOW" => sub { (shift)->meta }); 
        
        $meta->add_method("new" => sub { 
                 my ($class, %param) = @_;
                 $class->meta->new_object(%params);
             } );
        # respond to p6 'autobox'
        #$meta->add_method("FETCH" => sub { @_ } );

        return $meta;    
    }
    #sub FETCH { @_ }
}

1;
