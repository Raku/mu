#!/usr/bin/perl

use strict;
use warnings;

## ----------------------------------------------------------------------------
## Mini Meta-Model with explicit EigenClasses
## ----------------------------------------------------------------------------
## This is an extension of the Mini-MetaModel which adds explicit Eigenclasses
## to all Classes created. See Method_Dispatch_w_EigenClasses.jpg in this 
## same directory for a visual explaination of this. 
## ----------------------------------------------------------------------------

use Test::More tests => 46;

{
    use Hash::Util 'lock_keys';

    # Every instance should have a unique ID
    my $instance_counter = 0;

    # Input: reference to class and a slurpy attr hash
    sub ::create_opaque_instance ($%) {
        my ($class, %attrs) = @_;
        my $instance = bless {
            'id'    => ++$instance_counter,
            'class' => $class,
            'attrs' => \%attrs,
        }, 'Dispatchable';
        lock_keys(%{$instance});
        return $instance;
    }

    # Accessors for the inside of the opaque structure
    sub ::opaque_instance_id    ($) : lvalue { shift->{id}       }
    sub ::opaque_instance_class ($) : lvalue { ${shift->{class}} }     
    sub ::opaque_instance_attrs ($) : lvalue { shift->{attrs}    }
}

{
    package Dispatchable;
    use Carp 'confess';

    sub isa { our $AUTOLOAD = 'isa'; goto &AUTOLOAD; }
    sub can { our $AUTOLOAD = 'can'; goto &AUTOLOAD; }

    sub AUTOLOAD {
        my $label = (split '::', our $AUTOLOAD)[-1];
        return if $label eq 'DESTROY';

        my $class = ::opaque_instance_class($_[0]);

        while (defined $class) {
            my $method = ::opaque_instance_attrs($class)->{'%:methods'}{$label};
            goto &$method if $method;

            # try again in the superclass
            $class = $class->superclass;
        }

        confess "No method found for '$label'";
    }
}

# The 'Class' class -- placed here so ::create_class can refer to it
my $Class;

sub ::create_class (%) {
    my (%attrs) = @_;
    return ::create_opaque_instance(
        # < a Class object is an instance of the Class class >
        \$Class,
        (
            '$:name'        => '',
            '$:superclass'  => undef,
            '%:attributes'  => [],
            '%:methods'     => {},
            # and override anything here ...
            %attrs,
        )
    );
}

# create this here so we can refer to it,.. 
my $EigenClass;

# The 'Class' class
$Class = ::create_class(
    '$:name'    => 'Class',
    '%:methods' => {
        'new' => sub ($%) {
            my ($class, %attrs) = @_;
            if ($class != $EigenClass) {
                my $eigenclass = $EigenClass->new(
                    '$:name' => 'EigenClass[' . 
                                    ($attrs{'$:name'} || 'i' . $class->name) . 
                                ']'
                );
                if (defined $attrs{'$:superclass'}) {
                    $eigenclass->superclass(
                        ::opaque_instance_class($attrs{'$:superclass'})
                    );                
                }
                else {
                    $eigenclass->superclass($class);
                }
                $class = $eigenclass;                
            }
            return ::create_opaque_instance(\$class, %attrs);
        },         
        'name' => sub ($) {
            ::opaque_instance_attrs(shift)->{'$:name'}
        },
        'id' => sub ($) {
            ::opaque_instance_id(shift)
        },        
        'class' => sub ($) {
            return ::opaque_instance_class(shift) if $_[0] == $Class;
            return ::opaque_instance_class(shift)->superclass;
        },                       
        'class_precendence_list' => sub ($) {
            my ($self) = @_;
            my @cpl = ($self);
            my $current = $self;
            while (my $super = $current->superclass) {
                push @cpl => $super;
                $current = $super;
            }
            return @cpl;
        },
        'superclass' => sub ($;$) {
            my $self = shift;
            ::opaque_instance_attrs($self)->{'$:superclass'} = shift if @_;
            ::opaque_instance_attrs($self)->{'$:superclass'};
        },
        'get_method' => sub ($$) {
            my ($self, $label) = @_;
            ::opaque_instance_attrs($self)->{'%:methods'}->{$label};
        },
        'add_method' => sub ($$$) {
            my ($self, $label, $method) = @_;
            ::opaque_instance_attrs($self)->{'%:methods'}->{$label} = $method;
        },               
    },
);

# The 'EigenClass' class
$EigenClass = ::create_class(
    '$:name'       => 'EigenClass',
    '$:superclass' => $Class,
);

# The 'Object' class
my $Object = $Class->new(
    '$:name'    => 'Object',
    '%:methods' => {
        'id' => sub ($) {
            ::opaque_instance_id(shift)
        },
        'class' => sub ($) {
            return ::opaque_instance_class(shift)->superclass;
        },
        'add_singleton_method' => sub ($$$) {
            my ($self, $label, $method) = @_;
            ::opaque_instance_class($self)->add_method($label, $method);
        },          
    },
);

# < Class is a subclass of Object >
::opaque_instance_attrs($Class)->{'$:superclass'} = $Object;

## ----------------------------------------------------------------------------
## BOOTSTRAPPING COMPLETE
## ----------------------------------------------------------------------------

# Utility to test that "No method found" error is raised

sub fails_ok (&$) {
    my ($code, $desc) = @_;
    local $@; eval { &$code };
    like($@, qr/No method found/, $desc);
}

# Begins testing

is($Class->id, 1, '... $Class is the first id');
is($Class->class, $Class, '... $Class refs to itself');
is($Class->name, 'Class', '... $Class got the right method return value');
is($Class->superclass, $Object, '... $Class is now a subclass of $Object');
is_deeply(
    [ $Class->class_precendence_list ], 
    [ $Class, $Object ], 
    '... $Class class_precendence_list');

is($Object->id, 4, '... $Object is the second id');
is($Object->class, $Class, '... $Object class slot is $Class');
is($Object->name, 'Object', '... $Object got the right method return value');
is($Object->superclass, undef, '... $Object got the right method return value');
is_deeply(
    [ $Object->class_precendence_list ], 
    [ $Object ], 
    '... $Object class_precendence_list');

## test adding a class method (as a signleton method on the class instance)

$Object->add_singleton_method('singleton_test_on_object' => sub { 
    '&Object::singleton_test_on_object'
});

is($Object->singleton_test_on_object(), 
   '&Object::singleton_test_on_object',
   '... got the right return value from Object singleton/class method');

$Object->add_singleton_method('another_singleton_test_on_object' => sub { 
   '&Object::another_singleton_test_on_object'
});

is($Object->another_singleton_test_on_object(), 
   '&Object::another_singleton_test_on_object',
   '... got the right return value from another Object singleton/class method');
   
is($Object->singleton_test_on_object(), 
  '&Object::singleton_test_on_object',
  '... still got the right return value from first Object singleton/class method');   

## make class

my $Foo = $Class->new(
    '$:name'       => 'Foo',
    '$:superclass' => $Object,
    '%:methods'    => {
        'foo' => sub ($) { 'Foo->foo' },
        'bar' => sub ($) { 'Foo->bar' },
    },
);

is($Foo->id, 6, '... $Foo is the fourth id');
is($Foo->name, 'Foo', '... $Foo got the right method return value');
is($Foo->superclass, $Object, '... $Foo got the right method return value');
is_deeply(
    [ $Foo->class_precendence_list ], 
    [ $Foo, $Object ], 
    '... $Foo class_precendence_list');

fails_ok { $Foo->bar } '... metaclass calling instance method fails';

## does Foo get Object's singleton methods ..

is($Foo->another_singleton_test_on_object(), 
   '&Object::another_singleton_test_on_object',
   '... got the right return value from another Object singleton/class method called by Foo');

is($Foo->singleton_test_on_object(), 
  '&Object::singleton_test_on_object',
  '... still got the right return value from first Object singleton/class method called by Foo');  

$Foo->add_singleton_method('singleton_test_on_Foo' => sub { 
 '&Foo::singleton_test_on_Foo'
});

is($Foo->singleton_test_on_Foo(), 
  '&Foo::singleton_test_on_Foo',
  '... still got the right return value from Foo singleton/class method');  
  
fails_ok { $Object->singleton_test_on_Foo() } '... Object cannot call singleton method defined in Foo';  

## make instances

my $iFoo = $Foo->new;
is($iFoo->id, 8, '... $iFoo is the fourth id');

# try to call the Class method
fails_ok { $iFoo->name } '... instance calling metaclass method fails';

fails_ok { $iFoo->another_singleton_test_on_object } '... instance calling metaclass singleton method fails';
fails_ok { $iFoo->singleton_test_on_object } '... instance calling metaclass singleton method fails';

is($iFoo->foo, 'Foo->foo', '... $iFoo got the right method return value');
is($iFoo->bar, 'Foo->bar', '... $iFoo got the right method return value');

$iFoo->add_singleton_method('test_iFoo_singleton_method' => sub { 
   '$iFoo::test_iFoo_singleton_method' 
});

is($iFoo->test_iFoo_singleton_method(), 
   '$iFoo::test_iFoo_singleton_method',
   '... got the right return value from $iFoo singleton method');
   
$iFoo->add_singleton_method('another_test_iFoo_singleton_method' => sub { 
  '$iFoo::another_test_iFoo_singleton_method' 
});

is($iFoo->another_test_iFoo_singleton_method(), 
  '$iFoo::another_test_iFoo_singleton_method',
  '... got the right return value from another $iFoo singleton method');   

is($iFoo->test_iFoo_singleton_method(), 
 '$iFoo::test_iFoo_singleton_method',
 '... still got the right return value from $iFoo singleton method');

## make subclasses

my $Bar = $Class->new(
    '$:name'       => 'Bar',
    '$:superclass' => $Foo,
    '%:methods'    => {
        'bar' => sub ($) { 'Bar->bar' },
        'baz' => sub ($) { 'Bar->baz' },
    },
);

is($Bar->id, 10, '... $Bar is the fifth id');
is($Bar->name, 'Bar', '... $Bar got the right method return value');
is($Bar->superclass, $Foo, '... $Bar got the right method return value');
is_deeply(
    [ $Bar->class_precendence_list ], 
    [ $Bar, $Foo, $Object ], 
    '... $Bar class_precendence_list');
    
## does Bar get Object's singleton methods ..

is($Bar->another_singleton_test_on_object(), 
   '&Object::another_singleton_test_on_object',
   '... got the right return value from another Object singleton/class method called by Bar');

is($Bar->singleton_test_on_object(), 
  '&Object::singleton_test_on_object',
  '... still got the right return value from first Object singleton/class method called by Bar');      
  
is($Bar->singleton_test_on_Foo(), 
   '&Foo::singleton_test_on_Foo',
   '... still got the right return value from Foo singleton/class method called by Bar');   

## make instances of subclasses

my $iBar = $Bar->new;
is($iBar->id, 12, '... $iBar is the sixth id');
is($iBar->class, $Bar, '... $iBar refs to $Bar');

# try to call the Class method
fails_ok { $iBar->name } '... instance calling metaclass method fails';
fails_ok { $iBar->another_singleton_test_on_object } '... instance calling metaclass singleton method fails';
fails_ok { $iBar->singleton_test_on_object } '... instance calling metaclass singleton method fails';

is($iBar->foo, 'Foo->foo', '... $iBar calls superclass foo');
is($iBar->bar, 'Bar->bar', '... $iBar calls overridden bar');
is($iBar->baz, 'Bar->baz', '... $iBar calls new method baz');
