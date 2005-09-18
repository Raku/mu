#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 13;

## ----------------------------------------------------------------------------
## Mini Meta-Model with Self-Bootstrapping Roles
## ----------------------------------------------------------------------------
## This is an extension of the Mini-MetaModel which adds a self-bootstrapping
## implementation of Roles into the model. See below for more details.
## ----------------------------------------------------------------------------

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

        confess "No method found for $label";
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

# The 'Class' class
$Class = ::create_class(
    '$:name'    => 'Class',
    '%:methods' => {
        'name' => sub ($) {
            ::opaque_instance_attrs(shift)->{'$:name'}
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
        'superclass' => sub ($) {
            ::opaque_instance_attrs(shift)->{'$:superclass'}
        },
        'get_method' => sub ($$) {
            my ($self, $label) = @_;
            ::opaque_instance_attrs($self)->{'%:methods'}->{$label};
        },
        'has_method' => sub ($$) {
            my ($self, $label) = @_;
            exists ::opaque_instance_attrs($self)->{'%:methods'}->{$label} ? 1 : 0;
        },        
        'add_method' => sub ($$$) {
            my ($self, $label, $method) = @_;
            ::opaque_instance_attrs($self)->{'%:methods'}->{$label} = $method;
        },
        'get_method_list' => sub ($) {
            my ($self) = @_;
            keys %{::opaque_instance_attrs($self)->{'%:methods'}};
        },                             
    },
);
# The 'Object' class
my $Object = ::create_class(
    '$:name'    => 'Object',
    '%:methods' => {
        'new' => sub ($%) {
            my ($class, %attrs) = @_;
            return ::create_opaque_instance(\$class, %attrs);
        },      
        'id'    => sub ($) { ::opaque_instance_id(shift)    },
        'class' => sub ($) { ::opaque_instance_class(shift) }
    },
);

# < Class is a subclass of Object >
::opaque_instance_attrs($Class)->{'$:superclass'} = $Object;

## ----------------------------------------------------------------------------
## BOOTSTRAPPING Class/Object COMPLETE
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## BOOTSTRAPPING Roles
## ----------------------------------------------------------------------------

my $Role;

my $_resolve = sub ($) {
    my ($self) = @_;
    my %roles;
    foreach my $role (@{::opaque_instance_attrs($self)->{'@:roles'}}) {
        # make a note of the role for does()
        $roles{::opaque_instance_attrs($role)->{'$:name'}} = undef;
        foreach my $method_name ($role->get_method_list) {
            # if we already have the method, then
            # ignore it and go to the next one
            next if $self->has_method($method_name);
            # otherwise ... add the method in 
            $self->add_method($method_name => $role->get_method($method_name));
        }
    }
    # this is our does() method, it will
    # allow us to ask if the instance 
    my $_does = sub { exists $roles{$_[1]} ? 1 : 0 };
    $self->add_method('does' => $_does);
};

# our class(Role), which is also the role(Role) as well
$Role = $Class->new(
    '$:name' => 'Role',
    '$:superclass' => $Object,
    '%:methods' => {
        'resolve' => $_resolve,
        'add_method' => sub ($$$) {
            my ($self, $label, $method) = @_;
            ::opaque_instance_attrs($self)->{'%:methods'}->{$label} = $method;
        },
        'get_method' => sub ($$) {
            my ($self, $label) = @_;
            ::opaque_instance_attrs($self)->{'%:methods'}->{$label};
        },        
        'get_method_list' => sub ($$) {
            my ($self, $label) = @_;
            keys %{::opaque_instance_attrs($self)->{'%:methods'}};
        },                
    }
);

ok(!$Class->has_method('resolve'), '... we do not have the resolve method');

# Bootstrap -> Class does Role 
::opaque_instance_attrs($Class)->{'@:roles'} = [ $Role ];
$_resolve->($Class);

# Bootstrap -> Role does Role
::opaque_instance_attrs($Role)->{'@:roles'} = [ $Role ];
$Role->resolve(); # Role can resolve itself now :)

# Class gets a special case Does
# to avoid endless recursion
$Class->add_method('does' => sub {
    return $_[1] eq 'Role' if $_[0] == $Class;
    ::opaque_instance_attrs($_[0])->{'%:methods'}->{'does'}->(@_);
});

# now Class does Role

## ----------------------------------------------------------------------------
## BOOTSTRAPPING Roles COMPLETE
## ----------------------------------------------------------------------------

ok($Class->does('Role'), '... Class now does Role');
ok($Class->has_method('resolve'), '... we now have the resolve method');

ok($Role->has_method('does'), '... we now have the does() method');
ok($Role->does('Role'), '... Role now does Role');

## create some roles
my $rFoo = $Role->new(
    '$:name' => 'rFoo',
    '%:methods' => { foo => sub { 'rFoo::foo' } }
);
ok($rFoo->does('Role'), '... rFoo now does Role');

my $rBar = $Role->new(
    '$:name' => 'rBar',
    '%:methods' => { bar => sub { 'rBar::bar' } }
);
ok($rBar->does('Role'), '... rBar now does Role');

# create a class to put the roles in ...
my $FooBar = $Class->new(
    '$:name' => 'FooBar',
    '$:superclass' => $Object,
    '@:roles' => [ $rFoo, $rBar ],  # << tell the class we want these roles
    '%:methods' => {
        'foo_bar' => sub { 'FooBar::foo_bar' }
    }
);

is_deeply(
    [ $FooBar->get_method_list ],
    [ 'foo_bar' ],
    '... got the right method list (unresolved)');

# resolve the class and pull in all the roles
$FooBar->resolve();

is_deeply(
    [ sort $FooBar->get_method_list ],
    [ 'bar', 'does', 'foo', 'foo_bar' ],
    '... got the right method list (resolved)');
    
ok($FooBar->does('rFoo'), '... FooBar does rFoo');
ok($FooBar->does('rBar'), '... FooBar does rBar');

# and our instances do the roles as well
my $iFooBar = $FooBar->new();    
ok($iFooBar->does('rFoo'), '... iFooBar does rFoo');
ok($iFooBar->does('rBar'), '... iFooBar does rBar');     
    
1;