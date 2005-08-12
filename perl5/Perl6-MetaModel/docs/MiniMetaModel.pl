#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 25;

{
    use Hash::Util 'lock_keys';    
    
    # every instance should 
    # have a unique ID
    my $instance_counter = 0;

    sub ::create_opague_instance {
        my ($class, %attrs) = @_;
        my $instance = bless {
            id    => ++$instance_counter,
            class => $class,
            attrs => \%attrs
        }, 'Dispatchable';
        lock_keys(%{$instance});
        return $instance;
    }

    # accessors for the inside of 
    # the opaque structure
    sub ::opaque_instance_id    : lvalue { (shift)->{id}       }    
    sub ::opaque_instance_class : lvalue { ${(shift)->{class}} }
    sub ::opaque_instance_attrs : lvalue { (shift)->{attrs}    }
}

{
    package Dispatchable;
    use Carp 'confess';

    sub isa { our $AUTOLOAD = 'isa'; goto &AUTOLOAD; }
    sub can { our $AUTOLOAD = 'can'; goto &AUTOLOAD; }

    sub AUTOLOAD {
        my $label = (split '::', our $AUTOLOAD)[-1];
        return if $label =~ 'DESTROY';
        my $class = ::opaque_instance_class($_[0]);            
        my $current = $class;
    METHOD:
        if (my $method = ::opaque_instance_attrs($current)->{'%:methods'}->{$label}) {
            goto $method;
        }
        else {
            $current = $current->superclass;
            goto METHOD if defined $current;
        }
        confess "No method found for $label";
    }    
}

my $Class;

sub Class::new {
    my (%attrs) = @_;
    my $class = ::create_opague_instance(
        # < Class is instance of Class >
        \$Class,
        (
            '$:name'        => '',
            '@:superclass'  => undef,
            '%:attributes'  => {},
            '%:methods'     => {},                
            # and override anything here ...
            %attrs,
        )
    );
    return $class;
}  

$Class = Class::new(
    '$:name'    => 'Class',
    '%:methods' => {
        'name' => sub {
            ::opaque_instance_attrs($_[0])->{'$:name'}
        },
        'superclass' => sub {
            ::opaque_instance_attrs($_[0])->{'@:superclass'}
        },  
        'get_method' => sub {
            my ($self, $label) = @_;
            ::opaque_instance_attrs($self)->{'%:methods'}->{$label};
        }            
    }, 
);

my $Object = Class::new(
    '$:name'    => 'Object',
    '%:methods' => {
        'new' => sub {
            my ($class, %attrs) = @_;
            return ::create_opague_instance(\$class, %attrs);
        },
        'id' => sub {
            ::opaque_instance_id($_[0])        
        },
        'class' => sub {
            ::opaque_instance_class($_[0])
        }
    }
); 

# < Class is subclass of Object >
::opaque_instance_attrs($Class)->{'@:superclass'} = $Object;    

## ----------------------------------------------------------------------------
## BOOTSTRAPPING COMPLETE
## ----------------------------------------------------------------------------

is($Class->id, 1, '... $Class is the first id');
is($Class->class, $Class, '... $Class refs to itself');
is($Class->name(), 'Class', '... $Class got the right method return value');
is($Class->superclass(), $Object, '... $Class is now a subclass of $Object');

is($Object->id, 2, '... $Object is the second id');
is($Object->class, $Class, '... $Object class slot is $Class');
is($Object->name(), 'Object', '... $Object got the right method return value');
is($Object->superclass(), undef, '... $Object got the right method return value');

## make class

my $Foo = $Class->new(
    '$:name'       => 'Foo',
    '@:superclass' => $Object,
    '%:methods'    => {
        'bar' => sub { 'Foo->bar' }
    }
);

is($Foo->id, 3, '... $Foo is the fourth id');
is($Foo->class, $Class, '... $Foo refs to metaclass');
is($Foo->name(), 'Foo', '... $Foo got the right method return value');
is($Foo->superclass(), $Object, '... $Foo got the right method return value');

## make instances

my $iFoo = $Foo->new();
is($iFoo->id, 4, '... $iFoo is the fourth id');
is($iFoo->class, $Foo, '... $iFoo refs to $Foo');

$@ = undef;
# try to call the Class method
eval { $iFoo->name() };
ok($@, '... we got an exception (because no method was found)');

is($iFoo->bar(), 'Foo->bar', '... $iFoo got the right method return value');

## make subclasses

my $Bar = $Class->new(
    '$:name'       => 'Bar',
    '@:superclass' => $Foo,
    '%:methods'    => {
        'foo' => sub { 'Bar->foo' }
    }
);

is($Bar->id, 5, '... $Bar is the fifth id');
is($Bar->class, $Class, '... $Bar refs to metaclass');
is($Bar->name(), 'Bar', '... $Bar got the right method return value');
is($Bar->superclass(), $Foo, '... $Bar got the right method return value');

## make instances of subclasses

my $iBar = $Bar->new();
is($iBar->id, 6, '... $iBar is the sixth id');
is($iBar->class, $Bar, '... $iBar refs to $Bar');

$@ = undef;
# try to call the Class method
eval { $iBar->name() };
ok($@, '... we got an exception (because no method was found)');    

is($iBar->foo(), 'Bar->foo', '... $iBar got the right method return value');
is($iBar->bar(), 'Foo->bar', '... $iBar got the right method return value');
