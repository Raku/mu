#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 28;

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

my $Class;

sub ::create_class (%) {
    my (%attrs) = @_;
    return ::create_opaque_instance(
        # < Class is instance of Class >
        \$Class,
        (
            '$:name'        => '',
            '$:superclass'  => undef,
            '%:attributes'  => {},
            '%:methods'     => {},
            # and override anything here ...
            %attrs,
        )
    );
}

$Class = ::create_class(
    '$:name'    => 'Class',
    '%:methods' => {
        'name' => sub ($) {
            ::opaque_instance_attrs(shift)->{'$:name'}
        },
        'superclass' => sub ($) {
            ::opaque_instance_attrs(shift)->{'$:superclass'}
        },
        'get_method' => sub ($$) {
            my ($self, $label) = @_;
            ::opaque_instance_attrs($self)->{'%:methods'}->{$label};
        }
    },
);

my $Object = ::create_class(
    '$:name'    => 'Object',
    '%:methods' => {
        'new' => sub ($%) {
            my ($class, %attrs) = @_;
            return ::create_opaque_instance(\$class, %attrs);
        },
        'id' => sub ($) {
            ::opaque_instance_id(shift)
        },
        'class' => sub ($) {
            ::opaque_instance_class(shift)
        }
    },
);

# < Class is subclass of Object >
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

is($Object->id, 2, '... $Object is the second id');
is($Object->class, $Class, '... $Object class slot is $Class');
is($Object->name, 'Object', '... $Object got the right method return value');
is($Object->superclass, undef, '... $Object got the right method return value');

## make class

my $Foo = $Class->new(
    '$:name'       => 'Foo',
    '$:superclass' => $Object,
    '%:methods'    => {
        'foo' => sub ($) { 'Foo->foo' },
        'bar' => sub ($) { 'Foo->bar' },
    },
);

is($Foo->id, 3, '... $Foo is the fourth id');
is($Foo->class, $Class, '... $Foo refs to metaclass');
is($Foo->name, 'Foo', '... $Foo got the right method return value');
is($Foo->superclass, $Object, '... $Foo got the right method return value');

fails_ok { $Foo->bar } '... metaclass calling instance method fails';

## make instances

my $iFoo = $Foo->new;
is($iFoo->id, 4, '... $iFoo is the fourth id');
is($iFoo->class, $Foo, '... $iFoo refs to $Foo');

# try to call the Class method
fails_ok { $iFoo->name } '... instance calling metaclass method fails';

is($iFoo->foo, 'Foo->foo', '... $iFoo got the right method return value');
is($iFoo->bar, 'Foo->bar', '... $iFoo got the right method return value');

## make subclasses

my $Bar = $Class->new(
    '$:name'       => 'Bar',
    '$:superclass' => $Foo,
    '%:methods'    => {
        'bar' => sub ($) { 'Bar->bar' },
        'baz' => sub ($) { 'Bar->baz' },
    },
);

is($Bar->id, 5, '... $Bar is the fifth id');
is($Bar->class, $Class, '... $Bar refs to metaclass');
is($Bar->name, 'Bar', '... $Bar got the right method return value');
is($Bar->superclass, $Foo, '... $Bar got the right method return value');

## make instances of subclasses

my $iBar = $Bar->new;
is($iBar->id, 6, '... $iBar is the sixth id');
is($iBar->class, $Bar, '... $iBar refs to $Bar');

# try to call the Class method
fails_ok { $iBar->name } '... instance calling metaclass method fails';

is($iBar->foo, 'Foo->foo', '... $iBar calls superclass foo');
is($iBar->bar, 'Bar->bar', '... $iBar calls overridden bar');
is($iBar->baz, 'Bar->baz', '... $iBar calls new method baz');
