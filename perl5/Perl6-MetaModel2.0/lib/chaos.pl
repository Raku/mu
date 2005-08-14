#!/usr/bin/perl

use strict;
use warnings;

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
            $class = ::opaque_instance_attrs($class)->{'@:superclasses'};
        }

        confess "No method found for $label";
    }
}
