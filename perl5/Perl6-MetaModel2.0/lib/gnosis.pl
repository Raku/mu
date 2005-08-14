#!/usr/bin/perl

use strict;
use warnings;

BEGIN { do "lib/chaos.pl" };

# The 'Class' class -- placed here so ::create_class can refer to it
$::Class = undef;

sub ::create_class (%) {
    my (%attrs) = @_;
    return ::create_opaque_instance(
        # < a Class object is an instance of the Class class >
        \$::Class,
        (
            # meta-information
            '$:name'             => $attrs{'$:name'} || undef,
            '$:version'          => '0.0.0',
            '$:authority'        => undef,
            # the guts
            '@:MRO'              => [],
            '@:superclasses'     => [],
            '%:private_methods'  => {},
            '%:attributes'       => {},
            '%:methods'          => $attrs{'%:methods'} || {},
            '%:class_attributes' => {},
            '%:class_methods'    => {},            
        )
    );
}

# The 'Class' class
$::Class = ::create_class(
    '$:name'    => 'Class',
    '%:methods' => {
        'name' => sub ($) {
            ::opaque_instance_attrs(shift)->{'$:name'}
        },
        'class_precendence_list' => sub ($) {
            my ($self) = @_;
            my @cpl = ($self);
            my $current = $self;
            while (my $super = $current->superclass->[0]) {
                push @cpl => $super;
                $current = $super;
            }
            return @cpl;
        },        
        'superclass' => sub ($) {
            ::opaque_instance_attrs(shift)->{'@:superclasses'}
        },
        'get_method' => sub ($$) {
            my ($self, $label) = @_;
            ::opaque_instance_attrs($self)->{'%:methods'}->{$label};
        }
    },
);
