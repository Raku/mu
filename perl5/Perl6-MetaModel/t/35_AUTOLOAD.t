#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 15;

use Perl6::MetaModel;

my $FooAUTOLOADED = class 'FooAUTOLOADED' => {
    is => [ $::Object ],
    methods => {
        'isa' => sub { next_METHOD() },
        'AUTOLOAD' => sub {
            my ($self, @args) = @_;
            my $label = ::opaque_instance_class($self)->FETCH('$AUTOLOAD');
            return $label;
        }
    }
};

my $foo = $FooAUTOLOADED->new();
isa_ok($foo, 'FooAUTOLOADED');
isa_ok($foo, 'Object');

is($foo->test(), 'test', '... AUTOLOAD captrured the "test" method correctly');
is($foo->testing(), 'testing', '... AUTOLOAD captrured the "testing" method correctly');
is($foo->hello(), 'hello', '... AUTOLOAD captrured the "hello" method correctly');
ok($foo->isa('FooAUTOLOADED'), '... AUTOLOAD did not captrure the "isa" method (as expected)');


my $BarAUTOLOADED = class 'BarAUTOLOADED' => {
    is => [ $FooAUTOLOADED ],
    class_methods => {
        'AUTOLOAD' => sub {
            my ($self, @args) = @_;
            my $label = $self->FETCH('$AUTOLOAD');
            return $label;
        }
    }        
};

is($BarAUTOLOADED->class::test(), 'test', '... AUTOLOAD captrured the "test" method correctly');
is($BarAUTOLOADED->class::testing(), 'testing', '... AUTOLOAD captrured the "testing" method correctly');
is($BarAUTOLOADED->class::hello(), 'hello', '... AUTOLOAD captrured the "hello" method correctly');

my $bar = $BarAUTOLOADED->new();
isa_ok($bar, 'BarAUTOLOADED');
isa_ok($bar, 'FooAUTOLOADED');
isa_ok($bar, 'Object');

is($bar->test(), 'test', '... AUTOLOAD captrured the "test" method correctly');
is($bar->testing(), 'testing', '... AUTOLOAD captrured the "testing" method correctly');
is($bar->hello(), 'hello', '... AUTOLOAD captrured the "hello" method correctly');
