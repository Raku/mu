#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 15;

use Perl6::MetaModel;
use Perl6::Object;

class 'FooAUTOLOADED' => {
    is => [ 'Perl6::Object' ],
    instance => {
        methods => {
            'isa' => sub { next_METHOD() },
            'AUTOLOAD' => sub {
                my ($self, @args) = @_;
                my $label = AUTOLOAD($self);
                return $label;
            }
        }
    }    
};

my $foo = FooAUTOLOADED->new();
isa_ok($foo, 'FooAUTOLOADED');
isa_ok($foo, 'Perl6::Object');

is($foo->test(), 'test', '... AUTOLOAD captrured the "test" method correctly');
is($foo->testing(), 'testing', '... AUTOLOAD captrured the "testing" method correctly');
is($foo->hello(), 'hello', '... AUTOLOAD captrured the "hello" method correctly');
ok($foo->isa('FooAUTOLOADED'), '... AUTOLOAD did not captrure the "isa" method (as expected)');


class 'BarAUTOLOADED' => {
    is => [ 'FooAUTOLOADED' ],
    class => {
        methods => {
            'new' => sub { 
                next_METHOD();
            },
            'AUTOLOAD' => sub {
                my ($self, @args) = @_;
                my $label = AUTOLOAD($self);
                return $label;
            }
        }        
    }    
};

is(BarAUTOLOADED->test(), 'test', '... AUTOLOAD captrured the "test" method correctly');
is(BarAUTOLOADED->testing(), 'testing', '... AUTOLOAD captrured the "testing" method correctly');
is(BarAUTOLOADED->hello(), 'hello', '... AUTOLOAD captrured the "hello" method correctly');

my $bar = BarAUTOLOADED->new();
isa_ok($bar, 'BarAUTOLOADED');
isa_ok($bar, 'FooAUTOLOADED');
isa_ok($bar, 'Perl6::Object');

is($bar->test(), 'test', '... AUTOLOAD captrured the "test" method correctly');
is($bar->testing(), 'testing', '... AUTOLOAD captrured the "testing" method correctly');
is($bar->hello(), 'hello', '... AUTOLOAD captrured the "hello" method correctly');
