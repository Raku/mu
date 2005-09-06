#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 8;
use Test::Exception;

use Perl6::MetaModel;

my $Foo = class 'Foo' => {
    'is' => [ $::Object ],    
    'attributes' => [ '$.baz' ],
    'submethods' => {
        'BUILD' => sub { _('$.baz' => 'Foo::baz') },
    },
    'methods' => {
        'baz' => sub { _('$.baz') },
        'foo' => sub { $::SELF->baz() }
    }
};

my $foo = $Foo->new();
isa_ok($foo, 'Foo');
isa_ok($foo, 'Object');

is($foo->baz(), 'Foo::baz', '... $::SELF worked correctly in the BUILD method');
is($foo->foo(), 'Foo::baz', '... $::SELF worked correctly in the instance methods in nested calls (in the same class)');


my $Bar = class 'Bar' => {
    'is' => [ $::Object ],    
    'attributes' => [ '$.foo' ],
    'submethods' => { 
        'BUILD' => sub { _('$.foo' => $Foo->new()) }
    },
    'methods' => {
        'foo' => sub { _('$.foo') },
        'bar' => sub { 'Bar::bar' },
        'baz' => sub {
            my $val = $::SELF->foo()->baz();
            return "$val -> " . $::SELF->bar();
        }
    }
};

my $bar = $Bar->new();
isa_ok($bar, 'Bar');
isa_ok($bar, 'Object');

isa_ok($bar->foo(), 'Foo');

is($bar->baz(), 'Foo::baz -> Bar::bar', '... $::SELF worked correctly in the instance methods in nested calls (in the same class)');
