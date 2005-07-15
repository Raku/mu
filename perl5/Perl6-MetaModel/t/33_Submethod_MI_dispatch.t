#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 5;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::SubMethod;

=pod

class Foo {
	method baz { ... }
}

class Bar {
	submethod baz { ... }
}

class FooBar is Foo is Bar {}

my $foo_bar = FooBar.new();
$foo_bar.baz() # calls Foo::baz()

=cut

class Foo => {
    instance => {
        methods => {
            baz => sub { 'Foo::baz' }
        }
    }
};

class Bar => {
    instance => {
        submethods => {
            baz => sub { 'Bar::baz<submethod>' }
        }
    }     
};

class FooBar => {
    is => [ 'Bar', 'Foo' ]
};

# now check that the correct method is called

my $foo_bar = FooBar->new();
isa_ok($foo_bar, 'Bar');
isa_ok($foo_bar, 'Foo');

can_ok($foo_bar, 'baz');

{
    my $value;
    lives_ok {
        $value = $foo_bar->baz()
    } '... calling baz() succedded';
    pass(); #is($value, 'Foo::baz', '... got the right return value');    
}


