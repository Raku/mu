#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 26;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::Object;

=pod

This test file checks single level role composition, which 
demonstrates the following behaviors:

=over 4

=item flattening one role into a class

=item flattening two roles into a class

=item flattening two roles into a class with a fatal conflict

=item flattening two roles into a class with a conflict which the class resolves

=item flattening two roles into a class with a conflict which the superclass could resolve, but cannot

=item flatting one role into a class whose method overrides the superclass method

=back

=cut

# add 1 role

role rFoo => {
    methods => {
        foo => sub { 'rFoo::foo' }
    }
};

class Foo => {
    is => [ 'Perl6::Object' ],    
    does => [ 'rFoo' ]
};

my $foo = Foo->new();
isa_ok($foo, 'Foo');
can_ok($foo, 'foo');

ok($foo->does('rFoo'), '... our class does rFoo');

is_deeply(
    [ $foo->does() ],
    [ 'rFoo' ],
    '... does() w/out args gives us the right roles');

is($foo->foo, 'rFoo::foo', '... the foo() is from rFoo (as expected)');

# combine 2 roles

role rBar => {
    methods => {
        bar => sub { 'rBar::bar' }
    }
};

class FooBar => {
    is => [ 'Perl6::Object' ],    
    does => [ 'rFoo', 'rBar' ]
};

my $foo_bar = FooBar->new();
isa_ok($foo_bar, 'FooBar');
can_ok($foo_bar, 'foo');
can_ok($foo_bar, 'bar');

ok($foo_bar->does('rFoo'), '... our class does rFoo');
ok($foo_bar->does('rBar'), '... our class does rBar');

is_deeply(
    [ $foo_bar->does() ],
    [ 'rFoo', 'rBar' ],
    '... does() w/out args gives us the right roles');

is($foo_bar->foo, 'rFoo::foo', '... the foo() is from rFoo (as expected)');
is($foo_bar->bar, 'rBar::bar', '... the bar() is from rBar (as expected)');

# combine 2 roles with one fatal conflict

role rFoo2 => {
    methods => {
        foo => sub { 'rFoo2::foo' }
    }
};

dies_ok {
    class FooFail => {
        is => [ 'Perl6::Object' ],        
        does => [ 'rFoo', 'rFoo2' ]
    };
} '... we got an error when 2 roles conflicted';

# combine 2 roles with a conflict resolved by the class

class FooResolve => {
    is => [ 'Perl6::Object' ],    
    does => [ 'rFoo', 'rFoo2' ],
    instance => {
        methods => {
            foo => sub { 'FooResolve::foo' }
        }
    }
};

my $foo_resolve = FooResolve->new();
isa_ok($foo_resolve, 'FooResolve');
can_ok($foo_resolve, 'foo');

ok($foo_resolve->does('rFoo'), '... our class does rFoo');
ok($foo_resolve->does('rFoo2'), '... our class does rFoo2');

is_deeply(
    [ $foo_resolve->does() ],
    [ 'rFoo', 'rFoo2' ],
    '... does() w/out args gives us the right roles');

is($foo_resolve->foo, 'FooResolve::foo', '... the foo() is from FooResolve (as expected)');

# combine 2 roles in conflict with a super defining the method, but not resolving it

dies_ok {
    class FooFail2 => {
        does => [ 'rFoo', 'rFoo2' ],
        is => [ 'FooResolve' ],
    };
} '... we got an error combing two roles without resolving (even in super)';

# combine 1 role and have it override the super's method

class FooNoSuper => {
    does => [ 'rFoo' ],
    is => [ 'FooResolve' ],
};

my $foo_no_super = FooNoSuper->new();
isa_ok($foo_no_super, 'FooNoSuper');
can_ok($foo_no_super, 'foo');

ok($foo_no_super->does('rFoo'), '... our class does rFoo');

is_deeply(
    [ $foo_no_super->does() ],
    [ 'rFoo' ],
    '... does() w/out args gives us the right roles');

is($foo_no_super->foo, 'rFoo::foo', '... the foo() is from rFoo (as expected)');


