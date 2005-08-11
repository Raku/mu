#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 5;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::Object;

=pod

I am not sure if this behavior makes sense. That the class(Foo) actually behaves like
the Foo class, meaning it responds to class methods and such. 

=cut

my $Foo = class Foo => {
    is => [ 'Perl6::Object' ],    
    class => {
        methods => {
            bar => sub { "Foo::bar" }
        }
    },
    instance => {
        methods => {
            baz => sub { 'iFoo::baz' } 
        }
    }
};

isa_ok($Foo, 'Foo');

can_ok($Foo, 'bar');
ok(!$Foo->can('baz'), '... ::Foo cannot call instance methods');

is($Foo->bar(), 'Foo::bar', '... got he right value when bar() class method is called');

my $iFoo = $Foo->new();
isa_ok($iFoo, 'Foo');
