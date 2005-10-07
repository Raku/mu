#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 11;
use Test::Exception; 

require Perl6::MetaModel::Genesis;

ok($::Class->does('Role'), '... $::Class does Role');
ok($::Role->does('Role'), '... $::Role does Role'); 
    
ok($::Role->isa('Module'), '... $::Role isa Module');
ok($::Role->isa('Package'), '... $::Role isa Package');
ok($::Role->isa('Object'), '... $::Role isa Object');  

my $Foo = $::Class->new('$:name' => 'Foo');
isa_ok($Foo, 'Foo');
isa_ok($Foo, 'Class');
ok($Foo->does('Role'), '... $Foo does Role');

my $rFoo = $::Role->new('$:name' => 'rFoo');
isa_ok($rFoo, 'Role');
ok($rFoo->does('rFoo'), '... rFoo does rFoo'); 
ok($rFoo->does('Role'), '... rFoo does Role'); 
