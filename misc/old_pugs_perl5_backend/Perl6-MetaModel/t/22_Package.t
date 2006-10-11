#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 25;
use Test::Exception;

use Perl6::MetaModel;

=pod

package Foo {
    my $bar;
    my @bar;
    my %bar;

    sub bar { ... }

    package Foo::Bar {}
}

=cut

{
    my $Foo = $::Package->new('$:name' => 'Foo');

    lives_ok {
        $Foo->STORE('$bar' => 1);
        $Foo->STORE('@bar' => [ 1, 2, 3 ]);
        $Foo->STORE('%bar' => { one => 1, two => 2 });
        $Foo->STORE('&bar' => sub { 'Foo::bar' });
        $Foo->STORE('::Bar' => $::Package->new('$:name' => 'Foo::Bar'));
    } '... stored values inside the pacakge';

    is($Foo->FETCH('$bar'), 1, '... got the SCALAR right');
    is_deeply($Foo->FETCH('@bar'), [ 1, 2, 3 ], '... got the ARRAY right');
    is_deeply($Foo->FETCH('%bar'), { one => 1, two => 2 }, '... got the HASH right');
    is($Foo->FETCH('&bar')->(), 'Foo::bar', '... got the CODE right');
    
    my $FooBar = $Foo->FETCH('::Bar');
    isa_ok($FooBar, 'Package');
    is($FooBar->name, 'Foo::Bar', '... got the sub package right');
}

# treat a class as a package

{
    my $Foo = class 'Foo' => {
        is => [ $::Object ],
        attributes => [ '$.bar', '@:baz' ],
        methods => {
            'bar' => sub { 'Foo::bar' }
        }
    };
    
    isa_ok($Foo->FETCH('$.bar'), 'Perl6::Attribute');
    isa_ok($Foo->FETCH('@:baz'), 'Perl6::Attribute');
    

    ok(!defined($Foo->FETCH('%:baz')), '... no attribute by that name here');
    
    my $bizzle = ::make_attribute('$.bizzle');
    lives_ok {
        $Foo->STORE('$.bizzle' => $bizzle);
    } '... we can add Perl6::Attributes though';
    isa_ok($Foo->FETCH('$.bizzle'), 'Perl6::Attribute');    
    is($Foo->FETCH('$.bizzle'), $bizzle, '... it is the same attribute we stored');        
      
    lives_ok {
        $Foo->STORE('&bizzle' => ::make_method(sub { _('$.bizzle') }));
    } '... make a bizzle accessor';
    
    is($Foo->new('$.bizzle' => 42)->bizzle(), 42, '... created and stored the attribute value');
    
    # now for the code ...
      
    is($Foo->FETCH('&bar')->($Foo->new()), 'Foo::bar', '... got the CODE right');  
    
    # check adding instance methods
    lives_ok {
        $Foo->STORE('&baz' => ::make_method(sub { 'Foo::baz' }));
    } '... did store a method object';
    is($Foo->FETCH('&baz')->($Foo->new()), 'Foo::baz', '... got the STORE-ed CODE right');  
    is($Foo->new()->baz(), 'Foo::baz', '... and we can call the method normally too');

    # check adding class methods     
    lives_ok {
        $Foo->STORE('&blarch' => ::make_class_method(sub { 'Foo::blarch' }));
    } '... did store a class method object';
    is($Foo->FETCH('&blarch')->($Foo), 'Foo::blarch', '... got the STORE-ed CODE right');  
    is($Foo->blarch(), 'Foo::blarch', '... and we can call the method normally too');     
     
    # now just store some misc. subs in the class namespace
    lives_ok {
        $Foo->STORE('&bling' => sub { 'Foo::bling' });
    } '... did store a plain-ole sub';
    is($Foo->FETCH('&bling')->(), 'Foo::bling', '... got the STORE-ed CODE right'); 
    dies_ok { $Foo->bling() } '... but you cannot call this as a method';
     
}
