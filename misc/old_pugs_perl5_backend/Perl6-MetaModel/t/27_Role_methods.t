#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 19;
use Test::Exception;

use Perl6::MetaModel;

=pod

This test is primarily focues on Role methods

=cut

my $Foo = $::Role->new('$:name' => 'Foo');
isa_ok($Foo, 'Role');
ok(!$Foo->isa('Foo'), '... $Foo is not a Foo'); 

is_deeply(
    [ $Foo->get_method_list ],
    [],
    '... no $Foo methods yet');

$Foo->add_method('foo' => ::make_method(sub { 'Foo::foo' }));

ok($Foo->has_method('foo'), '... $Foo.has_method(foo)');
is($Foo->get_method('foo')->(), 'Foo::foo', '... $Foo.get_method(foo)');

ok(!$Foo->has_method('blarch'), '... no $Foo.has_method(blarch)');
is($Foo->get_method('blarch'), undef, '... $Foo.get_method(blarch) returns undef');

is_deeply(
    [ $Foo->get_method_list ],
    [ 'foo' ],
    '... got the list of $Foo methods');

$Foo->add_method('bar' => undef);

ok($Foo->has_method('bar'), '... $Foo.has_method(bar)');
throws_ok { 
    $Foo->get_method('bar')->() 
} qr/^Stub Method\!/, '... $Foo.get_method(bar)';

is_deeply(
    [ sort $Foo->get_method_list ],
    [ 'bar', 'foo' ],
    '... got the list of $Foo methods');

$Foo->add_method('baz' => ::make_class_method(sub { $::ROLE }));

ok($Foo->has_method('baz'), '... $Foo.has_method(baz)');
is($Foo->get_method('baz')->(), $Foo, '... $Foo.get_method(baz)');

is_deeply(
    [ sort $Foo->get_method_list ],
    [ 'bar', 'baz', 'foo' ],
    '... got the list of $Foo methods');
    
# dont bother calling the submethod it will break
$Foo->add_method('bing' => ::make_submethod(sub { 'Foo::bing (sub method)' }));    
    
is_deeply(
    [ sort $Foo->get_method_list ],
    [ 'bar', 'baz', 'bing', 'foo' ],
    '... got the list of $Foo methods');    
   
# make sure all our methods are still of the same type
    
isa_ok($Foo->get_method('foo'), 'Perl6::Method');  
isa_ok($Foo->get_method('bar'), 'Perl6::StubMethod');    
isa_ok($Foo->get_method('baz'), 'Perl6::ClassMethod');    
isa_ok($Foo->get_method('bing'), 'Perl6::Submethod');    
    
    