#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 14;
use Test::Exception; 

use Perl6::MetaModel;

# make sure genesis loaded correctly
ok($::Object->isa('Object'), '... genesis was loaded ok');
ok($::Class->isa('Class'), '... genesis was loaded ok');
ok($::Class->isa('Object'), '... genesis was loaded ok');
is_deeply(
    $::Class->superclasses,
    [ $::Object ],
    '... genesis was loaded ok');
      
## class-is-a-closure form 

my $Foo = class 'Foo-0.0.1-cpan:STEVAN' => sub {
    my $class = shift;
    $class->superclasses([ $::Object ]);
    foreach my $name (qw(foo bar)) {
        $class->add_method($name => ::make_class_method(sub { "Hello from $name" }, $class));
    }
};    

is($Foo->name, 'Foo', '... $Foo has the right name');
is($Foo->version, '0.0.1', '... $Foo has the right version');
is($Foo->authority, 'cpan:STEVAN', '... $Foo has the right authority');

is($Foo->identifier, 'Foo-0.0.1-cpan:STEVAN', '... $Foo has the right identifier');

is_deeply(
    $Foo->superclasses,
    [ $::Object ],
    '... $Foo has the right superclasses');

ok($Foo->is_a('Object'), '... $Foo is a Object');

ok($Foo->has_method('foo', (for => 'class')), '... $Foo has a foo method');
ok($Foo->has_method('bar', (for => 'class')), '... $Foo has a bar method');

is($Foo->class::foo(), 'Hello from foo', '... $Foo->class::foo() generated method worked great');
is($Foo->class::bar(), 'Hello from bar', '... $Foo->class::bar() generated method worked great');
