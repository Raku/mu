#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 26;
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
{

    my $Foo;
    lives_ok {
        $Foo = class 'Foo-0.0.1-cpan:STEVAN' => sub {    
            $::CLASS->superclasses([ $::Object ]);
            foreach my $name (qw(foo bar)) {
                $::CLASS->add_method($name => ::make_class_method(sub { "Hello from $name" }, $::CLASS));
            }
        };    
    } '... created a class with the closure form';

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

}

{
    
    my $Foo;
    lives_ok {
        $Foo = class 'Foo-0.0.1-cpan:STEVAN' => {
            is => [ $::Object ],
            class_methods => {
                foo => sub { "Hello from foo" },
                bar => sub { "Hello from bar" },          
            }
        }; 
    } '... created a class with the hash form';           
    

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
}
