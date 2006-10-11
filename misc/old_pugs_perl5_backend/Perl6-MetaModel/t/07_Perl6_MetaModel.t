#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 34;
use Test::Exception; 

use Perl6::MetaModel;

# make sure genesis loaded correctly
ok($::Object->isa('Object'), '... genesis was loaded ok');

ok($::Package->isa('Package'), '... genesis was loaded ok');
ok($::Package->isa('Object'), '... genesis was loaded ok');

ok($::Module->isa('Module'), '... genesis was loaded ok');
ok($::Module->isa('Package'), '... genesis was loaded ok');
ok($::Module->isa('Object'), '... genesis was loaded ok');

ok($::Class->isa('Class'), '... genesis was loaded ok');
ok($::Class->isa('Module'), '... genesis was loaded ok');
ok($::Class->isa('Package'), '... genesis was loaded ok');
ok($::Class->isa('Object'), '... genesis was loaded ok');
      
## class-is-a-closure form 
{

    my $Foo;
    lives_ok {
        $Foo = class 'Foo-0.0.1-cpan:STEVAN' => sub {    
            $::CLASS->superclasses([ $::Object ]);
            foreach my $name (qw(foo bar)) {
                $::CLASS->add_singleton_method($name => ::make_method(sub { "Hello from $name" }));
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

    ok($Foo->is_a($::Object), '... $Foo is a Object');
                                    
    ok($Foo->class->has_method('foo'), '... $Foo has a foo method');
    ok($Foo->class->has_method('bar'), '... $Foo has a bar method');

    is($Foo->foo(), 'Hello from foo', '... $Foo->foo() generated method worked great');
    is($Foo->bar(), 'Hello from bar', '... $Foo->bar() generated method worked great');

}

{
    
    my $Foo;
    lives_ok {
        $Foo = class 'Foo-0.0.1-cpan:STEVAN' => {
            is => [ $::Object ],
            attributes => [ '$.baz' ],
            class_methods => {
                foo => sub { "Hello from foo" },
                bar => sub { "Hello from bar" },          
            },
            methods => {
                baz => sub { _('$.baz') },
                bad => sub { _('$.bad') }, # this will die                        
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

    ok($Foo->is_a($::Object), '... $Foo is a Object');
                                    
    ok($Foo->class->has_method('foo'), '... $Foo has a foo method');
    ok($Foo->class->has_method('bar'), '... $Foo has a bar method');

    is($Foo->foo(), 'Hello from foo', '... $Foo->class::foo() generated method worked great');
    is($Foo->bar(), 'Hello from bar', '... $Foo->class::bar() generated method worked great'); 
    
    is($Foo->new('$.baz' => 42)->baz(), 42, '... $.baz attribute set and accessed correctly');   
    dies_ok { $Foo->new()->bad() } '... cannot access attributes which do not exist';
}
