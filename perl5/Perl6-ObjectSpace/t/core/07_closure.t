#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::Core::Closure');
use_ok('Perl6::Core::Hash');
use_ok('Perl6::Core::Num');

my $top_level_env = closure::env->new();
isa_ok($top_level_env, 'closure::env');

$top_level_env->set('$foo' => num->new(10));

{

    my $closure = closure->new($top_level_env, hash->new(str->new('$num') => num->new(50)), sub { 
        my $e = shift;
        return list->new($e->get('$num'), $e->get('$foo'));
    });

    {
        my $results = $closure->do(hash->new(str->new('$num') => num->new(100)));
        isa_ok($results, 'list');

        isa_ok($results->fetch(num->new(0)), 'num');
        cmp_ok($results->fetch(num->new(0))->to_native, '==', 100, '... got one of the expected values');
    
        isa_ok($results->fetch(num->new(1)), 'num');
        cmp_ok($results->fetch(num->new(1))->to_native, '==', 10, '... got one of the expected values');
    }

    $top_level_env->set('$foo' => num->new(35));

    {
        my $results = $closure->do(hash->new());
        isa_ok($results, 'list');

        isa_ok($results->fetch(num->new(0)), 'num');
        cmp_ok($results->fetch(num->new(0))->to_native, '==', 50, '... got one of the expected values');

        isa_ok($results->fetch(num->new(1)), 'num');
        cmp_ok($results->fetch(num->new(1))->to_native, '==', 35, '... got one of the expected values');
    }

}

=pod

NOTES:

----------------------------------------------------------------------

Maybe the env needs to only accept ref() types, this will allow 
updating of variables since most of the core objects are immutable
to some degree or another.

The question, should the ref()ing be automatic? or not. I think so, 
since they will always need a layer of immutability.

This means though that we need to deal with stuff in ref() to allow
for this to be done easily. 

my $number = ref->new(num->new(100));

$number->deref()->to_str(); # deref to call method
$number->deref(num->new(200)); # deref to re-assign

----------------------------------------------------------------------

Maybe I need to add some sort of ablity to compare, this could be
partially implemented in type:: and each one could supply the compare
routine or something (like Class::Comparable)

----------------------------------------------------------------------

Maybe I should have a Perl6::Core module which loads all my modules
and provides a macro layer like this:

::num(10)
::str("hello world")
::bit(1)

(actually for bit() maybe I should make constants for true and false)

(maybe also cache numbers too with num)

::list(1, 2, ::num(100), "testing this", ::list(1, 2, 3));

(::list would then loop through converting stuff, looks_like_number 
could be used to weed out the numbers from the strings)

::hash('test' => "hello")

(same principle for hash(), but we do the keys as strings always)

$hash->store(::str("key") => ::num(100))

Thats not too bad,.. it looks like of like type annotations actually.

=cut




