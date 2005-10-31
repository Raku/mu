#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::Core::Closure');

my $env = closure::env->new();
isa_ok($env, 'closure::env');

=pod

my $foo = 10;
my $bar = 11;
{
    my $foo = 20;
    {
        my $bar = 30
    }
}

=cut

$env->create('foo' => num->new(10));
is($env->get('foo')->to_native, 10, '... foo is 10');

$env->create('bar' => num->new(11));
is($env->get('bar')->to_native, 11, '... bar is 11');

my $env2 = closure::env->new();
isa_ok($env2, 'closure::env');
$env2->next($env);

    is($env2->get('foo')->to_native, 10, '... foo is still 10');
    is($env2->get('bar')->to_native, 11, '... bar is still 11');

    $env2->create('foo' => num->new(20));

    is($env2->get('foo')->to_native, 20, '... foo is now 20');
    is($env2->get('bar')->to_native, 11, '... bar is still 11');    

    my $env3 = closure::env->new();
    isa_ok($env3, 'closure::env');
    $env3->next($env2);

        is($env3->get('foo')->to_native, 20, '... foo is still 20');
        is($env3->get('bar')->to_native, 11, '... bar is still 11');    

        $env3->create('bar' => num->new(30));    

        is($env3->get('foo')->to_native, 20, '... foo is still 20');
        is($env3->get('bar')->to_native, 30, '... bar is now 30');        

    is($env2->get('foo')->to_native, 20, '... foo is still 20');
    is($env2->get('bar')->to_native, 11, '... bar is back to 11');       

is($env->get('foo')->to_native, 10, '... foo is back to 10');
is($env->get('bar')->to_native, 11, '... bar is 11');

