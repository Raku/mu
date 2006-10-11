#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::MetaModel::Parser::Env');


my $env = Perl6::MetaModel::Parser::Env->new();
isa_ok($env, 'Perl6::MetaModel::Parser::Env');

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

$env->set(
    'foo' => 10,
    'bar' => 11
    );
is($env->get('foo'), 10, '... foo is 10');
is($env->get('bar'), 11, '... bar is 11');

$env->enter_scope;

    is($env->get('foo'), 10, '... foo is still 10');
    is($env->get('bar'), 11, '... bar is still 11');

    $env->set('foo' => 20);
    
    is($env->get('foo'), 20, '... foo is now 20');
    is($env->get('bar'), 11, '... bar is still 11');    
    
    $env->enter_scope;

        is($env->get('foo'), 20, '... foo is still 20');
        is($env->get('bar'), 11, '... bar is still 11');    

        $env->set('bar' => 30);    
    
        is($env->get('foo'), 20, '... foo is still 20');
        is($env->get('bar'), 30, '... bar is now 30');        
    
    $env->leave_scope;
    
    is($env->get('foo'), 20, '... foo is still 20');
    is($env->get('bar'), 11, '... bar is back to 11');       

$env->leave_scope;

is($env->get('foo'), 10, '... foo is back to 10');
is($env->get('bar'), 11, '... bar is 11');

