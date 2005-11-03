#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::MM::Opaque');
use_ok('Perl6::MM::Method');

my $o = opaque->new(
            reference->new($opaque::NULL_OBJECT), 
            hash->new(
                str->new('$.one') => nil->new()
            )
        );
isa_ok($o, 'opaque');

my $e = closure::env->new();

{
    my $m = method->new($e, 
        closure::params->new(
            symbol->new('$self:' => 'opaque')
        ), 
        sub { 
            my $e = shift;
            return $e->get('$?SELF');
        }
    );
    isa_ok($m, 'method');
    
    my $result = $m->do(list->new($o));
    isa_ok($result, 'opaque');
    is($result, $o, '... we got the invocant back');
}

{
    my $m = method->new($e, 
        closure::params->new(
            symbol->new('$self:' => 'opaque')
        ), 
        sub { 
            my $e = shift;
            return $e->get('$?CLASS');
        }
    );
    isa_ok($m, 'method');

    my $result = $m->do(list->new($o));
    isa_ok($result, 'opaque');
    is($result, $opaque::NULL_OBJECT, '... we got the class back');
}

{
    my $m = method->new($e, 
        closure::params->new(
            symbol->new('$self:' => 'opaque')
        ),         
        sub { 
            my $e = shift;
            return $e->get('$?PACKAGE');
        }
    );
    isa_ok($m, 'method');

    my $result = $m->do(list->new($o));
    isa_ok($result, 'opaque');
    is($result, $opaque::NULL_OBJECT, '... we got the class back');
}



