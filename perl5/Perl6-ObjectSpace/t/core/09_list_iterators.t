#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::Core::List');
use_ok('Perl6::Core::Closure');
use_ok('Perl6::Core::Block');

my $e = closure::env->new();

{
    my $list = list->new(num->new(1), num->new(2), num->new(3), num->new(4));
    isa_ok($list, 'list');

    my $body = closure->new(
        $e,
        closure::params->new(
            symbol->new('$val' => 'reference')
        ),
        sub {
            my $e = shift;
            my $val = $e->get('$val');
            $val->store($val->fetch->increment);
        }
    );
    isa_ok($body, 'closure');

    $list->each($body);

    is($list->fetch(num->new(0))->to_native, 2, '... the value was incremented correctly');
    is($list->fetch(num->new(1))->to_native, 3, '... the value was incremented correctly');
    is($list->fetch(num->new(2))->to_native, 4, '... the value was incremented correctly');
    is($list->fetch(num->new(3))->to_native, 5, '... the value was incremented correctly');
}

# with a block

{
    my $list = list->new(num->new(1), num->new(2), num->new(3), num->new(4));
    isa_ok($list, 'list');

    my $body = block->new(
        $e,
        sub {
            my $e = shift;
            my $val = $e->get('$_');
            $val->store($val->fetch->increment);
        }
    );
    isa_ok($body, 'block');

    $list->each($body);

    is($list->fetch(num->new(0))->to_native, 2, '... the value was incremented correctly');
    is($list->fetch(num->new(1))->to_native, 3, '... the value was incremented correctly');
    is($list->fetch(num->new(2))->to_native, 4, '... the value was incremented correctly');
    is($list->fetch(num->new(3))->to_native, 5, '... the value was incremented correctly');
}

# apply

{
    my $list = list->new(num->new(1), num->new(2), num->new(3), num->new(4));
    isa_ok($list, 'list');

    my $body = closure->new(
        $e,
        closure::params->new(
            symbol->new('$val' => 'num')
        ),
        sub {
            my $e = shift;
            $e->get('$val')->increment;
        }
    );
    isa_ok($body, 'closure');

    my $new_list = $list->apply($body);

    is($new_list->fetch(num->new(0))->to_native, 2, '... the value was incremented correctly');
    is($new_list->fetch(num->new(1))->to_native, 3, '... the value was incremented correctly');
    is($new_list->fetch(num->new(2))->to_native, 4, '... the value was incremented correctly');
    is($new_list->fetch(num->new(3))->to_native, 5, '... the value was incremented correctly');    
}

# apply with block

{
    my $list = list->new(num->new(1), num->new(2), num->new(3), num->new(4));
    isa_ok($list, 'list');

    my $body = block->new(
        $e,
        sub {
            my $e = shift;
            $e->get('$_')->increment;
        }
    );
    isa_ok($body, 'block');

    my $new_list = $list->apply($body);

    is($new_list->fetch(num->new(0))->to_native, 2, '... the value was incremented correctly');
    is($new_list->fetch(num->new(1))->to_native, 3, '... the value was incremented correctly');
    is($new_list->fetch(num->new(2))->to_native, 4, '... the value was incremented correctly');
    is($new_list->fetch(num->new(3))->to_native, 5, '... the value was incremented correctly');    
}

