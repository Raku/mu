#!/usr/bin/perl -w

use strict;
use warnings;

use Test::More;
plan tests => 22;
 
use Perl6::Value;

can_ok('Int', 'new');
ok(Int->isa('Perl6::Object'), '... Int isa Perl6::Object');

{
    my $n = Num->new( '$.value' => 3.3 );
    isa_ok($n, 'Num');
    can_ok($n, 'value');
    is($n->value(), 3.3, '... got the unboxed num value');

    my $i = $n->int();
    isa_ok($i, 'Int');
    can_ok($i, 'value');
    is($i->value(), 3, '... got the unboxed int value');

    my $b = $n->bit();
    isa_ok($n, 'Num');
    isa_ok($b, 'Bit');
    can_ok($b, 'value');
    is($b->value(), 1, '... got the unboxed bit value');

    my $s = $b->str();
    isa_ok($s, 'Str');
    can_ok($s, 'value');
    is($s->value(), 'bool::true', '... Bit to Str');
}

{
    # Inf
    my $n = Num->Inf;
    isa_ok($n, 'Num');
    is($n->value(), &Perl6::Value::Num::Inf, '... Inf');
}

{
    # Pair
    my $p = Pair->new(
                '$.key' =>   Str->new( '$.value' => 'a' ),
                '$.value' => Str->new( '$.value' => 'x' ) );
    isa_ok($p, 'Pair', 'Pair');
    can_ok($p, 'value');
    can_ok($p, 'key');
    is($p->key()->value(),   'a', '... got the key');
    is($p->value()->value(), 'x', '... got the value');
}
