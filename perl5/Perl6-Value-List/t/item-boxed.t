#!/usr/bin/perl -w

use strict;
use warnings;

use Test::More;
plan tests => 8;
 
use Perl6::Value;

can_ok('Int', 'new');
ok(Int->isa('Perl6::Object'), '... Int isa Perl6::Object');

{
    my $p = Num->new( '$.value' => 3.3 );
    isa_ok($p, 'Num');
    can_ok($p, 'value');
    is($p->value(), 3.3, '... got the unboxed value');

    my $i = $p->int();
    isa_ok($i, 'Int');
    can_ok($i, 'value');
    is($i->value(), 3, '... got the unboxed value');
}
