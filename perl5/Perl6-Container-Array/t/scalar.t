#!/usr/bin/perl -w

use strict;
use warnings;

use Test::More;
plan tests => 12;

use Perl6::Container::Scalar;
use Perl6::Value;

can_ok('Scalar', 'new');
ok(Scalar->isa('Perl6::Object'), '... Scalar isa Perl6::Object');

{
    my $n = Scalar->new( '$.value' => Num->new( '$.value' => 3.3 ) );
  
    isa_ok($n, 'Scalar');
    can_ok($n, 'value');
    isa_ok($n->value, 'Num', '... fetch Num');
    is($n->value->value, 3.3, '... got the unboxed value');
    is($n->defined->str->value, 'bool::true', '... defined');

    # TODO - ref
    # my $class = $n->ref;
    # warn 'class '. $class. " ". ref($class);
    # warn 'class ident '. $class->identifier;
    # XXX is($n->ref->name, 'Num', '... get the class name');

    # undefine
    $n->undefine;
    is($n->defined->str->value, 'bool::false', '... defined() is false');
}

{
    my $n = Scalar->new();
  
    isa_ok($n, 'Scalar');
    can_ok($n, 'value');
    is($n->value, undef, '... type is undef');
    is($n->defined->str->value, 'bool::false', '... defined() is false');
    # TODO - ref
}
