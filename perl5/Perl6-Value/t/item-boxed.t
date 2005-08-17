#!/usr/bin/perl -w

use strict;
use warnings;

use Test::More;
# use Test::Exception;
plan tests => 33;
 
use Perl6::Value;

can_ok('Int', 'new');
ok(Int->isa('Perl6::Object'), '... Int isa Perl6::Object');

{
    my $n = Num->new( '$.unboxed' => 3.3 );
    isa_ok($n, 'Num');
    can_ok($n, 'num');
    is($n->unboxed(), 3.3, '... got the unboxed num value');

    my $i = $n->int();
    isa_ok($i, 'Int');
    can_ok($i, 'int');
    is($i->unboxed(), 3, '... got the unboxed int value');

    my $b = $n->bit();
    isa_ok($n, 'Num');
    isa_ok($b, 'Bit');
    can_ok($b, 'bit');
    is($b->unboxed(), 1, '... got the unboxed bit value');

    my $s = $b->str();
    isa_ok($s, 'Str');
    can_ok($s, 'str');
    is($s->unboxed(), 'bool::true', '... Bit to Str');
}

{
    # Inf
    my $n = Num->Inf;
    isa_ok($n, 'Num');
    is($n->unboxed(), &Perl6::Value::Num::Inf, '... Inf');
}

{
    # Pair
    my $p = Pair->new(
                '$.key' =>   Str->new( '$.unboxed' => 'a' ),
                '$.value' => Str->new( '$.unboxed' => 'x' ) );
    isa_ok($p, 'Pair', 'Pair');
    can_ok($p, 'value');
    can_ok($p, 'key');
    is($p->perl->unboxed, "('a', 'x')", '... got .perl');
    is($p->key()->unboxed(),   'a', '... got the key');
    is($p->value()->unboxed(), 'x', '... got the value');
    like($p->id(), qr/\d+/, '... got the object id');

    $p->value( Num->new( '$.unboxed' => 7 ) );
    is( $p->value->unboxed, 7, '... Pair value is rw' );

    #my $class_name = ::dispatch( ::meta( $p ), 'name' );  # ** get the class name, from object
    #my $class = $Perl6::Class::ALL_CLASSES{$class_name};  # ** get the class, from class name
    #warn $class;

    # my $class = ::meta( $p );

    my $class = $p->ref;
    my $q = $class->new(
                '$.key' =>   Str->new( '$.unboxed' => 'a' ),
                '$.value' => Str->new( '$.unboxed' => 'x' ) );
    isa_ok($q, 'Pair', 'create a new object from the reference');
}

{
    # Ref to undef
    my $r = Ref->new( '$.referred' => undef );
    isa_ok($r, 'Ref', 'reference to undef' );
    is($r->perl->unboxed, '\undef', '... reference to undef does .perl' );

    # reference value is not assignable
    #my $n = Num->new( '$.unboxed' => 3.3 );
    #eval { $r->store( $n ) };
    #like( $@, qr/read-only/, '... Ref is read-only' );
}

{
    # Ref to unboxed value
    my $r = Ref->new( '$.referred' => 5 );
    isa_ok($r, 'Ref', 'reference to unboxed value' );
    is($r->referred, '5', '... got referred value' );

    # reference value is not assignable
    #my $n = Num->new( '$.unboxed' => 3.3 );
    #eval { $r->store( $n ) };
    #like( $@, qr/read-only/, '... Ref is read-only' );
}

if(0){  # -- belongs to 'Container'
    # Scalar undef
    my $r = Scalar->new( '$.unboxed' => undef );
    isa_ok($r, 'Scalar', 'reference to undef' );
    is($r->perl->value, '\undef', '... reference to undef does .perl' );

    # scalar value is assignable
    my $n = Num->new( '$.unboxed' => 3.3 );
    $r->value( $n );
    is($r->perl->value, '\3.3', '... Scalar is read-write' );
}

{
    my $n = Num->new( '$.unboxed' => 3.3 );
    isa_ok($n, 'Num');
    my $r = Ref->new( '$.referred' => $n );
    isa_ok($r, 'Ref', 'reference to a value' );
    is($r->perl->unboxed, '\3.3', '... reference to a value does .perl' );

    # TODO - reference to Array

    # is($n->value(), 3.3, '... got the unboxed num value');
}

=for later

    warn ::meta('Pair');  # get metaclass from class name
    warn $p->ref;  # get class
    # warn $p->meta;  # get meta class
    warn ::meta( $p->ref );  # get metaclass from class ref
    warn ::dispatch( ::meta( $p->ref ), 'name' );  # get the class name from object
    warn ::meta( $p );
    warn ::dispatch( ::meta( $p ), 'name' );  # ** get the class name from object
    warn ::dispatch( ::meta( $p ), 'class' );  # 
    warn $p->ref->superclasses;
    warn $p->ref->meta;  # get class
    warn ::dispatch( ::meta('Pair'), 'name' );
    like($p->name(), 'xxx', '... got the object class name');

=cut

