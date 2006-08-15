#!/usr/bin/perl -w

use strict;
use warnings;

use Test::More;
plan tests => 27;

use Pugs::Runtime::Container::Scalar;
use Pugs::Runtime::Value;

can_ok('Scalar', 'new');
ok(Scalar->isa('Pugs::Runtime::Object'), '... Scalar isa Pugs::Runtime::Object');

{
    my $n = Scalar->new;
    $n->store( Num->new( '$.unboxed' => 3.3 ) );
  
    isa_ok($n, 'Scalar');
    can_ok($n, 'fetch');
    isa_ok($n->fetch, 'Num', '... fetch Num');
    is($n->unboxed, 3.3, '... got the unboxed value');
    is($n->defined->str->unboxed, bool::true, '... defined() is true');

    # .bind($other_scalar)
    my $m = Scalar->new;
    $m->bind( $n );
    is($n->unboxed, 3.3, '... got the unboxed value from the other Scalar');
    $m->increment;
    is($m->unboxed, 4.3, '... increment both Scalars');
    is($n->unboxed, 4.3, '... increment both Scalars');
    $n->bind( Scalar->new );
    $m->increment;
    is($m->unboxed, 5.3, '... unbind, increment only one Scalar');
    is($n->unboxed, undef, '... Scalar didn\'t change');
}

{
    # access control

    # NOTE - test removed - this is now controlled by a trait
    # See: t/trait.t
    # my $n = Scalar->new;
    # $n->access('ro');
    # $n->store( Num->new( '$.unboxed' => 3.3 ) );
    # is( $n->unboxed, 3.3, '... stored a value in a read only container');

    # clone
    # my $m = $n->clone;
;}

{
    # TODO - ref
    # my $class = $n->ref;
    # warn 'class '. $class. " ". ref($class);
    # warn 'class ident '. $class->identifier;
    # XXX is($n->ref->name, 'Num', '... get the class name');

    # warn $n->ref;  # Pugs::Runtime::Class=HASH(0x8269888)

    # warn $n->ref->id;
    # warn $n->ref->identifier;
    # warn $n->ref->name;
    # warn $n->ref->version;
    # warn $n->ref->class;
    # warn $n->ref->meta;

    my $n = Scalar->new;
    $n->store( Num->new( '$.unboxed' => 3.3 ) );

    my $n2 = $n->ref->new( '$.unboxed' => 'xxx' );
    isa_ok($n2, 'Num', '.ref() is a reference to the "value" class');

    # undefine
    $n->undefine;
    is($n->defined->str->unboxed, bool::false, '... defined() is false');
}

{
    my $n = Scalar->new();
  
    isa_ok($n, 'Scalar', 'empty Scalar');
    can_ok($n, 'fetch');
    is($n->unboxed, undef, '... type is undef');
    is($n->perl->unboxed, '\\undef', '... .perl is undef');
    is($n->defined->str->unboxed, bool::false, '... defined() is false');
    $n->increment;
    is($n->unboxed, 1, '... increment defines');

    $n->increment;
    is($n->unboxed, 2, '... increment Int');

    # TODO - ref of undef
}

{  
    # dispatching methods to the cell Value ?
    
    my $p = Scalar->new();
    $p->store( Pair->new( 
                   '$.key' => Str->new( '$.unboxed' => 'a' ),
                   '$.value' =>   Num->new( '$.unboxed' => 3.3 ) 
                ) );
    is( $p->key->unboxed, 'a', '... auto dereference and retrieve Pair key' );
    is( $p->value->unboxed, 3.3, '... auto dereference and retrieve Pair value' );
    $p->value( Num->new( '$.unboxed' => 7 ) );
    is( $p->value->unboxed, 7, '... auto dereference and store Pair value' );
}

{  
    # store unboxed value
    
    my $p = Scalar->new();
    $p->store( 5 );
    is( $p->fetch, 5, '... store/fetch unboxed value' );
}

{  
    # tie
    
    # NOTE - test removed - this is now controlled by a trait
    # See: t/trait.t
    # sub Thing::new { bless {}, 'Thing' } 
    # sub Thing::store { $_[0]{v} = $_[1] }
    # sub Thing::fetch { $_[0]{v} }

    # my $t = Thing->new();

    # my $s = Scalar->new();

    # $s->tie( $t );   # dies ok

    # $s->set_tieable;
    # $s->tie( $t );

    # $s->store( 5 );
    # is( $s->fetch, 5, '... store/fetch tied value' );

    # $s->untie;
    # is( $s->fetch, undef, '... untie' );
;}

{  
    # id

    my $s = Scalar->new();
    my $t = Scalar->new();

    isnt( $s->id, $t->id, 'id is different for each Scalar');

    $t->bind( $s );
    is( $s->id, $t->id, '... same id for binded Scalars');
}
