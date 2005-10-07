#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';
use Test::Deep;
use Test::Exception;

use lib 't/lib';

use Blondie::Nodes;
use Blondie::Runtime::Nada;

my $m; BEGIN { use_ok($m = "Blondie::Compiler") }

isa_ok(my $o = $m->new, $m);

my $n = Blondie::Runtime::Nada->new;

{
    my $env = Blondie::Env->new(
        '&func' => Thunk('moose'),
    );

    my $prog = App( Sym('&func') );

    my $c = $o->compile($n, $env, $prog);

    cmp_deeply(
        $c,
        App( Val( $env->get('&func') )),
        "symbol resolved during compile time",
    );
}


{
    my $env = Blondie::Env->new(
        '&func' => Thunk(
            Seq(
                Param('$x'),
                Sym('$x'),
            ),
        ),
    );

    my $prog = App(
        Sym('&func'),
        Val(1),
    );

    my $c = $o->compile($n, $env, $prog);

    cmp_deeply(
        $c,
        App( Val( $env->get('&func') ), Val(1) ),
        "dynamic symbol inside function delayed till runtime because it's a param",
    );
}

{
    my $env = Blondie::Env->new(
        stub '&func',
    );

    my $prog = App( Sym('&func') );

    throws_ok {
        my $c = $o->compile($n, $env, $prog);
    } qr/stub/, "can't use a function that isn't unstubbed";
}

