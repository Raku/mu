#!/usr/bin/pugs

use v6;
use Test;

plan 14;

{
    # { } has implicit signature ($_ is rw = $OUTER::_)
    
    $_ = 'Hello';
    eval_is('{ $_ }.()', 'Hello',              '$_ in bare block defaults to outer');
    is({ $_ }.('Goodbye'), 'Goodbye',   'but it is only a default');
    is({ 42 }.(), 42,                   'no implicit $_ usage checking');
    is({ 42 }.('Goodbye'), 42,          '$_ gets assigned but isn\'t used');

    is({ $_ }.arity, 1,                 '{$_} is arity 1, of course');
    is({ 42 }.arity, 1,                 'Even blocks that don\'t use $_ have arity 1');
}

{
    dies_ok(sub () { -> { "Boo!" }.(42) },     '-> {} is arity 0');
    dies_ok(sub () { -> { $_ }.(42) },         'Even when we use $_');
    
    eval_is('$_ = "Ack"; -> { $_ }.()', 'Ack!',     '$_ is lexical here');
    is(-> $a { $_ }.(42), 'Ack!',       'Even with parameters (?)');
    is(-> $_ { $_ }.(42), 42,           'But not when the parameter is $_');

    dies_ok( sub () { -> { $^a }.() },  'Placeholders not allowed in ->');

    is(-> { }.arity, 0,                 '->{} is arity 0, again');
}

{
    dies_ok(sub () { sub { $^foo }.(42) },  'Placeholders not allowed in sub()');
}
