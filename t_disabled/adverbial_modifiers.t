#!/usr/bin/pugs

use v6;
use Test;

plan 10;

eval_is 'infix:<..>(1, 10, by => 2)', <1 3 5 7 9>, 'range operator, :by parameter, long name', :todo<feature>;
eval_is '1..10 :by<2>', <1 3 5 7 9>, 'range operator, :by adverb, space', :todo<feature>;
eval_is '1..10:by<2>', <1 3 5 7 9>, 'range operator, :by adverb, without space', :todo<feature>;

eval_is 'infix:<...>(1, by => 2)[0..4]', <1 3 5 7 9>, 'infinite range operator, long name', :todo<feature>;
eval_is '1... :by<2>[0..4]', <1 3 5 7 9>, 'infinite range operator, :by adverb, space', :todo<feature>;
eval_is '1...:by<2>[0..4]', <1 3 5 7 9>, infinite range operator, :by adverb, without space', :todo<feature>;

# XXX need to test prefix:<=> on $handle with :prompt adverb

sub prefix:<blub> (Str $foo, Int +$times = 1) {
	("BLUB" x $times) ~ $foo;
}

eval_is 'prefix:<blub>("bar")', 'BLUBbar', 'user-defined prefix operator, long name', :todo<feature>;
eval_is 'prefix:<blub>("bar", times => 2)', 'BLUBBLUBbar', 'user-defined prefix operator, long name, optional parameter';
eval_is 'blub "bar"', 'BLUBbar', 'user-defined prefix operator, basic call';
eval_is 'blub "bar" :times<2>', 'BLUBBLUBbar', 'user-defined prefix operator, :times adverb, space';
eval_is 'blub "bar":times<2>', 'BLUBBLUBbar', 'user-defined prefix operator, :times adverb, no space';
