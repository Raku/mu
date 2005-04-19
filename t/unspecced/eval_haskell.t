#!/usr/bin/pugs

use v6;
require Test;

plan(4);

if ($*PUGS_HAS_HSPLUGINS eq '0') {
   is($*PUGS_HAS_HSPLUGINS, 0, '$*PUGS_HAS_HSPLUGINS is sane (0)');
   skip_rest;
   exit;
} elsif ($*PUGS_HAS_HSPLUGINS ne '1') {
   is($*PUGS_HAS_HSPLUGINS, 1, '$*PUGS_HAS_HSPLUGINS is insane!');
   skip_rest;
   exit;
} else {
   is($*PUGS_HAS_HSPLUGINS, 1, '$*PUGS_HAS_HSPLUGINS is sane (1)');
}

is(eval_haskell('"haskell"'), 'haskell', 'string return');

dies_ok(eval_haskell('thisisinvalidhaskellcode'), 'throws on error');
throws_ok(eval_haskell('thisisinvalidhaskellcode'),
 'thisisinvalidhaskellcode', 'throws something useful on error')
