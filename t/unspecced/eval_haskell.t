#!/usr/bin/pugs

use v6;
use Test;

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

is(eval('"haskell"', :lang<haskell>), 'haskell', 'string return');

eval('thisisinvalidhaskellcode', :lang<haskell>);
ok(1, 'still running after bad haskell code');
like($!, rx:perl5/thisisinvalidhaskellcode/, 'throws something useful on error');

