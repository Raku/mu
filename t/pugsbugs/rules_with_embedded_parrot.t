#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from failing tests in t/rules/from_perl6_rules/subrule.t.
They work with external parrot, but not with embedded parrot.
(As of r6222, on linux FC3 amd64).

Be careful editing this pod.  It's contents can make the test not fail. :(

The test succeeds if the cap or mumble rules are commented out.
In cap, abcnot and alsoabc fail, but with a different name, and with
many versions of this sentence here, it doesnt.  alsonotabc doesnt.

=cut

plan 1;

if(!eval('("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

rule abc {abc}

rule once {<?abc>}

rule mumble {<notabc>}

ok("abcabcabcabcd" ~~ m/<?once>/, 'Once match');


}

