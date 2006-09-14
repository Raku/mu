use v6-alpha;

use Test;

=kwid

This

  class C {} sub f() {C.new()}
  use Test;

when precompiled

  ./pugs -Iblib6/lib -C Parse-YAML mumble.pm > /dev/null

fails with

  pugs: *** No compatible subroutine found: "&Test::import"

But this

  use Test;
  class C {} sub f() {C.new()}

works fine.
Also this

  class C {} sub f() {}
  use Test;

We "use Test;" here simply because it is a module known to be available.
Use()ing other modules behaves the same.

Looks like a compiler bug?

This test takes a long time (it precompiles Test, perhaps twice),
has the path 't/xx-uncategorized/' hardwired,
and is perhaps OS dependent (maybe? - system('cmd > junk') must work).
Thus perhaps it should go away once the bug is fixed, rather than
being integrated with the test suite?.

=cut

plan 2;

ok system('./pugs -Iblib6/lib -C Parse-YAML t/xx-uncategorized/use_import_not_found_1.pm > t/xx-uncategorized/use_import_not_found.junk'),'This already works.';

ok system('./pugs -Iblib6/lib -C Parse-YAML t/xx-uncategorized/use_import_not_found_2.pm > t/xx-uncategorized/use_import_not_found.junk'),'If this works, the bug has been fixed.';
