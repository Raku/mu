use v6-alpha;

use Test;

=kwid

The value of Pugs::Internals::pi is {obj:Code::Exp} rather than 3.14... .
This was caused by renaming pi in Prim.hs from "pi" to "Pugs::Internals::pi".
r13255.

  % ./pugs
  pugs> Pugs::Internals::pi
  {obj:Code::Exp}
  pugs> 

=cut

plan 1;

ok eval('3.14 < Pugs::Internals::pi() < 3.15'),'Pugs::Internals::pi works, bug fixed.';
