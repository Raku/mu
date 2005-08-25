
# XXX - doesnt work quite yet...
#module PIL::Run::Root::P5Runtime::PrimP6-0.0.1;
use v6;

=kwid

This file contains p5 runtime primitives which are written in p6.
Most will eventually be implemented in perl6/Prelude.pm, and can
then be removed from here.

See the note at the top of Prelude.pm.

=cut

multi sub nothing () is builtin is primitive is safe {
    bool::true}

multi sub postcircumfix:<[ ]> ($a,$i) { Array::fetch($a,$i) }

# TODO - string versions
multi sub infix:<..^> ($x0,$x1) { $x0..($x1-1) };
multi sub infix:<^..> ($x0,$x1) { ($x0+1)..$x1 };
multi sub infix:<^..^> ($x0,$x1) { ($x0+1)..($x1-1) };
