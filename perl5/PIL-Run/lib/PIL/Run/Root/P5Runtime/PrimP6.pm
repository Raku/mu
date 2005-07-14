
# this file is currently(?) not actually loaded.

module PIL::Run::Root::P5Runtime::PrimP6-0.0.1;
use v6;

=kwid

This file contains p5 runtime primitives which are written in p6.
Most will eventually be implemented in perl6/Prelude.pm, and can
then be removed from here.

See the note at the top of Prelude.pm.

=cut

multi sub nothing () is builtin is primitive is safe {
    bool::true}

