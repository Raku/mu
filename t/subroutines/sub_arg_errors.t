use v6-alpha;

use Test;

plan 6;

=pod

These are misc. sub argument errors.

=cut

sub foo (*$x) { 1 }
dies_ok  { foo(reverse(1,2)) }, 'slurpy args are now bounded (1)';

sub bar (*@x) { 1 }
lives_ok { bar(reverse(1,2)) }, 'slurpy args are now bounded (2)';  

eval_dies_ok 'sub baz ($.x) { ... }', 'parser rejects members as args (1)';

class Moo {
    has $.y;
    eval_dies_ok 'sub quux ($.x) { ... }', 'parser rejects members as args (2)';
}

eval_dies_ok 'sub quuux ($?VERSION) { ... }', 'parser rejects magicals as args (1)';
eval_dies_ok 'sub quuux ($!) { ... }',        'parser rejects magicals as args (2)';
