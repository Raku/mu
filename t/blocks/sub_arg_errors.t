use v6;

use Test;

plan 6;

=begin pod

These are misc. sub argument errors.

=end pod

sub foo (*$x) { 1 }
dies_ok  { foo(reverse(1,2)) }, 'slurpy args are now bounded (1)';

sub bar (*@x) { 1 }
lives_ok { bar(reverse(1,2)) }, 'slurpy args are now bounded (2)';  

dies_ok eval('sub baz ($.x) { ... }'), 'parser rejects members as args (1)';

class Moo {
    has $.y;
    dies_ok eval('sub quux ($.x) { ... }'),
        'parser rejects members as args (2)';
}

dies_ok eval('sub quuux ($?VERSION) { ... }'),
    'parser rejects magicals as args (1)';
dies_ok eval('sub quuuux ($!) { ... }'),
    'parser rejects magicals as args (2)';

# vim: ft=perl6
