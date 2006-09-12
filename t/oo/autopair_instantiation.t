use v6-alpha;

use Test;

plan 4;

=pod

Autopair tests with class instantiation from
L<S02/"Literals"/"There is now a generalized adverbial form">

=cut

{ # L<S02/"Literals"/"There is now a generalized adverbial form">
    eval_ok('my $a; class b { has $.a }; my b $c .= new(:$a)',
            'class instantiation with autopair, no spaces');
    eval_ok('my $a; class b { has $.a }; my b $c .= new(:$a )',
            'class instantiation with autopair, spaces');
    eval_ok('my $a; role b { has $.a }; my b $c .= new(:$a)',
            'role instantiation with autopair, no spaces');
    eval_ok('my $a; role b { has $.a }; my b $c .= new(:$a )',
            'role instantiation with autopair, spaces');
}
