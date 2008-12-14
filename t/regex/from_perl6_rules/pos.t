use v6;
use Test;

=begin pod

This file was originally derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/pos.t.

=end pod

plan 9;

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

force_todo(2,4,6,8,9);

my $str = "abrAcadAbbra";

ok($str ~~ m/ a .+ A /, 'Match from start');
ok(eval(q{$/.to == 0}), 'Match pos is 0');

ok($str ~~ m/ A .+ a /, 'Match from 3');
ok(eval(q{$/.to == 3}), 'Match pos is 3');

ok(!( $str ~~ m/ Z .+ a / ), 'No match');
ok(eval(q{!defined($/.to)}), 'Match pos is undef');

regex Aa { A .* a }
ok($str ~~ m/ .*? <Aa> /, 'Subrule match from 3');
ok(eval(q{$/.to == 0}), 'Full match pos is 0');
ok(eval(q{$/<Aa>.to == 3}), 'Subrule match pos is 3');

}

