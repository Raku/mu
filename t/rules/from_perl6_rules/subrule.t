#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/subrule.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 31;

if(eval('!("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

rule abc {abc}


rule once {<?abc>}

ok("abcabcabcabcd" ~~ m/<?once>/, 'Once match');
ok($/, 'Once matched');
is($/, "abc", 'Once matched');
ok(@$/ == 0, 'Once no array capture');
ok(%$/.keys == 0, 'Once no hash capture');


rule rep {<?abc>**{4}}

ok("abcabcabcabcd" ~~ m/<?rep>/, 'Rep match');
ok($/, 'Rep matched');
is($/, "abcabcabcabc", 'Rep matched');
ok(@$/ == 0, 'Rep no array capture');
ok(%$/.keys == 0, 'Rep no hash capture');


rule cap {<abc>}

ok("abcabcabcabcd" ~~ m/<cap>/, 'Cap match');
ok($/, 'Cap matched');
is($/, "abc", 'Cap zero matched');
is($/<cap>, "abc", 'Cap captured');
is($/<cap><abc>, "abc", 'Cap abc captured');
ok(@$/ == 0, 'Cap no array capture');
ok(%$/.keys == 1, 'Cap hash capture');


rule repcap {<abc>**{4}}

ok("abcabcabcabcd" ~~ m/<repcap>/, 'Repcap match');
ok($/, 'Repcap matched');
is($/, "abcabcabcabc", 'Repcap matched');
is($/<repcap>, "abcabcabcabc", 'Repcap captured');
is(eval('$/<repcap><abc>[0]'), "abc", 'Repcap abc zero captured', :todo<bug>);
is(eval('$/<repcap><abc>[1]'), "abc", 'Repcap abc one captured', :todo<bug>);
is(eval('$/<repcap><abc>[2]'), "abc", 'Repcap abc two captured', :todo<bug>);
is(eval('$/<repcap><abc>[3]'), "abc", 'Repcap abc three captured', :todo<bug>);
ok(@$/ == 0, 'Repcap no array capture');


rule caprep {(<?abc>**{4})}

ok("abcabcabcabcd" ~~ m/<caprep>/, 'Caprep match');
ok($/, 'Caprep matched');
is($/, "abcabcabcabc", 'Caprep matched');
is($/<caprep>, "abcabcabcabc", 'Caprep captured');
is(eval('$/<caprep>[0]'), "abcabcabcabc", 'Caprep abc one captured');

}

