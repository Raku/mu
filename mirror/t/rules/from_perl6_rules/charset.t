#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/charset.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 22;

if(!eval('("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

ok("zyxaxyz" ~~ m/(<[aeiou]>)/, 'Simple set');
is($0, 'a', 'Simple set capture');
ok(!( "a" ~~ m/<-[aeiou]>/ ), 'Simple neg set failure');
ok("f" ~~ m/(<-[aeiou]>)/, 'Simple neg set match');
is($0, 'f', 'Simple neg set capture');

ok(!( "a" ~~ m/(<[a..z]-[aeiou]>)/ ), 'Difference set failure');
ok("y" ~~ m/(<[a..z]-[aeiou]>)/, 'Difference set match', :todo<feature>);
is($0, 'y', 'Difference set capture', :todo<feature>);
ok(!( "a" ~~ m/(<+<?alpha>-[aeiou]>)/ ), 'Named difference set failure');
ok("y" ~~ m/(<+<?alpha>-[aeiou]>)/, 'Named difference set match', :todo<feature>);
is($0, 'y', 'Named difference set capture', :todo<feature>);
ok(!( "y" ~~ m/(<[a..z]-[aeiou]-[y]>)/ ), 'Multi-difference set failure');
ok("f" ~~ m/(<[a..z]-[aeiou]-[y]>)/, 'Multi-difference set match', :todo<feature>);
is($0, 'f', 'Multi-difference set capture', :todo<feature>);

ok(']' ~~ m/(<[]]>)/, 'LSB match', :todo<feature>);
is($0, ']', 'LSB capture', :todo<feature>);
ok(']' ~~ m/(<[\]]>)/, 'quoted close LSB match');
is($0, ']', 'quoted close LSB capture');
ok('[' ~~ m/(<[\[]>)/, 'quoted open LSB match');
is($0, '[', 'quoted open LSB capture');
# Hack needed to make this file at least compile and run the other tests.
{
  my ($dollar_null, $res);
  eval '$res = "{" ~~ m{(<[\{]>)}; $dollar_null = $0';
  ok($res, 'quoted open LCB match');
  is($dollar_null, '{', 'quoted open LCB capture');
}

}
