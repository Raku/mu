#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/codevars.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 10;

if(eval('!("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

ok("abc" ~~ m/a(bc){$<caught> = $0}/, 'Inner match', :todo<feature>);
is($/<caught>, "bc", 'Inner caught', :todo<feature>);

my $caught = "oops!";
ok("abc" ~~ m/a(bc){$caught = $0}/, 'Outer match', :todo<feature>);
is($caught, "bc", 'Outer caught', :todo<feature>);

ok("abc" ~~ m/a(bc){$0 = uc $0}/, 'Numeric match', :todo<feature>);
is($/, "abc", 'Numeric matched', :todo<feature>);
is($0, "BC", 'Numeric caught', :todo<feature>);

eval_ok(' "abc" ~~ m/a(bc){$/ = Match.new(uc $0)}/ ', 'Zero match', :todo<feature>);
is($/, "BC", 'Zero matched', :todo<feature>);
is($0, "bc", 'One matched', :todo<feature>);

}

