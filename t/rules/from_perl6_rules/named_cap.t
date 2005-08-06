#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/named_cap.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 26;

if(eval('!("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

ok("abcd" ~~ m/a  $<foo>:=(..)  d/, 'Hypothetical variable capture');
is($/<foo>, "bc", 'Hypothetical variable captured');

my $foo;
ok("abcd" ~~ m/a  $foo:=(..)  d/, 'Package variable capture', :todo<feature> );
is($foo, "bc", 'Package variable captured', :todo<feature> );

ok("abcd" ~~ m/a  $1:=(.) $0:=(.) d/, 'Reverse capture');
is($0, "c", '$0 captured');
is($1, "b", '$1 captured');

rule two {..}

ok("abcd" ~~ m/a  $<foo>:=[<two>]  d/, 'Compound hypothetical capture');
is($/<two>, "bc", 'Implicit hypothetical variable captured');
is($/<foo>, "bc", 'Explicit hypothetical variable captured');

$foo = "";
ok("abcd" ~~ m/a  $foo:=[<two>]  d/, 'Mixed capture', :todo<feature> );
is($/<two>, "bc", 'Implicit hypothetical variable captured', :todo<feature> );
is($foo, "bc", 'Explicit package variable captured', :todo<feature> );

ok("a cat_O_9_tails" ~~ m:w/<alpha> <ident>/, 'Standard captures' );
is($/<alpha>, "a", 'Captured <?alpha>' );
is($/<ident>, "cat_O_9_tails", 'Captured <?ident>' );

ok("Jon Lee" ~~ m:w/$<first>:=(<ident>) $<family>:=[<ident>]/, 'Repeated standard captures' );
is($/<first>,  "Jon", 'Captured $first' );
is($/<family>, "Lee", 'Captured $family' );
is($/<ident>,  "Lee", 'Captured <?ident>' );

ok("foo => 22" ~~ m:w/$0:=(foo) =\> (\d+) | $1:=(\d+) \<= $0:=(foo) /, 'Pair match' );
is($0, 'foo', 'Key match' );
is($1, '22', 'Value match' );

ok("22 <= foo" ~~ m:w/$0:=(foo) =\> (\d+) | $1:=(\d+) \<= $0:=(foo) /, 'Pair match', :todo<feature> );
is($0, 'foo', 'Reverse key match', :todo<feature> );
is($1, '22', 'Reverse value match', :todo<feature> );

}
