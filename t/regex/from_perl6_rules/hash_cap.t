use v6-alpha;
use Test;

=pod

This file was originally derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/hash_cap.t.

=cut

plan 116;

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

force_todo(1..49,51..99,101..108,111..116);

ok("  a b\tc" ~~ m/%<chars>:=( \s+ \S+ )/, 'Named unrepeated hash capture');
ok(exists($/<chars>,<'  a'>), 'One key captured');
eval_ok(q{!defined($/<chars>{'  a'})}, 'One value undefined');
ok($/<chars>.keys == 1, 'No extra unrepeated captures');

ok("  a b\tc" ~~ m/%<chars>:=( \s+ \S+ )+/, 'Named simple hash capture');
ok(exists($/<chars>,<'  a'>), 'First simple key captured');
eval_ok(q{!defined($/<chars>{'  a'})}, 'First simple value undefined');
ok(exists($/<chars>,<' b'>), 'Second simple key captured');
eval_ok(q{!defined($/<chars>{' b'})}, 'Second simple value undefined');
ok(exists($/<chars>,<"\tc">), 'Third simple key captured');
eval_ok(q{!defined($/<chars>{"\tc"})}, 'Third simple value undefined');
ok($/<chars>.keys == 3, 'No extra simple captures');

ok("  a b\tc" ~~ m/%<first>:=( \s+ \S+ )+ %<last>:=( \s+ \S+)+/, 'Sequential simple hash capture');
ok(exists($/<first>,<'  a'>), 'First sequential key captured');
eval_ok(q{!defined($/<first>{'  a'})}, 'First sequential value undefined');
ok(exists($/<first>,<' b'>), 'Second sequential key captured');
eval_ok(q{!defined($/<first>{' b'})}, 'Second sequential value undefined');
ok(exists($/<last>,<"\tc">), 'Third sequential key captured');
eval_ok(q{!defined($/<last>{"\tc"})}, 'Third sequential value undefined');
ok($/<first>.keys == 2, 'No extra first sequential captures');
ok($/<last>.keys == 1, 'No extra last sequential captures');

ok("abcxyd" ~~ m/a  %<foo>:=(.(.))+ d/, 'Repeated nested hash capture');
ok(exists($/<foo>,<c>), 'Nested key 1 captured');
eval_ok(q{!defined($/<foo><c>)}, 'No nested value 1 captured');
ok(exists($/<foo>,<y>), 'Nested key 2 captured');
eval_ok(q{!defined($/<foo><y>)}, 'No nested value 2 captured');
ok($/<foo>.keys == 2, 'No extra nested captures');

ok("abcd" ~~ m/a  %<foo>:=(.(.))  d/, 'Unrepeated nested hash capture');
ok(exists($/<foo>,<c>), 'Unrepeated key captured');
eval_ok(q{!defined($/<foo><c>)}, 'Unrepeated value not captured');
ok($/<foo>.keys == 1, 'No extra unrepeated nested captures');

ok("abcd" ~~ m/a  %<foo>:=((.)(.))  d/, 'Unrepeated nested hash multicapture');
ok(exists($/<foo>,<b>), 'Unrepeated key multicaptured');
eval_is(q{$/<foo><b>}, 'c', 'Unrepeated value not multicaptured');
ok($/<foo>.keys == 1, 'No extra unrepeated nested multicaptures');

ok("abcxyd" ~~ m/a  %<foo>:=((.)(.))+ d/, 'Repeated nested hash multicapture');
ok(exists($/<foo>,<b>), 'Nested key 1 multicaptured');
eval_is(q{$/<foo><b>}, 'c', 'Nested value 1 multicaptured');
ok(exists($/<foo>,<x>), 'Nested key 2 multicaptured');
eval_is(q{$/<foo><x>}, 'y', 'Nested value 2 multicaptured');
ok($/<foo>.keys == 2, 'No extra nested multicaptures');

our %foo;
ok("abcxyd" ~~ m/a  %foo:=(.(.))+  d/, 'Package hash capture');
ok(exists(%foo,<c>), 'Package hash key 1 captured');
eval_ok(q{!defined(%foo{c})}, 'Package hash value 1 not captured');
ok(exists(%foo,<y>), 'Package hash key 2 captured');
eval_ok(q{!defined(%foo{y})}, 'Package hash value 2 not captured');
ok(%foo.keys == 2, 'No extra package hash captures');

rule two {..}

ok("abcd" ~~ m/a  %<foo>:=[<two>]  d/, 'Compound hash capture');
is($/<two>, "bc", 'Implicit subrule variable captured');
ok($/<foo>.keys == 0, 'Explicit hash variable not captured');

ok("  a b\tc" ~~ m/%<chars>:=( %<spaces>:=[\s+] (\S+))+/, 'Nested multihash capture');
ok(exists($/<chars>,<a>), 'Outer hash capture key 1');
eval_ok(q{!defined($/<chars><a>)}, 'Outer hash no capture value 1');
ok(exists($/<chars>,<b>), 'Outer hash capture key 2');
eval_ok(q{!defined($/<chars><b>)}, 'Outer hash no capture value 2');
ok(exists($/<chars>,<c>), 'Outer hash capture key 3');
eval_ok(q{!defined($/<chars><c>)}, 'Outer hash no capture value 3');
ok($/<chars>.keys == 3, 'Outer hash no extra captures');

ok(exists($/<spaces>,<'  '>), 'Inner hash capture key 1');
eval_ok(q{!defined($/<spaces>{'  '})}, 'Inner hash no capture value 1');
ok(exists($/<spaces>,<' '>), 'Inner hash capture key 2');
eval_ok(q{!defined($/<spaces>{' '})}, 'Inner hash no capture value 2');
ok(exists($/<spaces>,<"\t">), 'Inner hash capture key 3');
eval_ok(q{!defined($/<spaces>{"\t"})}, 'Inner hash no capture value 3');
ok($/<spaces>.keys == 3, 'Inner hash no extra captures');

rule spaces { @<spaces>:=[\s+] }

ok("  a b\tc" ~~ m/%<chars>:=( <spaces> (\S+))+/, 'Subrule hash capture');

ok(exists($/<chars>,<a>), 'Outer subrule hash capture key 1');
eval_ok(q{!defined($/<chars><a>)}, 'Outer subrule hash no capture value 1');
ok(exists($/<chars>,<b>), 'Outer subrule hash capture key 2');
eval_ok(q{!defined($/<chars><b>)}, 'Outer subrule hash no capture value 2');
ok(exists($/<chars>,<c>), 'Outer subrule hash capture key 3');
eval_ok(q{!defined($/<chars><c>)}, 'Outer subrule hash no capture value 3');
ok($/<chars>.keys == 3, 'Outer subrule hash no extra captures');
is($/<spaces>, "\t", 'Final subrule hash capture');


ok("  a b\tc" ~~ m/%<chars>:=( %<spaces>:=[<?spaces>] (\S+))+/, 'Nested subrule hash multicapture');
ok(exists($/<chars>,<a>), 'Outer rule nested hash key multicapture');
eval_ok(q{!defined($/<chars><a>)}, 'Outer rule nested hash value multicapture');
ok(exists($/<chars>,<b>), 'Outer rule nested hash key multicapture');
eval_ok(q{!defined($/<chars><b>)}, 'Outer rule nested hash value multicapture');
ok(exists($/<chars>,<c>), 'Outer rule nested hash key multicapture');
eval_ok(q{!defined($/<chars><c>)}, 'Outer rule nested hash value multicapture');
ok($/<chars>.keys == 3, 'Outer subrule hash no extra multicaptures');

ok(exists($/<spaces>,<'  '>), 'Inner rule nested hash key multicapture');
eval_ok(q{!defined($/<spaces>{'  '})}, 'Inner rule nested hash value multicapture');
ok(exists($/<spaces>,<' '>), 'Inner rule nested hash key multicapture');
eval_ok(q{!defined($/<spaces>{' '})}, 'Inner rule nested hash value multicapture');
ok(exists($/<spaces>,<"\t">), 'Inner rule nested hash key multicapture');
eval_ok(q{!defined($/<spaces>{"\t"})}, 'Inner rule nested hash value multicapture');
ok($/<spaces>.keys == 3, 'Inner subrule hash no extra multicaptures');

ok("  a b\tc" ~~ m/%<chars>:=( (<?spaces>) (\S+))+/, 'Nested multiple hash capture');
eval_ok(q{$/<chars>{'  '} eq 'a'}, 'Outer rule nested hash value multicapture');
eval_ok(q{$/<chars>{' '} eq 'b'}, 'Outer rule nested hash value multicapture');
eval_is(q{$/<chars>{"\t"}}, 'c', 'Outer rule nested hash value multicapture');
eval_ok(q{$/<chars>.keys == 3}, 'Outer subrule hash no extra multicaptures');

my %bases = ();
ok("Gattaca" ~~ m:i/ %bases:=(A|C|G|T)+ /, 'All your bases...');
ok(exists(%bases,<a>), 'a key');
eval_ok(q{!defined(%bases{a})}, 'No a value');
ok(exists(%bases,<c>), 'c key');
eval_ok(q{!defined(%bases{c})}, 'No c value');
ok(!exists(%bases,<g>), 'No g key');
ok(exists(%bases,<G>), 'G key');
eval_ok(q{!defined(%bases{G})}, 'No G value');
ok(exists(%bases,<t>), 't key');
eval_ok(q{!defined(%bases{t})}, 'No t value');
ok(%bases.keys == 4, 'No other bases');

%bases = ();
my %aca = ('aca' => 1);;
ok("Gattaca" ~~ m:i/ %bases:=(A|C|G|T)**{4} (%aca) /, 'Hash interpolation');
ok(exists(%bases,<a>), 'a key');
eval_ok(q{!defined(%bases{a})}, 'No a value');
ok(!exists(%bases,<c>), 'No c key');
ok(!exists(%bases,<g>), 'No g key');
ok(exists(%bases,<G>), 'G key');
eval_ok(q{!defined(%bases{G})}, 'No G value');
ok(exists(%bases,<t>), 't key');
eval_ok(q{!defined(%bases{t})}, 'No t value');
ok(%bases.keys == 3, 'No other bases');
is("$1", "aca", 'Trailing aca');

}

