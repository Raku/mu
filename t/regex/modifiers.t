use v6-alpha;

use Test;

plan 20;

=pod

Testing regex modifiers.  There are a bunch of these, and I have only
just started the list.  Please fill in more, preferably in the order
they are listed in the synopsis, and with accurate links.

These tests are being migrated away to t/spec/S05-modifier/*.t

=cut

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
  exit;
}

#L<S05/Modifiers/"The :c">

{
  regex simple { . a };
  my $string = "1a2a3a";
  $string ~~ m:c/<simple>/;
  is(~$/, '1a', "match first 'a'");
  $string ~~ m:c/<simple>/;
  is(~$/, '2a', "match second 'a'");
  $string ~~ m:c/<simple>/;
  is(~$/, '3a', "match third 'a'");
  $string ~~ m:c/<simple>/;
  is(~$/, '', "no more 'a's to match");
};

#L<S05/Modifiers/"The :b">

{
	ok('ä' ~~ m:b/a/, 'Basechar: a matches ä', :todo<pugs>);
	ok('a' ~~ m:b/ä/, 'Basechar: ä matches a', :todo<pugs>);
	ok('à' ~~ m:b/a/, 'Basechar: a matches à', :todo<pugs>);
	ok('á' ~~ m:b/a/, 'Basechar: a matches á', :todo<pugs>);
	ok('â' ~~ m:b/a/, 'Basechar: a matches â', :todo<pugs>);
	ok('å' ~~ m:b/a/, 'Basechar: a matches å', :todo<pugs>);
	ok('æ' ~~ m:b/^ae$/, 'Basechar: ae matches æ', :todo<pugs>);
	ok('ƌ' ~~ m:b/d/, 'Basechar: d matches ƌ', :todo<pugs>);
	ok('ａ' ~~ m:b/a/, 'Basechar: a matches fullwidth a', :todo<pugs>);
}

#L<S05/Modifiers/"If followed by an x, it means repetition.">
{
	# TODO: more tests
	ok('abab' ~~ m:2x/ab/,  ':2x (repetition) modifier (1)', :todo<pugs>);
	ok(!('ab' ~~ m:2x/ab/), ':2x (repetition) modifier (1)', :todo<pugs>);

}

