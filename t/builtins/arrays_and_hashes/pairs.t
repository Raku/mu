#!/usr/bin/pugs

use v6;
use Test;

plan 30;

=head1 DESCRIPTION

Basic C<pairs> tests, see S29.

=cut

# L<S29/"Perl6::Arrays" /"pairs"/>
{
  my @array = <a b c>;
  my @pairs;
  ok @pairs = @array.pairs, "basic pairs on arrays";
  is +@pairs, 3,            "pairs on arrays returned the correct number of elems";
  if(+@pairs != 3) {
    skip 6, "skipped tests which depend on a test which failed";
  } else {
    is @pairs[0].key,     0,  "key of pair returned by array.pairs was correct (1)";
    is @pairs[1].key,     1,  "key of pair returned by array.pairs was correct (2)";
    is @pairs[2].key,     2,  "key of pair returned by array.pairs was correct (3)";
    is @pairs[0].value, "a",  "value of pair returned by array.pairs was correct (1)";
    is @pairs[1].value, "b",  "value of pair returned by array.pairs was correct (2)";
    is @pairs[2].value, "c",  "value of pair returned by array.pairs was correct (3)";
  }
}


# L<S29/"Perl6::Hashes" /"pairs"/>
{
  my %hash = (a => 1, b => 2, c => 3);
  my @pairs;
  ok @pairs = %hash.pairs.sort, "sorted pairs on hashes";
  is +@pairs, 3,                "pairs on hashes returned the correct number of elems";
  if(+@pairs != 3) {
    skip 6, "skipped tests which depend on a test which failed";
  } else {
    is @pairs[0].key,   "a",      "value of pair returned by hash.pairs was correct (1)";
    is @pairs[1].key,   "b",      "value of pair returned by hash.pairs was correct (2)";
    is @pairs[2].key,   "c",      "value of pair returned by hash.pairs was correct (3)";
    is @pairs[0].value,   1,      "key of pair returned by hash.pairs was correct (1)";
    is @pairs[1].value,   2,      "key of pair returned by hash.pairs was correct (2)";
    is @pairs[2].value,   3,      "key of pair returned by hash.pairs was correct (3)";
  }
}

# Following stated by Larry on p6l
{
  my $pair  = (a => 1);
  my @pairs;
  ok @pairs = $pair.pairs, "pairs on a pair";
  is +@pairs, 1,           "pairs on a pair returned one elem";
  if(+@pairs != 1) {
    skip 2, "skipped tests which depend on a test which failed";
  } else {
    is @pairs[0].key,   "a", "key of pair returned by pair.pairs";
    is @pairs[0].value,   1, "value of pair returned by pair.pairs";
  }
}

# This next group added by Darren Duncan following discovery while debugging ext/Locale-KeyedText:
{
  my $hash_of_2_pairs = {'a'=>'b','c'=>'d'};
  my $hash_of_1_pair = {'a'=>'b'};
  is( $hash_of_2_pairs.pairs.sort.join( ',' ), "a\tb,c\td", "pairs() on 2-elem hash, 1-depth joined", :todo<feature> );
  is( $hash_of_1_pair.pairs.sort.join( ',' ), "a\tb", "pairs() on 1-elem hash, 1-depth joined", :todo<feature> );
  is( $hash_of_2_pairs.pairs.sort.map:{ .key~'='~.value }.join( ',' ), 'a=b,c=d', 
    "pairs() on 2-elem hash, 2-depth joined" );
  is( try { $hash_of_1_pair.pairs.sort.map:{ .key~'='~.value }.join( ',' ) }, 'a=b', 
    "pairs() on 1-elem hash, 2-depth joined", :todo<bug> );
}

{
    my %hash = (:a(1), :b(2), :c(3));

    lives_ok { for %hash.pairs -> $pair {
        $pair.value += 100;
    } }, 'aliases returned by %hash.pairs should be rw (1)', :todo<bug>;

    is %hash<b>, 102, 'aliases returned by %hash.pairs should be rw (2)', :todo<bug>;
}

{
    my @array = (17, 23, 42);

    lives_ok { for @array.pairs -> $pair {
        $pair.value += 100;
    } }, 'aliases returned by @array.pairs should be rw (1)', :todo<bug>;

    is @array[1], 123, 'aliases returned by @array.pairs should be rw (2)', :todo<bug>;
}

{
    my $pair = (a => 42);

    lives_ok { for $pair.pairs -> $p {
        $p.value += 100;
    } }, 'aliases returned by $pair.value should be rw (1)', :todo<bug>;

    is $pair.value, 142, 'aliases returned by $pair.kv should be rw (2)', :todo<bug>;
}
