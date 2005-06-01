#!/usr/bin/pugs

use v6;
use Test;

plan 14;

# test the Meta::Util M3

use Set;
use Meta::Util;

class SomeClass {
}

# this should add accessors to it
Has("SomeClass", "bob");

my $object = SomeClass.new();

my $rv;
eval { $rv = $object.set_bob("bar"); };
is($rv, "bar", "object got a mutator", :todo<feature>);

$rv = undef;
eval { $rv = $object.get_bob; };
is($rv, "bar", "object can use accessor", :todo<feature>);

$rv = undef;
eval { $rv = $object.bob; };
is($rv, "bar", "object can use switch", :todo<feature>);


# set accessors
Has("SomeClass", "bert", "Set");

$rv;
eval { $rv = $object.set_bert( set(1..10) ); };
is($rv, set(1..10), "object got a mutator", :todo<feature>);

$rv = undef;
eval { $rv = $object.get_bert; };
is($rv, set(1..10), "object can use accessor", :todo<feature>);

$rv = undef;
eval { $rv = $object.bert; };
is($rv, set(1..10), "object can use switch", :todo<feature>);

$rv = undef;
eval { $rv = $object.bert_remove( 1..5, 8..9 ) };
is($rv, 7, "got a _remove mutator", :todo<feature>);

$rv = undef;
eval { $rv = $object.bert_insert( 5..7 ) };
is($rv, 1, "got an _insert mutator", :todo<feature>);

$rv = undef;
eval { $rv = $object.bert; };
is($rv, set(5..7, 10), "they seemed to work", :todo<feature>);

$rv = undef;
eval { $rv = $object.bert_size };
is($rv, 4, "can get size OK", :todo<feature>);

# now, real magic ... mutual companion mutators
Has("SomeClass", "jack", "ref", { companion => "jill" });
Has("SomeClass", "jill", "ref", { companion => "jack" });

my $jack = SomeClass.new;
my $jill = SomeClass.new;

$rv = undef;
eval { $rv = $jack.set_jill($jill) };
is($rv, $jill, "ok so far...", :todo<feature>);

$rv = undef;
eval { $rv = $jill.get_jack };
is($rv, $jack, "wow, how did that work?  <g>", :todo<feature>);

$rv = "fail";
eval { $rv = $jack.set_jill(undef) };
is($rv, undef, "jack forgot jill", :todo<feature>);

$rv = "fail";
eval { $rv = $jill.get_jack };
is($rv, undef, "and so jill forgot jack", :todo<feature>);
