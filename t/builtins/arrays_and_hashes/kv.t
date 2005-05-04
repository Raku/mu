#!/usr/bin/pugs

use v6;
use Test;

plan 29;

=pod

Basic C<kv> tests, see S29.

=cut

# L<S29/"Perl6::Arrays" /"kv"/>
{ # check the invocant form
    my @array = <a b c d>;
    my @kv = @array.kv;
    is(+@kv, 8, '@array.kv returns the correct number of elems');
    is(~@kv, "0 a 1 b 2 c 3 d",  '@array.kv has no inner list');
}

{ # check the non-invocant form
    my @array = <a b c d>;
    my @kv = kv(@array);
    is(+@kv, 8, 'kv(@array) returns the correct number of elems');
    is(~@kv, "0 a 1 b 2 c 3 d", 'kv(@array) has no inner list');
}

# L<S29/"Perl6::Hashes" /"kv"/>
{ # check the invocant form
    my %hash = (a => 1, b => 2, c => 3, d => 4);
    my @kv = %hash.kv;
    is(+@kv, 8, '%hash.kv returns the correct number of elems');
    is(~@kv.sort, "1 2 3 4 a b c d",  '%hash.kv has no inner list');
}

{ # check the non-invocant form
    my %hash = (a => 1, b => 2, c => 3, d => 4);
    my @kv = kv(%hash);
    is(+@kv, 8, 'kv(%hash) returns the correct number of elems');
    is(~@kv.sort, "1 2 3 4 a b c d",  'kv(%hash) has no inner list');
}

# See "Questions about $pair.kv" thread on perl-6 lang
{
    my $pair  = (a => 1);
    my @kv = $pair.kv;
    is(+@kv, 2, '$pair.kv returned one elem');
    is(+@kv, 2, '$pair.kv inner list has two elems');
    is(~@kv, "a 1", '$pair.kv inner list matched expectation');
}


# test3 and test4 illustrate a bug 

sub test1{
	my $pair = boo=>'baz'; 
	my $type = $pair.ref;
	my $elems= $pair.elems;
	for $pair.kv->$key,$value{
		is($key, 'boo', "test1: $elems-elem $type \$pair got the right \$key");
		is($value, 'baz', "test1: $elems-elem $type \$pair got the right \$value");
	}
}
test1;

sub test2{
	my %pair = boo=>'baz'; 
	my $type = %pair.ref;
	my $elems= %pair.elems;
	for %pair.kv->$key,$value{
		is($key, 'boo', "test2: $elems-elem $type \%pair got the right \$key");
		is($value, 'baz', "test2: $elems-elem $type \%pair got the right \$value");
	}
}
test2;

my %hash  = ('foo' => 'baz');
sub test3 (Hash %h){
  for %h.kv -> $key,$value {
		is($key, 'foo', "test3:  from {%h.elems}-elem {%h.ref} \%h got the right \$key",:todo<bug>);
		is($value, 'baz', "test3: from {%h.elems}-elem {%h.ref} \%h got the right \$value",:todo<bug>);
  }
}
test3 %hash;

sub test4 (Hash %h){
	for 0..%h.kv.end -> $idx {
		is(%h.kv[$idx], %hash.kv[$idx], "test4: elem $idx of {%h.kv.elems}-elem {%h.kv.ref} \%hash.kv correctly accessed",:todo<bug>);
	}
}
test4 %hash;

# sanity
for %hash.kv -> $key,$value {
	is($key, 'foo', "for(): from {%hash.elems}-elem {%hash.ref} \%hash got the right \$key");
	is($value, 'baz', "for(): from {%hash.elems}-elem {%hash.ref} \%hash got the right \$value");
}

