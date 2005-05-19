#!/usr/bin/pugs

use Test;
plan 3;

sub sanity{
	my %sane = zip 'a'..'d',1..4;
	is(%sane.ref,'Hash','%sane is a Hash');
}

sub insanity (Hash %baloney) returns Void{
	is(%baloney.ref,'Hash','%baloney is a Hash');
}

# sanity 0
my %h = zip 'a'..'d',1..4;
is(%h.ref,'Hash','%h is a Hash');

#sanity 1;
sanity;

#XXX Hash passed to a sub becomes a List 
insanity %h;

