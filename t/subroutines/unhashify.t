#!/usr/bin/pugs

use Test;
plan 3;

sub sanity{
	my %sane = zip 'a'..'d',1..4;
	isa_ok(%sane, Hash, '%sane is a Hash');
}

sub insanity (Hash %baloney) returns Void{
	isa_ok(%baloney, Hash, '%baloney is a Hash');
}

# sanity 0
my %h = zip 'a'..'d',1..4;
is(%h.ref,'Hash','%h is a Hash');

#sanity 1;
sanity;

#XXX Hash passed to a sub becomes a List 
insanity %h;

