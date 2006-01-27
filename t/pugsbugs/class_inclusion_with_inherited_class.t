#!/usr/bin/pugs

use v6;
use Test;

=pod

role A { method t ( *@a ) { @a }}; class B does A; class C does A { has B $.b; method BUILD { $.b = B.new; $.b.t( 'haha','hehe','xixi');}} C.new
*** No such method: "&t"
    at <interactive> line 1, column 108-136
    <interactive> line 1, column 140-145
    <interactive> line 1, column 140-145

=cut

plan 1;

role A
{
	method t ( *@a ) {
		@a.sum;
	}
}

class B does A
{}

class C does A
{
	has $.s;
	has B $.b;
	submethod BUILD {
		$.b = B.new;
		$.s = $.b.t(1, 2, 3);
	}
}

is try{ C.new.s }, 6, "Test class include another class which inherited from same role", :todo<bug>;

