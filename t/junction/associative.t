#!/usr/bin/pugs

use v6;
require Test;

plan 14;

=pod

Checking Junctions' Associativeness

L<S09/"Junctions">

L<S03/"Junctive operators">

=cut

{ # L<S09/"Junctions">

    ok ('1 2 3' eq ~((1|2)|3).values, "Left-associative any, | operator");
    ok ('1 2 3' eq ~(1|(2|3)).values, "Right-associative any, | operator");

    ok ('1 2 3' eq ~any(any(1,2),3).values, "Left-associative any()");
    ok ('1 2 3' eq ~any(1,any(2,3)).values, "Right-associative any()");

    ok ('1 2 3' eq ~((1&2)&3).values, "Left-associative all, & operator");
    ok ('1 2 3' eq ~(1&(2&3)).values, "Right-associative all, & operator");

    ok ('1 2 3' eq ~all(all(1,2),3).values, "Left-associative all()");
    ok ('1 2 3' eq ~all(1,all(2,3)).values, "Right-associative all()");

    ok ('1 2 3' eq ~((1^2)^3).values, "Left-associative one, ^ operator");
    ok ('1 2 3' eq ~(1^(2^3)).values, "Right-associative one, ^ operator");

    ok ('1 2 3' eq ~one(one(1,2),3).values, "Left-associative one()");
    ok ('1 2 3' eq ~one(1,one(2,3)).values, "Right-associative one()");

    ok ('1 2 3' eq ~none(none(1,2),3).values, "Left-associative none()");
    ok ('1 2 3' eq ~none(1,none(2,3)).values, "Right-associative none()");

}