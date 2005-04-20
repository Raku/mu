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

    is ('1 2 3', ~((1|2)|3).values, "Left-associative any, | operator");
    is ('1 2 3', ~(1|(2|3)).values, "Right-associative any, | operator");

    is ('1 2 3', ~any(any(1,2),3).values, "Left-associative any()");
    is ('1 2 3', ~any(1,any(2,3)).values, "Right-associative any()");

    is ('1 2 3', ~((1&2)&3).values, "Left-associative all, & operator");
    is ('1 2 3', ~(1&(2&3)).values, "Right-associative all, & operator");

    is ('1 2 3', ~all(all(1,2),3).values, "Left-associative all()");
    is ('1 2 3', ~all(1,all(2,3)).values, "Right-associative all()");

    todo_isnt ('1 2 3', ~((1^2)^3).values, "Left-associative one, ^ operator");
    isnt ('1 2 3', ~(1^(2^3)).values, "Right-associative one, ^ operator");

    isnt ('1 2 3', ~one(one(1,2),3).values, "Left-associative one()");
    isnt ('1 2 3', ~one(1,one(2,3)).values, "Right-associative one()");

    is ('1 2 3', ~none(none(1,2),3).values, "Left-associative none()");
    is ('1 2 3', ~none(1,none(2,3)).values, "Right-associative none()");

}
