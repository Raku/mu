#!/usr/bin/pugs

use v6;
require Test;

plan 11;

{ # binary infix
	my @r;
	eval '@r = (1, 2, 3) »+« (2, 4, 6)';
	my @e = (3, 6, 9);
	is(~@r, ~@e, "hyper-sum two arrays");

	@r = (1, 2, 3) »*« (2, 4, 6);
	@e = (2, 8, 18);
	is(~@r, ~@e, "hyper-multiply two arrays");

	@r = (1, 2, 3) »x« (3, 2, 1);
	@e = ('111', '22', '3');
	is(~@r, ~@e, "hyper-x two arrays");

	@r = (1, 2, 3) »xx« (3, 2, 1);
	@e = ((1,1,1), (2,2), (3));
	is(~@r, ~@e, "hyper-xx two arrays");

	@r = (20, 40, 60) »/« (2, 5, 10);
	@e = (10, 8, 6);
	is(~@r, ~@e, "hyper-divide two arrays");

	@r = (1, 2, 3) »+« (10, 20, 30) »*« (2, 3, 4);
	@e = (21, 62, 123);
	is(~@r, ~@e, "precedence - »+« vs »*«");
};

{ # unary postfix
	my @r = (1, 2, 3);
	eval '@r »++';
	my @e = (2, 3, 4);
	todo_is(~@r, ~@e, "hyper auto increment an array");
};

{ # unary prefix
	my @r;
	eval '@r = -« (3, 2, 1)';
	my @e = (-3, -2, -1);
	todo_is(~@r, ~@e, "hyperr op on assignment/pipeline");
};

{ # dimention upgrade
	my @r;
	eval '@r = (1, 2, 3) »+« 1';
	my @e = (2, 3, 4);
	is(~@r, ~@e, "auto dimension upgrade on rhs");

	@r = 2 »*« (10, 20, 30);
	@e = (20, 40, 60);
	is(~@r, ~@e, "auto dimension upgrade on lhs");
};

{ # unary postfix again, but with a twist
	my @r;
	eval '@r = ("f", "oo", "bar")».length';
	my @e = (1, 2, 3);
	todo_is(~@r, ~@e);
};

