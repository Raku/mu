#!/usr/bin/pugs

use v6;
require Test;

plan 5;

{ # binary infix
	my @r;
	eval '@r = (1, 2, 3) »+« (2, 4, 6)';
	my @e = (3, 6, 9);
	todo_is(~@r, ~@e, "hyper-sum two arrays");
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
	eval '@r = (1, 2, 3) »-« 1';
	my @e = (0, 1, 2);
	todo_is(~@r, ~@e, "auto dimention upgrade");
};

{ # unary postfix again, but with a twist
	my @r;
	eval '@r = ("f", "oo", "bar")».length';
	my @e = (1, 2, 3);
	todo_is(~@r, ~@e);
};

