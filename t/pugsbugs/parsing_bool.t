#!/usr/bin/pugs

use v6;
use Test;

plan 4;

is eval("42 err bool::false"), 42, "bool::false as RHS";
is eval("bool::false err 42"), 42, "bool::false as LHS", :todo<bug>;

is eval("42 err false"), 42, "false as RHS";
is eval("false err 42"), 42, "false as LHS", :todo<bug>;
