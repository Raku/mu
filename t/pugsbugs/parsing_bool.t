#!/usr/bin/pugs

use v6;
use Test;

plan 4;

is try { 42 or bool::false }, 42, "bool::false as RHS";
is try { bool::false or 42 }, 42, "bool::false as LHS", :todo<bug>;

is try { 42 or false }, 42, "false as RHS";
is try { false or 42 }, 42, "false as LHS", :todo<bug>;
