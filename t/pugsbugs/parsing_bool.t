#!/usr/bin/pugs

use v6;
use Test;

plan 4;

is try { 42 err bool::false }, 42, "bool::false as RHS";
is try { bool::false err 42 }, 42, "bool::false as LHS", :todo<bug>;

is try { 42 err false }, 42, "false as RHS";
is try { false err 42 }, 42, "false as LHS", :todo<bug>;
