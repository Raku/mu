#!/usr/bin/pugs

use v6;
require Test;

plan 1;

todo_is(eval '$?PROGNAME', 't/magicals/progname.t', "progname var matches test file path");

