#!/usr/bin/pugs

use Test;

plan 5;
use_ok ::Log::Selective;
lives_ok { Log::Selective::select(:foo) }, "Selected tag ':foo'";

skip_rest("looping tests"); exit;

is (note "This should be output", :foo), "This should be output at $?FILE line $?LINE.\n", "no-newline message okay";
is (note "This should be output\n", :foo), "This should be output\n", "newline message okay";
is (note :foo), "Note: currently at $?FILE line $?LINE.\n", "default message okay";
