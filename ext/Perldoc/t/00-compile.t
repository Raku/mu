#!/usr/bin/pugs

# I don't normally do this, but keep all the compile tests seperate..
# seem to be compile time vs. run time issues with not doing this

use v6;
use Test;

plan 6;

use Perldoc::DOM::Node; pass "(dummy instead of broken use_ok)";
use Perldoc::DOM::Element; pass "(dummy instead of broken use_ok)";
use Perldoc::DOM::Text; pass "(dummy instead of broken use_ok)";
use Perldoc::DOM::WS; pass "(dummy instead of broken use_ok)";
use Perldoc::DOM::PI; pass "(dummy instead of broken use_ok)";
use Perldoc::DOM; pass "(dummy instead of broken use_ok)";
