
# I don't normally do this, but keep all the compile tests seperate..
# seem to be compile time vs. run time issues with not doing this

use v6;
use Test;

plan 6;

use_ok("Perldoc::DOM::Node");
use_ok("Perldoc::DOM::Element");
use_ok("Perldoc::DOM::Text");
use_ok("Perldoc::DOM::WS");
use_ok("Perldoc::DOM::PI");
use_ok("Perldoc::DOM");
