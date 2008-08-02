use v6;

use Test;

=begin pod

_ should be allowed in numbers

But according to L<S02/Literals/"A single underscore is allowed only">, only between two digits.

=end pod

plan 19;

is 1_0, 10, "Single embedded underscore works";

isnt eval('1__0'),  10, "Multiple embedded underscores fail";

isnt eval('_10'),   10, "Leading underscore fails";

isnt eval('10_'),   10, "Trailing underscore fails";

isnt eval('10_.0'), 10, "Underscore before . fails";

isnt eval('10._0'), 10, "Underscore after . fails";

isnt eval('10_e1'), 10, "Underscore before e fails";

isnt eval('10e_1'), 10, "Underscore after e fails";

isnt eval('10_E1'), 10, "Underscore before E fails";

isnt eval('10E_1'), 10, "Underscore after E fails";

is 3.1_41, 3.141, "Underscores work with floating point after decimal";

is 10_0.8, 100.8, "Underscores work with floating point before decimal";

is 0xdead_beef, 0xdeadbeef, "Underscores work with hex";

is 0b1101_1110_1010_1101_1011_1110_1110_1111, 0xdeadbeef, "Underscores work with binary";

is 2e0_1, 20, "Underscores work in the argument for e";

is 2.1_23, 2.123, "2.1_23 parses as number";

dies_ok { 2._foo },    "2._foo parses as method call";
dies_ok { 2._123 },    "2._123 parses as method call";
dies_ok { 2._e23 },    "2._23  parses as method call";
