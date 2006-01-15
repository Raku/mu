#!/usr/bin/pugs

use v6;
use Test;

plan 4;

# (Automatic s:g/::/$PATH_SEPARATOR_OF_CUR_OS/)++
use t::packages::Export_PackB;

ok t::packages::Export_PackB::does_export_work(),
  "'is export' works correctly even when not exporting to Main (1)";

# t::packages::Export_PackA::exported_foo should not have been exported into
# our namespace.
dies_ok { exported_foo() },
  "'is export' works correctly even when not exporting to Main (2)";

{
    use t::packages::Export_PackC;
    lives_ok { foo_packc() } "lexical export works";
}
dies_ok { foo_packc() } "lexical export is indeed lexical";
