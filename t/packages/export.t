#!/usr/bin/pugs

use v6;
use Test;

plan 2;

# (Automatic s:g/::/$PATH_SEPARATOR_OF_CUR_OS/)++
use t::packages::Export_PackB;

ok t::packages::Export_PackB::does_export_work(),
  "'is export' works correctly even when not exporting to Main (1)";

# t::packages::Export_PackA::exported_foo should not have been exported into
# our namespace.
dies_ok { exported_foo() },
  "'is export' works correctly even when not exporting to Main (2)";
