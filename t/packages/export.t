#!/usr/bin/pugs

use v6;
use Test;

plan 1;

# (Automatic s:g/::/$PATH_SEPARATOR_OF_CUR_OS/)++
use t::pugsbugs::Export_PackB;

ok t::pugsbugs::Export_PackB::does_export_work(),
  "'is export' works correctly even when not exporting to Main";
