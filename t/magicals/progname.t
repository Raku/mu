#!/usr/bin/pugs

use v6;
use Test;

plan 1;

if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

ok($*PROGRAM_NAME eq ('t/magicals/progname.t' | 't\\magicals\\progname.t'), "progname var matches test file path");

# NOTE:
# above is a junction hack for Unix and Win32 file 
# paths until the FileSpec hack is working - Stevan
