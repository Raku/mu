#!/usr/bin/pugs

use v6;
require Test;

plan 1;

require File::Spec;

if ($?OS eq 'MSWin32') {
    is(devnull(), 'nul', '... you are on Win32');
}
else {
    is(devnull(), '/dev/null', '... you are on Unix');
}