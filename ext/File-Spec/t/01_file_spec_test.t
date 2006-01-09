#!/usr/bin/pugs

use v6;
use Test;

plan 2;

use_ok('File::Spec');

if ($?OS eq 'MSWin32') {
    is(devnull(), 'nul', '... you are on Win32');
}
else {
    is(devnull(), '/dev/null', '... you are on Unix');
}
