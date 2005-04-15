#!/usr/bin/pugs

use v6;
require Test;

plan 2;

ok( grep { $_ eq 'LICENSE' }, readdir $*CWD );
$*CWD ~= 't/unspecced';
ok( grep { $_ eq 'cwd.t' }, readdir $*CWD );


skip("error handling not implemented");
#is( ($*CWD = 'I/do/not/exist'), undef );
