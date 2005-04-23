#!/usr/bin/pugs

use v6;
require Test;

plan 3;

my $pwd = $*CWD;
ok( grep { $_ eq 'LICENSE' }, readdir $*CWD );
$*CWD =  $*CWD ~ '/t/unspecced';
ok( grep { $_ eq 'cwd.t' }, readdir $*CWD );
is( ($*CWD = 'I/do/not/exist'), undef, "error handling" , :todo(1));
$*CWD = $pwd;
