use v6-alpha;

use Test;

plan 3;

if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

my $pwd = $*CWD;
ok( grep { $_ eq 'LICENSE' }, readdir $*CWD );
$*CWD =  $*CWD ~ '/t/unspecced';
ok( grep { $_ eq 'cwd.t' }, readdir $*CWD );
is( ($*CWD = 'I/do/not/exist'), undef, "error handling" , :todo);
$*CWD = $pwd;
