use v6-alpha;
use Test;

plan 3;

use lib ('../lib', 'lib/');

use File::Util; pass "(dummy instead of broken use_ok)";

my $f = File::Util.new;

is($f.readlimit, 52428800, "readlimit OK");
$f.readlimit = 1000;
is($f.readlimit, 1000, "readlimit set OK");