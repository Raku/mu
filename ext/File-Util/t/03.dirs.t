use v6-alpha;
use Test;

plan 2;
use lib ('../lib', 'lib/');

use File::Util; pass "(dummy instead of broken use_ok)";

my $f = File::Util.new;

my @files = $f.list_dir('t');

