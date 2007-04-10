use v6-alpha;
use Test;

plan 3;
use lib ('../lib', 'lib/');

use File::Util; pass "(dummy instead of broken use_ok)";

my $current_data_file = slurp $?FILE;
my $f = File::Util.new;

$f.load_file($?FILE);
is($f.content, $current_data_file, "Content is ok for load_file via filename");

my $fh = open $?FILE;
$f.load_file($fh);
is($f.content, $current_data_file, "Content is ok for load_file via file handle");
close $fh;