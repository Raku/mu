use v6-alpha;
use Test;

plan 9;
use lib ('../lib', 'lib/');

use File::Util; pass "(dummy instead of broken use_ok)";

my $f = File::Util.new;

$f.existent('ext/File-Util/t/01.use.t');
is($f.exist, 1, "File exist");
$f.existent('ext/File-Util/t/01.t');
is($f.exist, undef, "File doesn't exist");

$f.size('ext/File-Util/t/01.use.t');
is(!$f.fsize, undef, "File size is not 0");

$f.size('ext/File-Util/t/01.t');
is($f.fsize, undef, "File size is 0");

$f.can_read('ext/File-Util/t/01.use.t');
is($f.canread, 1, "Can read file");

$f.can_write('ext/File-Util/t/01.use.t');
is($f.canwrite, 1, "Can write file");

$f.can_read('ext/File-Util/t/01.t');
is($f.canread, undef, "Can read file");

$f.can_write('ext/File-Util/t/01.t');
is($f.canwrite, undef, "Can write file");

$f.line_count('ext/File-Util/t/01.use.t');
is($f.lines, 14, "Count lines");
