use v6-alpha;
use Test;

plan 41;
use lib ('../lib', 'lib/');

use File::Util; pass "(dummy instead of broken use_ok)";

my @filenames = ("?foo", ">foo", "<foo", ":foo", "*foo", "/foo", "\\foo", "\"foo", "\tfoo");

my $f = File::Util.new;

for @filenames -> $fn {
    ok(!$f.valid_filename($fn));
}

# strange but legal filename character tolerance
ok($f.valid_filename(q['foo]));
ok($f.valid_filename(';foo'));
ok($f.valid_filename('$foo'));
ok($f.valid_filename('%foo'));
ok($f.valid_filename('`foo'));
ok($f.valid_filename('!foo'));
ok($f.valid_filename('@foo'));
ok($f.valid_filename('#foo'));
ok($f.valid_filename('^foo'));
ok($f.valid_filename('&foo'));
ok($f.valid_filename('-foo'));
ok($f.valid_filename('_foo'));
ok($f.valid_filename('+foo'));
ok($f.valid_filename('=foo'));
ok($f.valid_filename('(foo'));
ok($f.valid_filename(')foo'));
ok($f.valid_filename('{foo'));
ok($f.valid_filename('}foo'));
ok($f.valid_filename('[foo'));
ok($f.valid_filename(']foo'));
ok($f.valid_filename('~foo'));
ok($f.valid_filename('.foo'));
ok($f.valid_filename(q/;$%`!@#^&-_+=(){}[]~baz.foo'/));

my $filename;

for @filenames -> $fn {
    $filename = $f.escape_filename($fn);
    is($filename, '_foo');
}