use v6-alpha;
use Test;

plan 2;
use lib ('../lib', 'lib/');

use File::Util; pass "(dummy instead of broken use_ok)";

my $f = File::Util.new;
my $d = 'ext/File-Util/t';

my @files = $f.list_dir($d);
ok( @files );

@files = $f.list_dir($d, '--no-fsdots');
ok( @files.grep:{/^\.\.$/} == 0 ); # only .svn

@files = $f.list_dir($d, '--dirs-only');
ok( @files.grep:{$_ ~~ :d} == @files && @files > 1 );

@files = $f.list_dir($d, '--dirs-only', '--no-fsdots');
ok( @files == 1 && @files.grep:{$_ ~~ :d} == 1 ); # only .svn

@files = $f.list_dir($d, '--files-only');
ok( @files.grep:{"$d/$_" ~~ :f} == @files );

@files = $f.list_dir($d, '--with-paths');
ok( @files.grep:{$_.substr(0, $d.chars) eqv $d} > 0 &&
    @files.grep:{$_.substr(0, $d.chars) !eqv $d} == 0 );
