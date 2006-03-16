#!/usr/bin/pugs

use v6;
use Test;

plan 18;

if ($*OS eq any<MSWin32 mingw msys cygwin browser>) {
    skip_rest "not supported on this platform";
    exit;
}

=pod

opendir/readdir support

=cut

my $dir = opendir('.');
isa_ok($dir, 'IO::Dir', "opendir worked");

my @files = readdir($dir);
ok(@files, "seems readdir worked too");

my @more_files = readdir($dir);
is(+@more_files, 0, "No more things to read");

my $row = readdir($dir);
ok(!defined($row), "in scalar context it returns undef");

my $rew_1 = rewinddir($dir);
is($rew_1, 1, "success of rewinddir 1 returns 1");

my @files_again = readdir($dir);

is_deeply(\@files_again, @files, "same list of files retrieved after rewind");

my $rew_2 = rewinddir($dir);
is($rew_2, 1, "success of rewinddir 2 returns 1");

my @files_scalar;
for readdir($dir) -> $f {
    @files_scalar.push($f);
}
is_deeply(\@files_scalar, @files, "same list of files retrieved after rewind, using scalar context");

my $rew_3 = $dir.rewinddir;
is($rew_3, 1, 'success of rewinddir 3 using $dir.rewinddir returns 1');
my @files_dot = $dir.readdir;
is_deeply(\@files_dot, @files, 'same list of files retrieved using $dir.readdir');

my $rew_4 = $dir.rewinddir;
is($rew_4, 1, 'success of rewinddir 4 using $dir.rewinddir returns 1');

my @files_scalar_dot;
for $dir.readdir -> $f {
    @files_scalar_dot.push($f);
}
is_deeply(\@files_scalar_dot, @files, 'same list of files, using $dir.readdir in scalar context');

my @more_files_2 = $dir.readdir;
is(+@more_files_2, 0, "No more things to read");

my $row_2 = $dir.readdir;
ok(!defined($row_2), "in scalar context it returns undef");


ok(closedir($dir), "as does closedir");

# on closed directory handler these calls should throw an exception
#my $undef = readdir($dir);
#my @empty = readdir($dir);
# rewinddir($dir);
# closedir


my $dh = opendir('.');
isa_ok($dh, 'IO::Dir', "opendir worked");
my @files_once_more = $dh.readdir;
is_deeply(\@files_once_more, @files, 'same list of files,after reopen');
ok($dir.closedir, 'closedir usinf $dir.closedir format');


