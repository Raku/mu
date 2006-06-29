#!/usr/bin/perl

use strict;
use warnings;
use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';
use Cwd qw< abs_path cwd >;

my $cwd = cwd();
my $pm = 'lib/Pugs/Grammar/MiniPerl6.pm';
my $grammar = abs_path('lib/Pugs/Grammar/MiniPerl6.grammar');
my $pm_is_uptodate = 0;

# Win32 abs_path - No such file or directory fix
if ( -e $pm ) {
    $pm = abs_path($pm);
    $pm_is_uptodate = (-s $pm and (-M $pm <= -M $grammar));
}

if (!$pm_is_uptodate) {
    print "Compiling grammar for miniperl6, this can take a while...\n";
    system("$^X -I../Pugs-Compiler-Rule/lib ../Pugs-Compiler-Rule/compile_p6grammar.pl $grammar > $pm");
}

my $mtime = time;
utime $mtime, $mtime, $pm;

name('Pugs-Grammar-MiniPerl6');
all_from('lib/Pugs/Grammar/MiniPerl6.pm');
clean_files('lib/Pugs/Grammar/MiniPerl6.pm ');

&WriteAll;
