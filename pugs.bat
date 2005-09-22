@rem = '--*-Perl-*--
@echo off
if "%OS%" == "Windows_NT" goto WinNT
perl -x -S "%0" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofperl
:WinNT
perl -x -S "%0" %*
if NOT "%COMSPEC%" == "%SystemRoot%\system32\cmd.exe" goto endofperl
if %errorlevel% == 9009 echo You do not have Perl in your PATH.
if errorlevel 1 goto script_failed_so_exit_with_non_zero_val 2>nul
goto endofperl
@rem ';
#!/bin/sh
eval 'exec perl -x -S $0 ${1+"$@"}'
  if $this_is_not_perl;
#!/usr/bin/perl -w
#line 18

use strict;
use FindBin;
use File::Spec;
use Config;

my $bin = helper_exe('pugs_bin');

my $v  = "\014\014\014";   # a string unlikely to be part of a filename
my $vf = qr/(?=$v)/;

local $_ = join($v, "", @ARGV, "");

if (s/${v}-B(${v})?JS${vf}//) {
    # "pugs -BJS" should go to the interactive shell, jspugs.pl.
    if (/^(\s|${v})*$/) {
        $bin = helper_exe('perl5', 'PIL2JS', 'jspugs.pl');
    }
    # "pugs -BJS ..." should go to the evaluator, runjs.pl.
    else {
        $bin = helper_exe('perl5', 'PIL2JS', 'runjs.pl');
    }
}
elsif (s/${v}-B(${v})?(?i:Perl5)${vf}//) {
    $bin = helper_exe('perl5', 'PIL-Run', 'crude_repl.pl');
}

s/^($v)+//;
s/($v)+$//;

if($^O eq 'MSWin32') {
    if( system($bin, split($v, $_)) == 0 ) {
	exit(0);
    } elsif( $? == -1 ) {
	die "system( $bin , ...) failed to execute!\n";
    } else {
	exit($? >> 8);
    }
} else {
    exec($bin, split($v, $_));
    die "$0: exec of $bin failed. $!";
}

sub helper_exe {
    my @tries;

    # Hack? Needed to find runjs.pl and jspugs.pl if this script is executed
    # from blib/script/ (copying runjs.pl and jspugs.pl to blib/script/ won't
    # work, as these scripts in turn expect the dir of $FindBin::Bin to contain
    # libjs/, lib/, lib6/, etc).
    for my $prefix ([], ["..", ".."]) {
        my $fn = File::Spec->catfile($FindBin::RealBin, @$prefix, @_);
        push @tries, $fn;
        return $fn if -e $fn;

        $fn .= $Config{_exe};
        push @tries, $fn;
        return $fn if -e $fn;
    }

    die "Couldn't find helper program \"$_[-1]\" (searched in @tries)!\n";
}

__END__
:endofperl
