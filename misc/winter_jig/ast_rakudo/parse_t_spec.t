#!/usr/bin/perl -w
use strict;
use warnings;

$ENV{PUGS_ROOT} || die "Need environment variable PUGS_ROOT.\n";
$ENV{PARROT_ROOT} || die "Need environment variable PARROT_ROOT.\n";
my $dir_t = $ENV{PUGS_ROOT}."/t";
my $dir_spec = $ENV{PUGS_ROOT}."/t/spec";
my $dir = $dir_spec;
my @files = map{chomp;$_} `find $dir -type f|egrep '\\.t\$'|sort`;
print "1..",0+@files,"\n";

for my $file (@files) {
    my $cmd = "$ENV{PUGS_ROOT}/misc/winter_jig/sixen/sixcmd rakudo.pbc --target=parse ";
    $cmd .= $file;
    print "\n# $file\n";
    my $no_errs = '2>&1';
    my $ret = system("$cmd > /dev/null $no_errs");
    if($ret == 0) { print "ok\n"; }
    elsif(($? >> 8) == 1) { print "not ok\n"; }
    elsif("$?" eq "2") {
	print "\nINTERRUPTED\n";
	exit(1);
    }
    else {
	print "# ERROR - Unexpected system() failure.\n";
	print "not ok\n";
	#exit(1); # Allow killing of non-terminating parses.
    }
}
