#!/usr/bin/pugs

use v6;

#XXX just so that File:;Spec can be used w/o being installed
unshift @*INC, 'ext/File-Spec/lib', '../ext/File-Spec/lib', '../../ext/File-Spec /lib';
require File::Spec;
my $progdir = splitpath($*PROGRAM_NAME)[1] || '.'; 
@*INC.push($progdir);
require Motd; 

my $limit     = @ARGS[0] // '2';
my $dict      = canonpath("$progdir/pugspraise");
my $fh        = open "< $dict" err die $!;
my @list      = map -> $a{my $b = $a; chomp $b; $b;} =$fh;

#XXX it sure would make things tidier if the topic could be chomped,
# or better, if the filehandle could be autochomped.

my &tell = -> $max {
		# the hash prevents the pick of identical phrases for a single
		# sentence
		say "Pugs is{   
			my %a; for 1..$max {%a{pick @list}=1;} report
			matchval 1,%a,$max;
		}."
};

tell $limit; 

