#!/usr/bin/pugs

use v6;

#XXX just so that File:;Spec can be used w/o being installed
unshift @*INC, 'ext/File-Spec/lib', '../ext/File-Spec/lib', '../../ext/File-Spec /lib';
require File::Spec;
my $progdir = splitpath($*PROGRAM_NAME)[1] || '.'; 
@*INC.push($progdir);
require Motd; 

my $subject   = @ARGS[0] // 'Pugs';
my $surveyed  = @ARGS[1] // 20;
my $dict      = @ARGS[2] // canonpath("$progdir/pugspraise");
my $fh        = open "< $dict" err die $!;
my @list;

for =$fh ->$line{
	my $a = $line; 
	chomp $a; 
	push @list, $a || next()
};
$fh.close;

my %opinions = $surveyed  # number 
               .whisper_about( @list) ;

my $most     = %opinions.values.max;
my &tell = -> $limit {
		say "$subject is{ 
			report matchval $most,%opinions,$limit;
		}."
};
tell 2;
