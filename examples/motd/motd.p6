#!/usr/bin/pugs

use v6;

unshift @*INC, 'ext/File-Spec/lib', '../ext/File-Spec/lib', '../../ext/File-Spec /lib';
require File::Spec;
my ($progdir) = splitpath($*PROGRAM_NAME)[1];
unshift @*INC, $progdir;
require "MOTD"; 

my $subject  = shift @ARGS || "Pugs";
my $sample  = shift @ARGS ||  10;
my @list     = @ARGS  ;
my %tally    = $sample  # people
					     .whisper_about( *@list) ;
my $most     = %tally.values.max;
my @mostsaid = matchval $most,%tally;

my &tell := sub {
		say "$subject is{ 
			report @mostsaid 
		}."
};

tell;
