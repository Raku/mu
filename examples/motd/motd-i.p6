#!/usr/bin/pugs

use v6;
unshift @*INC, 'ext/File-Spec/lib', '../ext/File-Spec/lib', '../../ext/File-Spec /lib';
require File::Spec;
my ($progdir) = splitpath($*PROGRAM_NAME)[1];
unshift @*INC, $progdir;
require "MOTD"; 

my $subject   = shift @ARGS || 'Pugs';
my $surveyed  = shift @ARGS || 10;
my %tally     ;
my @list      = @ARGS;
my $most      = 0;
my @mostsaid := { matchval \$most,\%tally };

my &tell := sub {
	 say 
# "From a sample of $surveyed, the majority { 
#			+@mostsaid(1) > 1 ?? "($most of each opinion)" :: "($most)" 
#		} said that 
	  "$subject is{ 
			report @mostsaid(1) 
		}.{
			"\n" x 10
		}([Subj],[Samplesize#],[List,...] <Enter>)"
};

say "Press Enter to generate an opinion about $subject";
my $keyed;
while($keyed = =$*IN){
	clear;
	chomp $keyed;
	my @keyed = parse_args($keyed);
	#new surveyFor
	#r2018 couldn't say = shift @keyed || 
	$subject  = @keyed[0]??@keyed[0]::$subject;
	$surveyed = @keyed[1]??@keyed[1]::$surveyed;
	@list     = @keyed[2..@keyed-1]??@keyed[2..@keyed-1]::@list;
	%tally    = $surveyed .whisper_about( *@list );
	tell(@mostsaid( $most = %tally.values.max));
}

