#!/usr/bin/pugs

use v6;

unshift @*INC, 'ext/File-Spec/lib', '../ext/File-Spec/lib', '../../ext/File-Spec /lib';
require File::Spec;
my ($progdir) = splitpath($*PROGRAM_NAME)[1];
unshift @*INC, $progdir;
require "MOTD"; 

my $subject  = shift @ARGS || "Pugs";
my $sample  = shift @ARGS ||  10;

# unimplemented: should be able to say 
# my @list = =$fh is chomped;
my @list ;
my $dict = canonpath("$progdir/pugspraise");

my $fh = open("<$dict");

# should be able to chomp $_, but can't yet
# check back
for =$fh->$line is rw{
  # $line should be declarable as 'is rw'
	# not yet implemented
	my $a = $line; #so, we need to make a rw copy 
	chomp $a; 
	# next isn't working correctly, yet
	if( $a ){
		push @list,$a;
	}
};

my %tally    = $sample  # people
					     .whisper_about( @list) ;
my $most     = %tally.values.max;
my @mostsaid = matchval $most,%tally;

my &tell := sub {
		say "$subject is{ 
			report @mostsaid 
		}."
};

tell;
