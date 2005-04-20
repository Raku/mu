#!/usr/bin/pugs

use v6;
#XXX just so that File:;Spec can be used w/o being installed
unshift @*INC, 'ext/File-Spec/lib', '../ext/File-Spec/lib', '../../ext/File-Spec /lib';
#XXX should be able to 'use'
require File::Spec;
my $progdir   = splitpath($*PROGRAM_NAME)[1];
unshift @*INC, $progdir;
require MOTD; 

my $subject   = shift @ARGS || 'Pugs is';
my $surveyed  = shift @ARGS || 20;
my %tally     ;

# XXX unimplemented: should be able to say 
# my @list = =$fh is chomped;
my @list      ;
my $dict = canonpath("$progdir/pugspraise");

my $fh = open("<$dict") || die $!;

# XXX should be able to chomp $_, but can't yet
# check back
for =$fh->$line is rw{
  # XXX $line should be declarable as 'is rw'
	# not yet implemented
	my $a = $line; #so, we need to make a rw copy 
	chomp $a; 
	push @list,$a || next()
};

$fh.close;
my $orig 	  = ~@list;
my $most      = 0;
my @mostsaid := { matchval \$most,\%tally,3};
my &tell := sub {
	 say "{ 
			$subject~report @mostsaid(1) 
		}.{
			"\n" x 10
		}([NewListItem,...] <Enter>)"
};

say "Press Enter to generate quotes about \"$subject...\""~
	  "\nPress Ctrl-D to end";

my $keyed;
while $keyed = =$*IN {
	clear;
	chomp $keyed;
 	my @keyed ;
	if $keyed {
		@keyed = parse_args($keyed);
	}
	@list      = (*@keyed , *@list);
	%tally     = whisper_about $surveyed,*@list ;
	tell @mostsaid( $most = %tally.values.max ); 
	@keyed.perl.say;
}

unless $orig eq ~@list {
	say "Do you want to save your changes?";
	print "y/N ..."; 
	my $ans = =$*IN; # XXX is chomped;
	chomp $ans; 
	# User wants to save changes
	# Save the original $dict to a backup
	if $ans eq any ('y','Y'){
		my $backup = $dict;
		my $incr    = 1;
		while -f "$backup-$incr" {
			$incr++;
		}
		$backup ~= "-$incr";
		rename $dict,"$backup";
		# Write the changes to a new $dict
		my $newfh = open(">$dict");
		for @list->$line{say $newfh,$line}
		$newfh.close;
		say "diff -u $backup $dict";
	}
}
