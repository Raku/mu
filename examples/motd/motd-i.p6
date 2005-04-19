#!/usr/bin/pugs

use v6;
unshift @*INC, 'ext/File-Spec/lib', '../ext/File-Spec/lib', '../../ext/File-Spec /lib';
require File::Spec;
my ($progdir) = splitpath($*PROGRAM_NAME)[1];
unshift @*INC, $progdir;
require MOTD; 

my $subject   = shift @ARGS || 'Pugs is';
my $surveyed  = shift @ARGS || 20;
my %tally     ;

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
$fh.close;
my $orig = ~@list;

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
while($keyed = =$*IN){
	clear;
	chomp $keyed;
 	my @keyed ;
	if($keyed){
		@keyed = parse_args($keyed);
	}
	@list     = (*@keyed , *@list);
	%tally    = $surveyed .whisper_about( *@list );
	tell(@mostsaid( $most = %tally.values.max));
	@keyed.perl.say;
}

unless $orig eq ~@list {
	say "Do you want to save your changes?";
	print "y/N ..."; 
	my $ans = =$*IN; # is chomped;
	chomp $ans; # hack while 'is chomped' is unimplemented
	if $ans eq any ('y','Y'){
		my $backup = $dict;
		my $incr    = 1;
	  while -f "$backup-$incr" {
			$incr++;
	  }
		$backup ~= "-$incr";
	  rename $dict,"$backup";
		my $newfh = open(">$dict");
		for @list->$line{say $newfh,$line}
		$newfh.close;
		say "diff $backup $dict";
	}
	
}
