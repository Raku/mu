#!/usr/bin/pugs

use v6;

#XXX just so that File:;Spec can be used w/o being installed
unshift @*INC, 'ext/File-Spec/lib', '../ext/File-Spec/lib', '../../ext/File-Spec /lib';
use File::Spec;
my @path_parts   = splitpath($*PROGRAM_NAME);
my $progdir      = @path_parts[1];
unshift @*INC, $progdir;
require MOTD; 

# XXX unimplemented: should be able to say 
# my @list = =$fh is chomped;
my $subject   = shift @ARGS || 'Pugs';
my $surveyed  = shift @ARGS || 20;
my $dict      = shift @ARGS || canonpath("$progdir/pugspraise");
my $fh        = open "<$dict" or die $!;
my @list      ;

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

my %opinions = $surveyed  # number 
               .whisper_about( @list) ;

my $most     = %opinions.values.max;
my &tell := -> $limit {
		say "$subject is{ 
			report matchval $most,%opinions,$limit;
		}."
};
tell 2;
