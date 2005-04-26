#!/usr/bin/pugs

use v6;

use Test;

plan 4;
force_todo 4;

	my $a := $_; 
	for 1 .. 3 { $a++ }; 
	is $a, 3, 'global $_ increments' ;

# work around missing capabilities
# to get the output of 'say' into a test; 
	my $out = open ">tmpfile" ;  
	say $out, ;  
	close $out; 
	my$in = open "<tmpfile"; 
	my $s = =$in; close $in; 
	unlink "tmpfile";

	is $s,"3\n", 'and is the default argument for "say"';

#pugs> for 1 .. 3 { say }; 

	my $out = open ">tmpfile" ;  
    for 1 { say $out, };
	close $out; 
	my$in = open "<tmpfile"; 
	my $s = =$in; close $in;
	unlink "tmpfile";

	isnt $s,"3\n", 'and global $_ should not be the default topic of "for"'; 

    eval_ok 'for 1 .. 3 { $_++ } ', 'default topic is rw by default'; 
# #*** Error: cannot modify constant item at 1

