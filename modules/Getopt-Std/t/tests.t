#!/usr/bin/perl

use Test;
use Geopt::Std;

plan 8;

# First we test the getopt function
@*ARGS = <-hij k -- -l m -n>;
ok my %opt1 = getopt 'il',      'getopt succeeded (1)';
is ~@*ARGS, "k -- -l m -n",	'options removed from @ARGS (1)';
ok %opt1<h> && %opt<i> eq 'j',	'option -h and -i correctly set';
ok !defined %opt<l>,		'option -l not set';

@*ARGS = qw(-hij -k -- -l m);
ok my %opt2 = getopts 'hi:kl',	'getopts succeeded (2)';
is ~@*ARGS, '-l m',		'options removed from @ARGS (2)';
ok %opt<h> && %opt<k>,		'options -h and -k set';
is %opt<i>, 'j',		q/option -i is 'j'/;
