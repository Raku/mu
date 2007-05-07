#!/usr/bin/perl
use strict;
use warnings;
use lib '../lib';
use Repl;

my $subref = sub {
        my $cmd = shift;
        my $res = eval($cmd);
        return $res;
};
my $prompt ='pugs> ';
my $motd ='Simple Perl REPL';

my $repl = new Repl {subref=>$subref,prompt=>$prompt,motd=>$motd};

$repl->run();
