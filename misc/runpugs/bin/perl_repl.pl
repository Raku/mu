#!/usr/bin/perl
use strict;
use warnings;
#use lib '../lib','/home/andara/lib/perl/5.8.8';
use Repl;
my $prompt ='pugs> ';
my $motd ='Simple Perl REPL';
my $subref = sub {
        my $cmd = shift;
        my $res = eval($cmd);
        return ($res,$prompt);
};


my $repl = new Repl {subref=>$subref,prompt=>$prompt,motd=>$motd};

$repl->run();
