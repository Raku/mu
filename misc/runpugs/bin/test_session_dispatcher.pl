#!/usr/bin/perl
use warnings;
use strict;
use lib '../lib/';
use Repl;
use Web::Terminal::Settings;
use Web::Terminal::Dispatcher;
$Web::Terminal::Settings::port=2059;

my $app=(defined $ARGV[0])?$ARGV[0]:2;
my $id=(defined $ARGV[1])?$ARGV[1]:42;
$id='testsession'.$id;
my $ip="127.0.0.1";

my $subref= sub {
	my $cmd=shift;
(my $reply,my $prompt,my $histref) =
&Web::Terminal::Dispatcher::send($id,$ip,$app,1,$cmd);
return $reply;
};

my $prompt ='pugs> ';
my $motd ='Simple Perl REPL';

my $repl = new Repl {subref=>$subref,prompt=>$prompt,motd=>$motd};

$repl->run();

