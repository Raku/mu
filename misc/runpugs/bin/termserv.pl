#!/usr/bin/perl
use strict;
use warnings;
use lib './lib','../lib';
#BEGIN {
#$ENV{PERLIO}= ":utf8";
#}
#use utf8;

use warnings;
use strict;

use Web::Terminal::Server;
use  Web::Terminal::Settings;
$ENV{PUGS_SAFEMODE}=1;
my $v=1-$Web::Terminal::Settings::daemon;
print "Starting server\n" if $v;
&Web::Terminal::Server::run();

