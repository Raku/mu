#!/usr/bin/perl
use strict;
use warnings;
BEGIN {
$ENV{PERLIO}= ":utf8";
}
use warnings;
use strict;
use utf8;

use lib '../lib/';
use Web::Terminal::Server3;
use  Web::Terminal::Settings;
#$Web::Terminal::Settings::port=2058;
$ENV{PUGS_SAFEMODE}=1;
my $v=1-$Web::Terminal::Settings::daemon;
print "Starting server\n" if $v;
&Web::Terminal::Server3::run();

