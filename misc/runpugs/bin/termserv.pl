#!/usr/bin/perl
use warnings;
use strict;
use utf8;
use lib '../lib/';
use WebTerminal::Server;
my $host = 'localhost';
my $port = 2057;
$ENV{PUGS_SAFEMODE}=1;
&WebTerminal::Server::run($host,$port);

