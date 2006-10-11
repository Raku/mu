#!/usr/bin/perl
use warnings;
use strict;
use lib '../lib/';
use WebTerminal::Server;
#$SIG{USR1}=\&timeout; #sub {print time,"\n"};
my $host = 'localhost';
my $port = 2057;
&run($host,$port);

