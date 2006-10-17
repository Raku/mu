#!/usr/bin/perl
use warnings;
use strict;
use utf8;
use lib '../lib/';
use Web::Terminal::Server;
use  Web::Terminal::Settings;
$Web::Terminal::Settings::port=2058;
$ENV{PUGS_SAFEMODE}=1;
&Web::Terminal::Server::run();

