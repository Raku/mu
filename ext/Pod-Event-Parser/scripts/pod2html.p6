#!/usr/bin/pugs

use v6;

BEGIN {
    require lib;
    import('lib': 'ext/Pod-Event-Parser/lib/');
}

use Pod::Event::Parser;
use Pod::Event::Handler::HTML;

die "usage:
    pod2html.p6 <POD file>
" unless @*ARGS;

my $file = @*ARGS[0];
die "Cannot locate '$file'" unless -e $file; 

my %events = pod2html($*OUT);
parse($file, %events);