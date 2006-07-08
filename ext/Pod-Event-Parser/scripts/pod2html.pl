use v6-alpha;

BEGIN {
    require lib;
    import('lib': 'ext/Pod-Event-Parser/lib/');
}

use Pod::Event::Parser;
use Pod::Event::Handler::HTML;

die "usage:
    pod2html.pl <POD file>
" unless @*ARGS;

my $file = @*ARGS[0];
die "Cannot locate '$file'" unless -e $file; 

my %events = pod2html($*OUT);
parse($file, %events);
