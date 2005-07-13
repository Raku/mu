#!/usr/bin/pugs
use v6;

sub quit () {
    say "Leaving interactive shell...";
    exit(0);
}

say "^Z doesn't work, please use :q for now.";

my $EOF = chr(26);

loop (;;) {
    print "pugs> ";
    my $line = =$*IN;
    
    given ($line) {
        when ""         { next; }
        when $EOF       { quit(); }
        when /^\:e /    { }
        when /^\:E /    { }
        when /^\:d /    { }
        when /^\:D /    { }
        when /^\:q /    { quit(); }
        when /^\:h /    { }
        when /^\:r /    { }
        when /^\:l /    { }
        default {
            my $ret = eval $_;
            if ($!) {
                say $!;
            } else {
                say $ret;
            }
        }
    }
}

quit();