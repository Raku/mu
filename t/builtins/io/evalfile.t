#!/usr/bin/pugs

use v6;
use Test;

plan 1;

sub nonce () { return (".$*PID." ~ int rand 1000) }


my $tmpfile = "temp-evalfile" ~ nonce();
{
    my $fh = open "> $tmpfile";
    say $fh, "32 + 10";
    close $fh;
}

is evalfile($tmpfile), 42, "evalfile() works";

END { unlink $tmpfile }
