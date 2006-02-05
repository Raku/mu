#!/usr/bin/pugs

use v6;

if !@*ARGS {
    die "usage: $*PROGRAM_NAME format [argument ...]\n";
}

print sprintf @*ARGS.shift, @*ARGS;
$*OUT.flush;
