#!/usr/bin/pugs

use v6;

my $VERSION = '0.1';

for @*ARGS {
    when <--version> { version() };
    when <--help>    { help() };
    when <-->        { shift @*ARGS; last };
}

@*ARGS = <y> unless @*ARGS;

loop { say @*ARGS.join(" ") };

sub version {
    say "$*PROGRAM_NAME (Perl6 Power Tools) $VERSION";
    exit;
}

sub help {
    #say qq:t/EOF/;
    $_ = "
    $*PROGRAM_NAME [OPTION] [STRING...]

    Repeatedly print its arguments, or 'y'.

    Options:
           --version:  Print version number, then exit.
           --help:     Print usage, then exit.
           --:         Stop parsing options.
    ";
    say;
    #EOF
    exit;
}
