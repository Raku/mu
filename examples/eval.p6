#!/usr/bin/pugs
use v6;

# Todo list:
# - ^Z should exit.
# - There must be a cleaner way of presenting .perl without a leading \.
# - Need to implement :{e,E,d,D,h,r,l}.

sub quit () {
    say "Leaving interactive shell...";
    exit(0);
}

say "^Z doesn't work, please use :q for now.";

my $EOF = chr(26);

loop (;;) {
    print "pugs> ";
    my $line = =$*IN;
    chomp($line);
    
    given ($line) {
        when ""         { } # XXX should simply move to the next iteration of the outer loop
        when $EOF       { quit(); }
        when /^\: (.) \s+/ {
            given ($0) {
                when "e" {
                    # XXX
                }
                when "E" {
                    # XXX
                }
                when "d" {
                    # XXX
                }
                when "D" {
                    # XXX
                }
                when "q" {
                    quit();
                }
                when "h" {
                    # XXX
                }
                when "r" {
                    # XXX
                }
                when "l" {
                    # XXX
                }
            }
        }
        default {
            my $ret = eval $_;
            if ($!) {
                say $!;
            } else {
                #say $ret.perl;
                say $ret.perl.substr(1, $ret.perl.chars - 1); # XXX NASTY HACK
            }
        }
    }
}

quit();

=head1 NAME

eval.p6 - simple read-eval-print loop implementation

=head1 DESCRIPTION

This simplistic program will keep reading from standard input and evaluating whatever is
typed.

=end