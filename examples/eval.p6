#!/usr/bin/pugs
use v6;

# Todo list:
# - There must be a cleaner way of presenting .perl without a leading \.
# - Need to implement :{e,E,d,D,h,r,l}.

sub quit () {
    say "Leaving interactive shell...";
    exit(0);
}

# We'd like to not pollute the eval's environment with our vars ($line, etc.).
sub clean_eval ($___str) { eval $___str }

say "Welcome to Pugs -- $?PUGS_VERSION";
say "Type :h for help.";

loop (;;) {
    # XXX use :prompt
    #my $line = =$*IN :prompt('pugs> ');
    print "pugs> ";
    my $line = =$*IN;

    # Quit and EOF.
    quit() unless defined $line;
    # Skip empty lines.
    next   if $line eq "";
    
    given ($line) {
        when rx:P5/^\:(.)/ {
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
            # We need to .perl the result right here to make prettyprinting of
            # junctions work.
            my $ret = clean_eval $line;
            say chomp $! and next if $!;

            # We've to .perl in a second pass (and in a try {...} block) to
            # catch "fail_"s.
            $ret    = try { $ret.perl };
            say chomp $! and next if $!;
            say substr($ret, 0, 1) eq "\\" ?? substr($ret, 1) !! $ret;   # XXX NASTY HACK
            #" #--vim
        }
    }
}

quit();

=head1 NAME

eval.p6 - simple read-eval-print loop implementation

=head1 DESCRIPTION

This simplistic program will keep reading from standard input and evaluating whatever is
typed.
