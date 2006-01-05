package Rpn;

# Super simple rpn evaluator derived from:
#   http://perlgolf.sourceforge.net/TPR/0/5a/tpr05a.pl
# Only handles operators: + - * /

use strict;
use warnings;

# Input a string containing a rpn expression.
# Return the evaluated integer result.
# Example: "5 4 +" returns 9
sub evaluate {
    my ($expr) = @_;
    my @stack;
    for my $tok (split ' ', $expr) {
        if ($tok =~ /^-?\d+$/) {
            push @stack, $tok;
            next;
        }
        my $x = pop @stack;
        defined $x or die "Stack underflow\n";
        my $y = pop @stack;
        defined $y or die "Stack underflow\n";
        if ($tok eq '+') {
            push @stack, $y + $x;
        } elsif ($tok eq '-') {
            push @stack, $y - $x;
        } elsif ($tok eq '*') {
            push @stack, $y * $x;
        } elsif ($tok eq '/') {
            push @stack, int($y / $x);
        } else {
            die "Invalid token:\"$tok\"\n";
        }
    }
    @stack == 1 or die "Invalid stack:[@stack]\n";
    return $stack[0];
}

1;
