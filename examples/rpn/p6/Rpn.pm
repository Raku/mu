module Rpn-0.0.1-cpan:ASAVIGE;

# Super simple rpn evaluator derived from:
#   http://perlgolf.sourceforge.net/TPR/0/5a/tpr05a.pl
# Only handles operators: + - * /

# Pugs note: change "die" to "fail" when "fail" is implemented.

# Input a string containing a rpn expression.
# Return the evaluated integer result.
# Example: "5 4 +" returns 9
sub evaluate (Str $expr) returns Int {
    my @stack;
    for ($expr.split()) -> $tok {
        if $tok ~~ rx:Perl5/-?\d+/ {
            @stack.push($tok);
            next;
        }
        my $x = @stack.pop() err die "Stack underflow\n";
        my $y = @stack.pop() err die "Stack underflow\n";

        # given/when is a sexy new P6 construct that can avoid
        # long if/elsif/else chains
        given $tok {
            when '+' { @stack.push($y + $x) }
            when '-' { @stack.push($y - $x) }
            when '*' { @stack.push($y * $x) }
            when '/' { @stack.push($y / $x) }
            default { die "Invalid token: \"$tok\"\n" }
        }
    }

    @stack.elems == 1 or die "Invalid stack:[@stack[]]\n";
    return @stack[0];
}
