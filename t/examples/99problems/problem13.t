use v6-alpha;
use Test;
plan 1;

# P13 (**) Run-length encoding of a list (direct solution).
# 
# Implement the so-called run-length encoding data compression method directly.
# I.e. don't explicitly create the sublists containing the duplicates, as in
# problem P09, but only count them. As in problem P11, simplify the result list
# by replacing the singleton lists (1 X) by X.
# 
# Example:
# * (encode-direct '(a a a a b c c a a d e e e e))
# ((4 A) B (2 C) (2 A) D (4 E))

sub encode_direct {
    my @chars = @_;
    my $encoded;
    my $prev_ch = undef;
    my $ch_cnt = 0;
    while (my $ch = @chars.shift) {
        if ($ch ~~ $prev_ch) {
            $ch_cnt++;
            # If it's the last char, add it.
            if (@chars.elems == 0) {
                if ($ch_cnt != 1) {
                    $encoded ~= $ch_cnt;
                }
                $encoded ~= $ch;
            }
        }
        # the very first one..
        elsif (not $prev_ch.defined) { 
            $ch_cnt++;
            # If it's the last char, add it.
            if (@chars.elems == 1) {
                if ($ch_cnt != 1) {
                    $encoded ~= $ch_cnt;
                }
                $encoded ~= $ch;
            }
        }
        # not a match, but a new letter
        else {
            if ($ch_cnt != 1) {
                $encoded ~= $ch_cnt;
            }
            $encoded ~= $prev_ch;
            $ch_cnt = 1;
        }
        $prev_ch = $ch;
    }

    return $encoded;
}


is( 
    encode_direct(<a a a a b c c a a d e e e e>),
   '4ab2c2ad4e',
    '(**) Run-length encoding of a list (direct solution).'
);



