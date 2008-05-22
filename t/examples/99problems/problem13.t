use v6;
use Test;
plan 8;

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


# Alternative solution

sub encode_direct2(*@array is copy) returns Str {
    my ($packed, $count);
    while @array {
      if @array[0] eq @array[1] {
          $count++;
      }
      else {
          $packed ~=( $count ?? ($count+1) ~ @array[0] !! @array[0] );
          $count=0;
      }
      @array.shift;
    }
    return $packed;
}

for (&encode_direct,&encode_direct2) ->$ed {
is $ed(<>),'', 'We should be able to encode_direct an empty list';
is $ed(<a>), 'a', '.. or a one-element iist';
is $ed(<a a>), '2a', '.. or a n-ary list with always same element';
is $ed(<a a a a b c c a a d e e e e>),
    '4ab2c2ad4e',
    '.. or a generic list'; 
}
