#!/usr/bin/pugs
use v6-alpha;

# Solution to exercise 2.2.1-7 in TAOCP 1.
#
# I'm a bit wary to repeat any of the material from the book here due to
# copyright restrictions, but there's an introduction to deques on Wikipedia:
#
#  <http://en.wikipedia.org/wiki/Deque>
#
# The exercise is about finding out which permutations of 1 2 3 4 can be
# produced (a) with an input-restricted deque but not with an
# output-restricted deque, (b) vice versa, and (c) with neither. As it happens,
# each of these three conditions are met by exactly one among the
# 24 permutations.
#
# Knuth describes it better than I do, though. If you have his book in the
# shelf, have a look at it there. -- masak

sub seqOfOperationsForIRDeque( $wagonsOut ) {
    my $nextWagonOnTurn = 1;
    my (@deque, @operations);

    for @$wagonsOut -> $wagonOut {

        while $wagonOut >= $nextWagonOnTurn {
            push @deque, $nextWagonOnTurn++;
            push @operations, 'IR';
        }

        if $wagonOut == @deque[0] {
            shift @deque;
            push @operations, 'OL';
        }
        elsif $wagonOut == @deque[-1] {
            pop @deque;
            push @operations, 'OR';
        }
        else {
            return [];
        }
    }

    return @operations;
}

sub seqOfOperationsForORDeque( $wagonsOut ) {
    my $wagonOutIndex = 0;
    my (@deque, @operations);

    for 1 .. $wagonsOut.elems -> $nextWagonOnTurn {

        if ( !@deque || indexOf( $wagonsOut, @deque[0]        ) >
                        indexOf( $wagonsOut, $nextWagonOnTurn ) ) {

            unshift @deque, $nextWagonOnTurn;
            push @operations, 'IL';
        }
        elsif ( indexOf( $wagonsOut, @deque[-1]       ) <
                indexOf( $wagonsOut, $nextWagonOnTurn ) ) {

            push @deque, $nextWagonOnTurn;
            push @operations, 'IR';
        }
        else {
            return [];
        }

        while ( $wagonOutIndex < $wagonsOut.elems &&
                $wagonsOut[$wagonOutIndex] == @deque[0] ) {

            shift @deque;
            push @operations, 'OL';
            ++$wagonOutIndex;
        }
    }

    push @operations, 'OL' for @deque;

    return @operations;
}

sub indexOf( $array, $element ) {
    for $array.kv -> $index, $elem {
        return $index if $element == $elem;
    }

    return -1;
}

sub permutations( @numbers ) {
    given @numbers.elems {
        when 0 { return (); }
        when 1 { return @numbers[0]; }
    }

    return gather {
        for @numbers -> $number {
            for permutations( without(@numbers, $number) ) -> $perm {
                take [ $number, @$perm ];
            }
        }
    };
}

sub without( $array, $element ) {
    return gather {
        for @$array -> $elem {
            take $elem if $element != $elem;
        }
    };
}

.say for
    "Permutation\tIR deque\t\tOR deque\t\tAnswer",
    "-----------\t--------\t\t--------\t\t------",
    '';

for permutations( 1 .. 4 ) -> $permutation {

  print $permutation.perl;

  my $IRsolution = seqOfOperationsForIRDeque( $permutation );
  print "\t", $IRsolution.elems ?? join '-', @$IRsolution !! "no solution\t";

  my $ORsolution = seqOfOperationsForORDeque( $permutation );
  print "\t", $ORsolution.elems ?? join '-', @$ORsolution !! "no solution\t";

  print "\t";

  print '(a)' if  $IRsolution && !$ORsolution;
  print '(b)' if !$IRsolution &&  $ORsolution;
  print '(c)' if !$IRsolution && !$ORsolution;

  print "\n";
}
