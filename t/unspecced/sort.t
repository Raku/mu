use v6-alpha;

use Test;

plan 1;

# This is a perl6 implementation of L<S29/List/"=item sort">
# based on mergesort.

=for TODO

  * Pugs
     * `subset`
     * return signature using `-->`

  * Spec
     * clarify KeyExtractor returning multiple types

  * Design cleanup
     * recursive mergesort won't recurse deep but using
       slices will keep a lot of data in play.  Rework
       to use indices or work iteratively.

  * Syntax cleanup
     * is a module the best way to package the implementation?
     * guidance on `primitive`

=cut

# L<S29/"List"/"=item sort">

my $prelude_sort = q:to'END_PRELUDE_SORT'

subset KeyExtractor of Code(Any --> Any);
subset Comparator   of Code(Any, Any --> Int);
subset SortCriterion
    of KeyExtractor | Comparator | Pair(KeyExtractor, Comparator);

module Prelude::MergeSort {
    my &return_ifn0 ::= -> $v { return $v if $v; }

    our Any
    sub kex (KeyExtractor $ex, $v) is cached { $ex($v); }

    our Array
    sub handle_kex (KeyExtractor $ex, $a, $b)
    {
        my $asc = $ex !~~ descending;

        my $y = kex($ex, $asc ?? $a !! $b);
        my $z = kex($ex, $asc ?? $b !! $a);

        ( $y, $z ) = $y.lc, $z.lc
            if $ex ~~ insensitive;

        return $y, $z;
    }

    our Order
    sub by_cmp (SortCriterion @by, $a, $b)
    {
        for @by -> $criterion {
            when Comparator {
                return_ifn0 $criterion($a, $b);
            }
    
            when KeyExtractor {
                my ($ka, $kb) = handle_kex($criterion, $a, $b);
    
                # Spec does not discuss if key extractor returns
                # Num for one side of comparison but Str for other.
                # Handle this case in a useful way.  I.e., Nums
                # will sort numerically within the typographical
                # sort of strings.  (No more having to fmt('%02d')
                # all of your ints.)

                given $ka {
                    when Num {
                        given $kb {
                            when Num {
                                return_inf0 infix:{'<=>'}($ka, $kb);
                            }
    
                            default {
                                return_ifn0 infix:<cmp>($ka, $kb);
                            }
                        }
                    }
    
                    default {
                        return_ifn0 infix:<cmp>($ka, $kb);
                    }
                }
            }

            when Pair {
                my KeyExtractor $ex = $criterion.key;
                my Comparator $comp = $criterion.value;
            
                my ($ka, $kb) = handle_kex($ex, $a, $b);
            
                return_ifn0 $comp($ka, $kb);
            }
        }

        return Order::Same;
    }

    our Bool
    sub in_order (SortCriterion @by, *$x, *@xs)
    {
        my $y := $x;

        for @xs -> $z {
            return Bool::False if by_cmp(@by, $y, $z) > 0;

            $y := $z;
        }

        return Bool::True;
    }

    # mergesort() --
    #   O(N*log(N)) comparisons
    #   O(N) space
    #   stable

    our Array
    sub mergesort (@values is rw, SortCriterion @by, Bit $inplace? = 0)
    {
        return @values if ( in_order(@by, @values) );

        my $mid = int( +@values / 2 );

        my @result = merge(
            mergesort(@values[0..^$mid],
            mergesort(@values[$mid..^+@values],
            @by, $inplace
        );

        return $inplace ?? @values !! @result;
    }

    our Array
    sub merge (@left is rw, @right is rw, SortCriterion @by, Bit $inplace? = 0)
    {
        unless $inplace {
            my $lc = 0;
            my $rc = 0;

            return gather {
                while ( $lc < +@left && $rc < +@right ) {
                    take( by_cmp(@by, left[$lc], @right[$rc]) <= 0
                        ?? @left[$lc++]
                        !! @right[$rc++] );
                }

                take(@left[$lc..^+@left]);
                take(@right[$rc..^+@right]);
            };
        }
        else {
        # copy @left to a scratch area
        my @scratch = @left;

        # merge @scratch and @right into and until @left is full
        my $lc = 0;
        my $rc = 0;
        my $sc = 0;

        while ( $lc < +@left ) {
            @left[$lc++] = by_cmp(@by, @scratch[$sc], @right[$rc]) <= 0
                ?? @scratch[$sc++]
                !! @right[$rc++];
        }

        # at this point @left is full.  start populating @right
        # until @scratch is empty
        my $ri = 0;
        while ( $sc < +@scratch ) {
            @right[$ri++] = by_cmp(@by, @scratch[$sc], @right[$rc]) <= 0
                ?? @scratch[$sc++]
                !! @right[$rc++];
        }

        # anything remaining in @right is in the correct place.
    }

    return;
}

our Array multi Array::sort( @values is rw, *&by, Bit $inplace? )
{
    return Prelude::MergeSort::mergesort(@values, list(&by), $inplace);
}

our Array multi Array::sort( @values is rw, SortCriterion @by, Bit $inplace? )
{
    return Prelude::MergeSort::mergesort(@values, @by, $inplace);
}

our Array multi Array::sort( @values is rw, SortCriterion $by = &infix:<cmp>,
    Bit $inplace? )
{
    return Array::sort(@values, $by, $inplace);
}

our List multi List::sort( SortCriterion @by, *@values )
{
    my @result = Prelude::MergeSort::mergesort(@values, @by);
    return @result[];
}

our List multi List::sort( SortCriterion $by = &infix:<cmp>, *@values )
{
    my @result = Prelude::MergeSort::mergesort(@values, list($by));
    return @result[];
}
END_PRELUDE_SORT;

eval_ok($prelude_sort, 'prelude sort parses', :todo,
    :depends<subset and argument list return signatures>);
