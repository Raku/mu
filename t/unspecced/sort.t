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
    sub mergesort (@values is rw, Code @by? = list(&infix:<cmp>),
        Bit $inplace? = 0)
    {
        if $inplace {
            inplace_mergesort(@values, 0 => +@values, @by);
            return @values;
        }
        else {
            my @copy = @values;
            inplace_mergesort(@copy, 0 => +@copy, @by);
            return @copy;
        }
    }

    our Pair
    sub inplace_mergesort (@values is rw, Pair $span, Code @by)
    {
        return $span
            if ( $span.value - $span.key == 1
            || in_order(@by, @values) );

        my $mid = $span.key + int( ($span.value - $span.key)/ 2 );

        return merge(
            @values,
            inplace_mergesort(@values, $span.key => $mid, @by),
            inplace_mergesort(@values, $mid => $span.value, @by),
            @by
        );
    }

    our Pair
    sub merge (@values is rw, Pair $lspan, Pair $rspan, Code @by)
    {
        # copy @left to a scratch area
        my @scratch = @values[$lspan.key ..^ $lspan.value];

        # merge @scratch and @right into and until @left is full
        my $lc = $lspan.key;
        my $rc = $rspan.key;
        my $sc = 0;

        while ( $lc < $lspan.value ) {
            @values[$lc++] = by_cmp(@by, @scratch[$sc], @values[$rc]) <= 0
                ?? @scratch[$sc++]
                !! @values[$rc++];
        }

        # at this point @left is full.  start populating @right
        # until @scratch or @right is empty
        my $ri = $rspan.key;

        while ( $sc < +@scratch && $rc < $rspan.value ) {
            @values[$ri++] = by_cmp(@by, @scratch[$sc], @values[$rc]) <= 0
                ?? @scratch[$sc++]
                !! @values[$rc++];
        }

        # anything remaining in @right is in the correct place.
            # anything remaining in @scratch needs to be filled into @right
            @values[$ri..^$rspan.value] = @scratch[$sc..^+@scratch];

        # return the merged span
            return $lspan.key => $rspan.value;
    }
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
