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
     * guidance on `primitive`

=cut

# L<S29/"List"/"=item sort">

my $prelude_sort = q:to'END_PRELUDE_SORT'

subset KeyExtractor of Code(Any --> Any);
subset Comparator   of Code(Any, Any --> Int);
subset SortCriterion
    of KeyExtractor | Comparator | Pair(KeyExtractor, Comparator);

module Prelude::Sort {
    our Any
    sub kex (KeyExtractor $ex, $v) is cached { $ex($v); }

    our List
    sub handle_kex (KeyExtractor $ex, $a, $b)
    {
        my $asc = $ex !~~ descending;

        my $y = kex($ex, $asc ?? $a !! $b);
        my $z = kex($ex, $asc ?? $b !! $a);

        ( $y, $z ) = $y.lc, $z.lc
            if $ex ~~ insensitive;

        list($y, $z);
    }

    our Order
    sub by_cmp (SortCriterion @by, $a, $b)
    {
        my $result = Order::Same;
        my &return_ifn0 ::= -> $v { if $v { $result = $v; leave LOOP; } };

        LOOP: for @by -> $criterion {
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
                #
                # Probably more correct would be:
                #
                #   given $criterion.of() {
                #       when :(Num) { return_inf0 infix:{'<=>'}($ka, $kb); }
                #       default { return_inf0 infix:<cmp>($ka, $kb); }
                #   }
                #
                # However .signature..isof() is just conjecture at
                # this time for how # you would access the 'of'
                # return-type of a Code object.

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

        $result;
    }

    our bool
    sub in_order (SortCriterion @by, *$x, *@xs)
    {
        my $result = 1;
        my $y := $x;

        for @xs -> $z {
            if by_cmp(@by, $y, $z) > 0 {
                $result = 0;
                last;
            }

            $y := $z;
        }

        $result;
    }

    # mergesort() --
    #   O(N*log(N)) comparisons
    #   O(N) space
    #   stable

    our Array
    sub mergesort (@values is rw, Code @by? = list(&infix:<cmp>),
        Bit $inplace? = 0)
    {
        my @result;

        if $inplace {
            inplace_mergesort(@values, 0 => +@values, @by);
            @result := @values;
        }
        else {
            my @copy = @values;
            inplace_mergesort(@copy, 0 => +@copy, @by);
            @result := @copy;
        }

        @result;
    }

    our Pair
    sub inplace_mergesort (@values is rw, Pair $span, Code @by)
    {
        my $result = $span;

        unless ( $span.value - $span.key == 1 || in_order(@by, @values) ) {
            my $mid = $span.key + int( ($span.value - $span.key)/ 2 );

            $result = merge(
                @values,
                inplace_mergesort(@values, $span.key => $mid, @by),
                inplace_mergesort(@values, $mid => $span.value, @by),
                @by
            );
        }

        $result;
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
        $lspan.key => $rspan.value;
    }
}

our Array multi Array::sort( @values is rw, *&by, Bit $inplace? )
{
    Prelude::Sort::mergesort(@values, list(&by), $inplace);
}

our Array multi Array::sort( @values is rw, SortCriterion @by, Bit $inplace? )
{
    Prelude::Sort::mergesort(@values, @by, $inplace);
}

our Array multi Array::sort( @values is rw, SortCriterion $by = &infix:<cmp>,
    Bit $inplace? )
{
    Array::sort(@values, $by, $inplace);
}

our List multi List::sort( SortCriterion @by, *@values )
{
    my @result = Prelude::Sort::mergesort(@values, @by);
    @result[];
}

our List multi List::sort( SortCriterion $by = &infix:<cmp>, *@values )
{
    my @result = Prelude::Sort::mergesort(@values, list($by));
    @result[];
}
END_PRELUDE_SORT;

eval_ok($prelude_sort, 'prelude sort parses', :todo,
    :depends<subset and argument list return signatures>);
