#!/usr/bin/pugs

use v6;

# This function multiplies the squaring factors of $n and $m to receive
# the squaring factors of ($n*$m)
sub multiply_squaring_factors($n,$m)
{
    return sort { $^a <=> $^b } one(@$n,@$m).values;
}

my %gsf_cache = (1 => []);

# This function gets the squaring factors of $n.
# The squaring factors are those prime numbers that need to be multiplied
# by $n to reach a perfect square. They are the minimal such number.

sub get_squaring_factors($n,$start_from=2)
{
    if (%gsf_cache.exists($n))
    {
        return %gsf_cache{$n};
    }

    my $p = [//] grep { $n % $_ == 0 } ($start_from .. $n);
    # This function is recursive to make better use of the Memoization
    # feature.
    return 
        (%gsf_cache{$n} = 
            multiply_squaring_factors(
                [$p], 
                get_squaring_factors(int($n / $p), $p)
            )
        );
}


sub Graham($n)
{
    my $n_sq_factors = get_squaring_factors($n);
    
    # The graham number of a perfect square is itself.
    if ($n_sq_factors.elems() == 0)
    {
        return ($n, [$n]);
    }

    # Cheating: 
    # Check if between n and n+largest_factor we can fit
    # a square of SqFact{n*(n+largest_factor)}. If so, return
    # n+largest_factor.
    #
    # So, for instance, if n = p than n+largest_factor = 2p
    # and so SqFact{p*(2p)} = 2 and it is possible to see if
    # there's a 2*i^2 between p and 2p. That way, p*2*i^2*2p is
    # a square number.

    {
        my $largest_factor = $n_sq_factors.[-1];

        my ($lower_bound, $lb_sq_factors);
        
        $lower_bound = $n + $largest_factor;
        loop
        {
            $lb_sq_factors = get_squaring_factors($lower_bound);
            if (grep { $_ == $largest_factor } @$lb_sq_factors)
            {
                last;
            }
            $lower_bound += $largest_factor;
        }

        my $n_times_lb = 
            multiply_squaring_factors($n_sq_factors, $lb_sq_factors);

        my $rest_of_factors_product = [*] @$n_times_lb;

        my $low_square_val = int(sqrt($n/$rest_of_factors_product));
        my $high_square_val = int(sqrt($lower_bound/$rest_of_factors_product));
        
        if ($low_square_val != $high_square_val)
        {
            return ($lower_bound, [$n, ($low_square_val+1)*($low_square_val+1)*$rest_of_factors_product,$lower_bound]);
        }
    }

    # %primes_to_ids_map maps each prime number to its ID. IDs are consective.
    my (%primes_to_ids_map);
    my $next_id = 0;

    # @base is an array that for each ID of a prime number holds the 
    # controlling vector for this number.
    #
    # This is in fact a matrix that is kept stair-shaped and canonized.
    my (@base);

    # The integers whose multiplication compose this vector
    my (@base_composition);

    # Register all the primes in the squaring factors of $n
    for @$n_sq_factors -> $p
    {
        %primes_to_ids_map{$p} = ($next_id++);
    }

    # $n_vec is used to determine if $n can be composed out of the base's 
    # vectors.
    my $n_vec = $n_sq_factors;

    my $n_composition = [ $n ];

    my $i;

    loop ($i = $n+1 ; ; $i++)
    {
        my $i_sq_factors = get_squaring_factors($i);
        
        # Skip perfect squares - they do not add to the solution
        if ($i_sq_factors.elems() == 0)
        {
            next;
        }

        # Check if $i is a prime number
        # We need n > 2 because for n == 2 it does include a prime number.
        #
        # Prime numbers cannot be included because 2*n is an upper bound
        # to G(n) and so if there is a prime p > n than its next mulitple
        # will be greater than G(n).
        if (($n > 2) && ($i_sq_factors[0] == $i))
        {
            next;
        }

        # $final_vec is the new vector to add after it was
        # stair-shaped by all the controlling vectors in the base.

        my $final_vec = $i_sq_factors;

        my $final_composition = [ $i ];

        for @$i_sq_factors -> $p
        {
            if (!%primes_to_ids_map.exists($p))
            {
                # Register a new prime number
                %primes_to_ids_map{$p} = ($next_id++);
            }
            else
            {
                my $id = %primes_to_ids_map{$p};
                if (defined(@base[$id]))
                {
                    $final_vec = multiply_squaring_factors($final_vec, @base[$id]);
                    $final_composition = multiply_squaring_factors($final_composition, @base_composition[$id]);
                }
            }
        }
 
        # Get the minimal ID and its corresponding prime number
        # in $final_vec.
        my $min_id = -1;
        my $min_p = 0;

        for @$final_vec -> $p
        {
            my $id = %primes_to_ids_map{$p};
            if (($min_id < 0) || ($min_id > $id))
            { 
                $min_id = $id;
                $min_p = $p;
            }
        }

        if ($min_id >= 0)
        {
            # Assign $final_vec as the controlling vector for this prime
            # number
            @base[$min_id] = $final_vec;
            @base_composition[$min_id] = $final_composition;
            # Canonize the rest of the vectors with the new vector.
            my $j;
            loop($j=0 ; $j < @base.elems() ; $j++)
            {
                if (($j == $min_id) || (!defined(@base[$j])))
                {
                    next;
                }
                if (grep { $_ == $min_p }, @{@base[$j]})
                {
                    @base[$j] = multiply_squaring_factors(@base[$j], $final_vec);
                    @base_composition[$j] = multiply_squaring_factors(@base_composition[$j], $final_composition);
                }
            }
        }
 
        # A closure to print the base. It is not used but can prove useful.
        my $print_base = sub {
            print "Base=\n\n";
            my $j;
            loop($j=0 ; $j < @base.elems() ; $j++)
            {
                next if (! defined(@base[$j]));
                print "base[$j] (" ~ join(" * ", @{@base[$j]}) ~ ")\n";
            }
            print "\n\n";
        };

        # Check if we can form $n

        while ($n_vec.elems())
        {
            # Assing $id as the minimal ID of the squaring factors of $p
            my @ids_vec = (sort { $^a <=> $^b } %primes_to_ids_map{@$n_vec});
            my $id = @ids_vec[0];
            # Mulitply by the controlling vector of this ID if such one exists
            # or terminate if there isn't such.
            last if (!defined(@base[$id]));
            $n_vec = multiply_squaring_factors($n_vec, @base[$id]);
            $n_composition = multiply_squaring_factors($n_composition, @base_composition[$id]);
        }
        if ($n_vec.elems() == 0)
        {
            return ($i, $n_composition);
        }
    }
}

my $start = int(@*ARGS.shift);
my $end = int(@*ARGS.shift || $start);
my $n;
for $start..$end -> $n
{
    my ($g_val, $composition) = Graham($n);
    say "G($n) = $g_val [{join(",", @$composition)}]";
}



