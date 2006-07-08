use v6-alpha;

our Int multi Num::round_half_up_symmetric ( Num $x ) {
    my $x_round;

    # round .5 up above 0, down below 0
    given abs($x) - floor( abs($x) ) {
        when $_ <  .5 { $x_round = sign($x) * floor( abs($x) ) }
        when $_ >= .5 { $x_round = sign($x) * ceil( abs($x) ) }
    }
    return $x_round;
}

our Int multi Num::round_half_up_asymmetric ( Num $x ) {
    my $x_round;

    # round .5 up above 0, up below 0
    given abs($x) - floor( abs($x) ) {
        when ($_ <  .5) && ($x >= 0) { $x_round = sign($x) * floor( abs($x) ) }
        when ($_ >= .5) && ($x >= 0) { $x_round = sign($x) * ceil( abs($x) ) }
        when ($_ <= .5) && ($x <  0) { $x_round = sign($x) * floor( abs($x) ) }
        when ($_ >  .5) && ($x <  0) { $x_round = sign($x) * ceil( abs($x) ) }
    }
    return $x_round;
}

our Int multi Num::round_half_down_symmetric ( Num $x ) {
    my $x_round;

    # round .5 down above 0, down below 0
    given abs($x) - floor( abs($x) ) {
        when $_ <= .5 { $x_round = sign($x) * floor( abs($x) ) }
        when $_ >  .5 { $x_round = sign($x) * ceil( abs($x) ) }
    }
    return $x_round;
}

our Int multi Num::round_half_down_asymmetric ( Num $x ) {
    my $x_round;

    # round .5 down above 0, up below 0
    given abs($x) - floor( abs($x) ) {
        when ($_ <= .5) && ($x >= 0) { $x_round = sign($x) * floor( abs($x) ) }
        when ($_ >  .5) && ($x >= 0) { $x_round = sign($x) * ceil( abs($x) ) }
        when ($_ <  .5) && ($x <  0) { $x_round = sign($x) * floor( abs($x) ) }
        when ($_ >= .5) && ($x <  0) { $x_round = sign($x) * ceil( abs($x) ) }
    }
    return $x_round;
}

our Int multi Num::round_half_even ( Num $x ) {
    my $x_round;

    # round .5 to the nearest even number
    given abs($x) - floor( abs($x) ) {
        when $_ < .5 { $x_round = sign($x) * floor(abs($x)) }
        when $_ > .5 { $x_round = sign($x) * ceil(abs($x)) }
        when $_ == .5 {
            given floor( abs($x) ) % 2 {
                when $_ == 0 { $x_round = sign($x) * floor( abs($x) ) }
                when $_ == 1 { $x_round = sign($x) * (floor( abs($x) ) + 1) }
            }
        }
    }
    return $x_round;
}

our Int multi Num::round_half_odd ( Num $x ) {
    my $x_round;

    # round .5 to the nearest odd number
    given abs($x) - floor( abs($x) ) {
        when $_ < .5 { $x_round = sign($x) * floor(abs($x)) }
        when $_ > .5 { $x_round = sign($x) * ceil(abs($x)) }
        when $_ == .5 {
            given floor( abs($x) ) % 2 {
                when $_ == 0 { $x_round = sign($x) * (floor( abs($x) ) + 1) }
                when $_ == 1 { $x_round = sign($x) * floor( abs($x) ) }
            }
        }
    }
    return $x_round;
}

our Int multi Num::round_alternate ( Num $x ) {
    my $x_round;
    state $round_up = 1;

    # alternate rounding .5 up and down
    given abs($x) - floor( abs($x) ) {
        when $_ < .5 { $x_round = sign($x) * floor(abs($x)) }
        when $_ > .5 { $x_round = sign($x) * ceil(abs($x)) }
        when $_ == .5 {
            if $round_up {
                $x_round = round_up_symmetric($x);
                $round_up = 0;
            }
            else {
                $x_round = round_down_symmetric($x);
                $round_up = 1;
            }
        }
    }

    return $x_round;
}

our Int multi Num::round_random ( Num $x ) {
    my $x_round;
    state $rand_up = 1;

    # randomly round .5 up or down
    given abs($x) - floor( abs($x) ) {
        when $_ <  .5 { $x_round = sign($x) * floor(abs($x)) }
        when $_ >  .5 { $x_round = sign($x) * ceil(abs($x)) }
        when $_ == .5 {
            if $rand_up {
                if rand < .5 {
                    $x_round = round_up_symmetric($x);
                }
                else {
                    $x_round = round_down_symmetric($x);
                }
                $rand_up = 0;
            }
            else {
                if rand <= .5 {
                    $x_round = round_up_symmetric($x);
                }
                else {
                    $x_round = round_down_symmetric($x);
                }
                $rand_up = 1;
            }
        }
    }

    return $x_round;
}

our Int multi Num::round_ceiling ( Num $x ) {
    my $x_round = ceil($x);

    return $x_round;
}

our Int multi Num::round_floor ( Num $x ) {
    my $x_round = floor($x);

    return $x_round;
}

our Int multi Num::round_toward_zero ( Num $x ) {
    my $x_round;

    # round floor above 0, round ceiling below 0
    if $x < 0 {
        $x_round = round_floor($x);
    }
    elsif $x > 0 {
        $x_round = round_ceiling($x);
    }
    else {
        $x_round = 0;
    }

    return $x_round;
}

our Int multi Num::round_away_from_zero ( Num $x ) {
    my $x_round;

    # round ceiling above 0, round floor below 0
    if $x < 0 {
        $x_round = round_ceiling($x);
    }
    elsif $x > 0 {
        $x_round = round_floor($x);
    }
    else {
        $x_round = 0;
    }

    return $x_round;
}

our Int multi Num::sign (Num $x) {
	return 0 if $x == 0;
	$x > 0 ?? 1 !! -1;
}

