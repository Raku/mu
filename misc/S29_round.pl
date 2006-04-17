our Int multi Num::round_toward_nearest (Num $x, int $symmetrical? = 1) {
# XXX -- Not implemented
# How should I implement this? As in
# http://www.pldesignline.com/howto/showArticle.jhtml?articleID=175801189
# says, This may be considered as the superset of round_half_up and
# round_half_down

}

our Int multi Num::round_half_up (Num $x, int $symmetrical? = 1) {
	return int($x + 0.5)
		if $x > 0;

	$symmetrical ??
		-(int(abs($x) + 0.5)) !!
		-(int(abs($x) + 0.4))
}

our Int multi Num::round_half_down (Num $x, int $symmetrical? = 1) {
	return int($x + 0.4)
		if $x > 0;

	$symmetrical ??
		-(int(abs($x) + 0.4)) !!
		-(int(abs($x) + 0.5))
}

# No symmetry flag
our Int multi Num::round_half_even (Num $x) {
	return int($x.is_even ?? $x + 0.4 !! $x + 0.5)
		if $x > 0;

	return 0 if $x == 0;

	return -(&?ROUTINE(abs($x)));
}

our Int multi Num::round_half_odd (Num $x) {
	return int($x.is_odd ?? $x + 0.4 !! $x + 0.5)
		if $x > 0;

	return 0 if $x == 0;

	return -(&?ROUTINE(abs($x)));
}

our Int multi Num::round_alternate (Num $x) {
# XXX I don't understand what round-alternate means.
# But in my understanding, The implemention below might be right.
	state $t;
	if $t {
		$t = 0;
		return $x.round_half_even;
	} else {
		$t = 1;
		return $x.round_half_odd;
	}
}

our Int multi Num::round_random (Num $x) {
	...
}

our Int multi Num::round_ceiling (Num $x) {
	my Int $t = int($x);
	$x > 0 && $x != $t ??
		$t + 1 !! $t
}

our Int multi Num::round_floor (Num $x) {
	my Int $t = int($x);
	$x > 0 || $x == $t ??
		$t !! $t - 1
}

our Int multi Num::round_toward_zero (Num $x) {
	return(int $x);
}

our Int multi Num::round_away_from_zero (Num $x) {
	return 0 if $x == 0;

	my Int $t = int($x);
	return $x if $x == $t;
	$x > 0 ?? $t + 1 !! $t - 1;
}

our Int multi Num::round_up (Num $x) {
	...
}

our Int multi Num::round_down (Num $x) {
	...
}

# Other math functions.
our bool multi Num::is_odd (Num $x) {
        return ?(int($x) % 2);
}

our bool multi Num::is_even (Num $x) {
        return !(int($x) % 2);
}

our Num multi Num::abs (Num $x) {
	$x < 0 ?? -$x !! $x;
}

our Num multi Num::sign (Num $x) {
	return 0 if $x == 0;
	$x > 0 ?? 1 !! -1;
}

