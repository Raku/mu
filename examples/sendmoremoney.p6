#!perl6
use v6;

die "*** This example is currently broken -- needs more work.";

# basic test case, A + B = AC
my $a = any(0..9) & none(0);
my $b = any(0..9) & none(0);
my $c = any(0..9);
my $ac;

if ( any($a, $b, $c) == one($a, $b, $c) ) {

    $ac = $a * 10 + $c;

    if ( $a + $b == $ac ) {
	say " A = $a";
	say "+B = $b";
	say "-------";
	say "AC =$ac";
    }
}

# a more complicated and "classical" case :)
sub show_me_the_money($s,$e,$n,$d,$m,$o,$r,$y) {

    my $send := construct($s,$e,$n,$d);
    my $more := construct($m,$o,$r,$e);
    my $money := construct($m,$o,$n,$e,$y);

    if ($send + $more == $money) {
        say " send =  $send";
        say "+more =  $more";
        say "-------------";
        say "money = $money";
    }

}

my $s = any(0..9) & none(0);
my $e = any(0..9);
my $n = any(0..9);
my $d = any(0..9);
my $m = any(0..9) & none(0);
my $o = any(0..9);
my $r = any(0..9);
my $y = any(0..9);

# breaking the calculation down into a series of additions, in terms
# of individual digits and carry values is a necessary step to solving
# this problem quickly (as I see it).

# the test starting with ($c3 == $m), etc is essentially a big
# optimisation hint, roughly organised in a way that represents a
# "quick" way to solve the problem.  By the time the test in
# show_me_the_money runs, the values have already been determined.

# Languages like Oz might be able to figure out this stuff
# automatically, but that is a very lofty goal to aim for to begin
# with!   (see http://xrl.us/fehh (Link to www.mozart-oz.org))

# set this to 1 to disable the hint, instead using an exhaustive
# search method, requiring ~1e8 iterations, until the optimiser is
# smart enough to reduce it to the broken down version
my $use_exhaustive = 0;

my $c0 = any(0..1);
my $c1 = any(0..1);
my $c2 = any(0..1);
my $c3 = any(0..1);

if ( any($s,$e,$n,$d,$m,$o,$r,$y) == one($s,$e,$n,$d,$m,$o,$r,$y) ) {

    if ( $use_exhaustive or
	 (           $c3     == $m ) &&
	 (( $s+$m+$c2 ) % 10 == $o ) && ( int( ( $s+$m+$c2 ) / 10 ) == $c3 ) &&
	 (( $e+$o+$c1 ) % 10 == $n ) && ( int( ( $e+$o+$c1 ) / 10 ) == $c2 ) &&
	 (( $n+$r+$c0 ) % 10 == $e ) && ( int( ( $n+$r+$c0 ) / 10 ) == $c1 ) &&
	 (( $d+$e     ) % 10 == $y ) && ( int( ( $d+$e     ) / 10 ) == $c0 ) &&
       ) {

	show_me_the_money($s,$e,$n,$d,$m,$o,$r,$y);
    }
}

# functions used by example
sub foldl (Code &op, Any $initial, *@values) returns Any {
    if (+@values == 0) {
         return $initial;
    } else {
         return &op(shift @values, &?SUB(&op, $initial, @values));
    }
}

sub add (Int $x, Int $y) returns Int {
    return $x + $y;
}

sub construct (*@values) returns Junction {
    return foldl( -> $x, $y { $x * 10 + $y}, 0, @values);
}


__END__

-- Heres the equivalent SQL, as junctions have direct representation
-- in Set Theory (see http://xrl.us/feh8)), then the below should be a
-- very relevant way to express the problem, especially given
-- MySQL/InnoDB (for one) already has the relevant logic to solve this
-- problem in ~130ms on a 300MHz PII.  Although of course that is
-- still over 39,000,000 cycles!  :-)  Oracle took longer - ~6s, but
-- admittedly it was only running on a dual processor 1.6GHz Opteron.
-- Pg took a similar amount of time on a 1.7GHz AthlonXP (5s), SQLite
-- took over 30s!

-- MySQL table setup (adjust for your DBMS as necessary)

CREATE TABLE Dx ( X INTEGER(1) );
INSERT INTO Dx (X) VALUES (0),(1),(2),(3),(4),(5),(6),(7),(8),(9);
CREATE TABLE Cx ( X INTEGER(1) );
INSERT INTO Cx (X) VALUES (0),(1);

-- actual query:

SELECT
    (S.X * 1000 + E.X * 100 + N.X * 10 + D.X) as SEND,
    (M.X * 1000 + Oh.X * 100 + R.X * 10 + E.X) as MORE,
    (M.X * 10000 + Oh.X * 1000 + N.X * 100 + E.X * 10 + Y.X) as MONEY
from
    Dx M
    LEFT JOIN Dx S ON (M.X != S.X)
    LEFT JOIN Dx E ON ( (M.X != E.X) AND (S.X != E.X) )
    LEFT JOIN Dx Oh ON ( (M.X !=Oh.X) AND (S.X !=Oh.X) AND (E.X !=Oh.X) )
    LEFT JOIN Dx N ON ( (M.X != N.X) AND (S.X != N.X) AND (E.X != N.X)
            AND (Oh.X != N.X) )
    LEFT JOIN Dx R ON ( (M.X != R.X) AND (S.X != R.X) AND (E.X != R.X)
            AND (Oh.X != R.X) AND (N.X != R.X) )
    LEFT JOIN Dx D ON ( (M.X != D.X) AND (S.X != D.X) AND (E.X != D.X)
            AND (Oh.X != D.X) AND (N.X != D.X) AND (R.X != D.X) )
    LEFT JOIN Dx Y ON ( (M.X != Y.X) AND (S.X != Y.X) AND (E.X != Y.X)
            AND (Oh.X != Y.X) AND (N.X != Y.X) AND (R.X != Y.X)
            AND (D.X != Y.X) )
    LEFT JOIN Cx C0 ON ( C0.X = floor( (D.X + E.X) / 10 ) )
    LEFT JOIN Cx C1 ON ( C1.X = floor( (N.X + R.X + C0.X) / 10 ) )
    LEFT JOIN Cx C2 ON ( C2.X = floor( (E.X + Oh.X + C1.X) / 10 ) )
    LEFT JOIN Cx C3 ON ( C3.X = floor( (S.X + M.X + C2.X) / 10 ) )
WHERE
    ( M.X != 0 ) AND
    ( S.X != 0 ) AND
    (                C3.X    ) = M.X  AND
    MOD( S.X + M.X + C2.X, 10) = Oh.X  AND
    MOD( E.X +Oh.X + C1.X, 10) = N.X  AND
    MOD( N.X + R.X + C0.X, 10) = E.X   AND
    MOD( D.X + E.X       , 10) = Y.X
;
