#!/usr/bin/pugs
use v6;

my $timer;
sub start_timer() {
    $timer = time();
}
my $piggy_bank;

sub show_rate($num) {
    my $elapsed = time() - $timer;
    if ($elapsed) {
        say "Found all solutions in "~$elapsed~"s ("~($num/$elapsed)~" comb/s)";
    } else {
        say "Damn, that was fast.  Didn't even see the clock tick,";
        say "for searching a solution space of $num!";
    }
    if (defined($piggy_bank)) {
	say "There are $piggy_bank coins left in the bank.";
    }
}

# basic test case, A + B = AC
my $a = any(1..9);
my $b = any(1..9);
my $c = any(0..9);

sub do_it($a,$b,$c) {
    if ( any($a, $b, $c) == one($a, $b, $c) ) {
        my $ac = $a * 10 + $c;
        if ( $a + $b == $ac ) {
	    say "  A =  $a";
	    say "+ B =  $b";
	    say "--------";
	    say " AC = $ac";
        }
    }
}

say "Finding solutions for A + B = AC";
start_timer();
do_it($a,$b,$c);
show_rate(810);

# a more complicated and "classical" case :)
sub show_me_the_money($s,$e,$n,$d,$m,$o,$r,$y) {

    if (all($s,$e,$n,$d,$m,$o,$r,$y) == one($s,$e,$n,$d,$m,$o,$r,$y)) {

        $piggy_bank --;

        my $send = ((($s)*10+$e)*10+$n)*10+$d;
        my $more = ((($m)*10+$o)*10+$r)*10+$e;
        my $money = (((($m)*10+$o)*10+$n)*10+$e)*10+$y;

        if ($send + $more == $money) {
            say " send =  $send";
            say "+more =  $more";
            say "-------------";
            say "money = $money";
	}
    }
}

my $s = any(1..9);
my $e = any(0..9);
my $n = any(0..9);
my $d = any(0..9);
my $m = any(1..9);
my $o = any(0..9);
my $r = any(0..9);
my $y = any(0..9);

my $c0 = any(0..1);
my $c1 = any(0..1);
my $c2 = any(0..1);
my $c3 = any(0..1);

sub collapse($one, $sub) { $sub.($one) };
sub collapse($one, $two, $sub) { $sub.($one, $two) };
sub collapse($one, $two, $three, $sub) { $sub.($one, $two, $three) };

say "Finding solutions for SEND + MORE = MONEY (psuedo-optimised)";
$piggy_bank = 1e8;
start_timer();

# this is still really ugly, but fast-ish :)
collapse($c3, $m, -> $c3, $m {

    $piggy_bank--;

    # FIFTH (most significant) column of addition
    if ($c3 == $m) {
        say "found c3 = $c3, m = $m";
    collapse($s, $o, $c2, -> $s, $o, $c2 {

        # FOURTH column of addition
        $piggy_bank--;
	if ($s != $m && $s != $o && $m != $o &&
	    (( $s+$m+$c2 ) % 10 == $o ) && ( int( ( $s+$m+$c2 ) / 10 ) == $c3 ) ) {
    	say " found s = $s, o = $o, c2 = $c2";
    	collapse($e, $c1, -> $e, $c1 {

            $piggy_bank--;
	    if ( $e != $s && $e != $o && $e != $m &&
	          ( int( ( $e+$o+$c1 ) / 10 ) == $c2 ) ) {
    	    say "  found e = $e, c1 = $c1";
    	    collapse($d, $y, $c0, -> $d, $y, $c0 {

                $piggy_bank--;
		if ($d != $s && $d != $m && $d != $s && $d != $e && $d != $o &&
		    $y != $s && $y != $m && $y != $s && $y != $e && $y != $o &&
		    $d != $y &&
	     	    (( $d+$e     ) % 10 == $y ) &&
		    ( int( ( $d+$e     ) / 10 ) == $c0 )) {
    	        say "   found d = $d, y = $y, c0 = $c0";
		
		collapse($n, $r, -> $n, $r {

                    $piggy_bank--;
		    if ($n != $s && $n != $m && $n != $s && $n != $o &&
		        $n != $e && $n != $d && $n != $y &&
		        $r != $s && $r != $m && $r != $s && $r != $o &&
		        $r != $e && $r != $d && $r != $y && $r != $n &&
	         	(( $e+$o+$c1 ) % 10 == $n ) &&
	     		( ( $n+$r+$c0 ) % 10 == $e ) &&
			( int( ( $n+$r+$c0 ) / 10 ) == $c1 )) {

    	        	say "    found n = $n, r = $r (that should be it)";

			show_me_the_money($s,$e,$n,$d,$m,$o,$r,$y);	

		    }
		});
		}
	    });
	    }
        });
	}
    });
    }
});

show_rate(16e8);

say "Finding solutions for SEND + MORE = MONEY (exhaustive)";

# in fact, cheat again :-)
# note: this would take BLEEDING AGES (days or weeks) without this
# cheating.
my $s = any(8..9);
my $e = any(5..6);
my $n = any(4..7);
my $d = any(6..8);
my $m = any(1..2);
my $o = any(0,1,9);
my $r = any(7..9);
my $y = any(1..3);

start_timer();
show_me_the_money($s,$e,$n,$d,$m,$o,$r,$y);
show_rate(2*2*4*3*2*3*3*3);

=kwid

-- Heres the equivalent SQL, as junctions have direct representation
-- in Set Theory (see http://xrl.us/feh8)), then the below should be a
-- very relevant way to express the problem, especially given
-- MySQL/InnoDB (for one) already has the relevant logic to solve this
-- problem in ~130ms on a 300MHz PII.  Although of course that is
-- still over 39,000,000 cycles!  :-)  Oracle took longer - ~6s, but
-- admittedly it was only running on a dual processor 1.6GHz Opteron.
-- Pg took a similar amount of time on a 1.7GHz AthlonXP (5s), SQLite
-- took over 30s!

-- The `exhaustive' solution cranks through possibilities at about 

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

=cut
