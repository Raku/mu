#!perl6
use v6;

die "*** This example is currently broken -- needs more work.";

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

sub show_me_the_money($s,$e,$n,$d,$m,$o,$r,$y) {

    my $send := construct($s,$e,$n,$d);
    my $more := construct($m,$o,$r,$e);
    my $money := construct($m,$o,$n,$e,$y);

    if ($send + $more == $money) {
        say " send = $send";
        say "+more = $more";
        say "-------------";
        say "money = $money";
    }

}

# basic test case, A + B = AC

my $a = any(0..9) & none(0);
my $b = any(0..9) & none(0);
my $c = any(0..9);
my $ac;

if ($a != $b && $b != $c && $a != $c &&
    ( $a + $b == ($ac = construct($a,$c)) )
   ) {
	say " A = $a";
	say "+B = $b";
	say "-------";
	say "AC =$ac";
}

# more complicated case :)

my $s = any(0..9) & none(0);
my $e = any(0..9);
my $n = any(0..9);
my $d = any(0..9);
my $m = any(0..9) & none(0);
my $o = any(0..9);
my $r = any(0..9);
my $y = any(0..9);


# does this actually make sure they are all different?  I don't follow
# the logic...
if (any($s,$e,$n,$d,$m,$o,$r,$y) == one($s,$e,$n,$d,$m,$o,$r,$y)) {
    show_me_the_money($s,$e,$n,$d,$m,$o,$r,$y);
}

# if you break it into components, you get a much more tractable
# problem, from an optimisation's point of view:

my $c0 = any(0..1);
my $c1 = any(0..1);
my $c2 = any(0..1);
my $c3 = any(0..1);

if (
  (           $c3        == $m ) &&
( ( $s + $m + $c2 ) % 10 == $o ) && ( int( ( $s + $m + $c2 ) / 10 ) == $c3 ) &&
( ( $e + $o + $c1 ) % 10 == $n ) && ( int( ( $e + $o + $c1 ) / 10 ) == $c2 ) &&
( ( $n + $r + $c0 ) % 10 == $e ) && ( int( ( $n + $r + $c0 ) / 10 ) == $c1 ) &&
( ( $d + $e       ) % 10 == $y ) && ( int( ( $d + $e       ) / 10 ) == $c0 ) &&
 ( $m != $s ) && ( $m != $e ) && ( $m != $n ) && ( $m != $d ) &&
 ( $m != $o ) && ( $m != $r ) && ( $m != $y ) &&
 ( $s != $e ) && ( $s != $n ) && ( $s != $d ) && ( $s != $o ) &&
 ( $s != $r ) && ( $s != $y ) &&
 ( $e != $n ) && ( $e != $d ) && ( $e != $o ) && ( $e != $r ) &&
 ( $e != $y ) &&
 ( $n != $d ) && ( $n != $o ) && ( $n != $r ) && ( $n != $y ) &&
 ( $d != $o ) && ( $d != $r ) && ( $d != $y ) &&
 ( $e != $r ) && ( $e != $y ) &&
 ( $r != $y )
) {
    show_me_the_money($s,$e,$n,$d,$m,$o,$r,$y);
}



__END__

-- Here's the equivalent SQL, note that modern databases seem to grok
-- this expression very well and solve the problem in a minute amount
-- of time.  Less than 10ms on my Athlon!  If Perl 6 has a way of
-- writing this sort of thing in the core language, I'll be very
-- impressed...

-- MySQL table setup:

CREATE TABLE Dx ( X INTEGER(1) );
INSERT INTO Dx (X) VALUES (0),(1),(2),(3),(4),(5),(6),(7),(8),(9);
CREATE TABLE Cx ( X INTEGER(1) );
INSERT INTO Cx (X) VALUES (0),(1);

-- Oracle table setup:

CREATE TABLE Cx ( X NUMBER(1));
CREATE TABLE Dx ( X NUMBER(1));
INSERT INTO Cx (X) VALUES (0);
INSERT INTO Cx (X) VALUES (1);
INSERT INTO Dx (SELECT ROWNUM-1 FROM Cx C0, Cx C1, Cx C2,
                Cx C3 WHERE ROWNUM <= 10);

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
