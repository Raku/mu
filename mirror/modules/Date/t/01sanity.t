#!/usr/bin/perl -w

# based on t/01sanity.t from DateTime 0.29

# note: this test does not test creating a date with a Time Zone

use Test;
use Date::Gregorian;

plan 301;

my $dg = Date::Gregorian.new( :year(1870), :month(10), :day(21),
			      :hour(12),   :minute(10),
			      :second(45.000123456),
			    );

is( $dg.year, '1870', "Year accessor, outside of the epoch" );
is( $dg.month, '10',  "Month accessor, outside the epoch" );
is( $dg.day, '21',    "Day accessor, outside the epoch" );
is( $dg.hour, '12',   "Hour accessor, outside the epoch" );
is( $dg.minute, '10', "Minute accessor, outside the epoch" );
is( $dg.second, '45.000123456', "Second accessor, outside the epoch" );
is( $dg.nanosecond, '123456', "nanosecond accessor, outside the epoch" );

my $dt = $dg.clone;

is( $dt.year, '1870', "Year should be identical" );
is( $dt.month, '10',  "Month should be identical" );
is( $dt.day, '21',    "Day should be identical" );
is( $dt.hour, '12',   "Hour should be identical" );
is( $dt.minute, '10', "Minute should be identical" );
is( $dt.second, '45.000123456', "Second should be identical" );
is( $dt.nanosecond, '123456', "nanosecond should be identical" );

ok( !($dt =:= $dg), "clone created different objects" );

# new tests for the Date::Gregorian API

$dg = date( 2000, 1, 1, 12, 12, 12 );

is($dg.year, 2000, "positional date()");
is($dg.month, 1, "positional date()");
is($dg.day, 1, "positional date()");
is($dg.hour, 12, "positional date()");
is($dg.minute, 12, "positional date()");
is($dg.second, 12, "positional date()");

# note: I consider parsing ISO-8601 natively to be a sanity condition
# of a date module, this is why it is in this test.

my @forms = < YY YYYY YYYY-MM YYYYMM YYYY-MM-DD YYYYMMDD
	      YY-MM YY-MM-DD --DD -MM
	      YYYY-DDD YYYYDDD YY-DDD YYDDD
              YYYYWNN YYYYWNND YYWNND
	      YYYY-MM-DDTHH:MN:SS YYYY-MM-DDHH:MN:SS
	      YYYYMMDDHH:MN:SS YYYYMMDDHHMNSS
	      YYYYMMDDHH YYYYMMDDHHMN YYYYMMDDHHMNSSF
	      YY-MMDD YY-MMDDHH YY-MMDDHHMN YY-MMDDHHMNSS
              YY-MMDDHHMNSSF
              THH HH.FFF HH,FFF HH:MN THHMN HH:MN.FFF
              HH:MN,FFF HH:MN:SS THHMNSS HH:MN:SS.FFF
              HH:MN:SS,FFF>,
              "YYYY-MM-DD HH:MN:SS";

for @forms -> $form {

    # pick a random time for each form, then test it parses correctly.
    my ($year, $week, $dow, $doy, $month, $day, $hour, $min, $sec, 
	$frac)
        = (r(100)+1950, r(53)+1, r(7)+1, r(366)+1, r(12)+1, r(31)+1,
           r(24), r(60), r(60), r(1000)/1000);

    my $iso = $form;
    # note: no support for :e perl5 modifier to s//

    #for ( rx:perl5/YYYY/ => { ($year = r(100)+1950).as("%04d") },
    #	  rx:perl5/YY/   => { ( $year = r(100)).as("%02d") },
    #	  rx:perl5/WNN/  => { ( $week = r(53)+1 ).as("W%02d") },
    #	  rx:perl5/MM/   => { ( $month = r(12)+1 ).as("W%02d") },
    #	  rx:perl5/DDD/  => { ( $doy = r($year % 4 ?? 365 : 366)+1 )
    #			      .as("W%03d") },
    #	  rx:perl5/DD/   => { ( $month = r(31)+1 ).as("W%02d") },
    #	  rx:perl5/D/    => { ( $wday = r(7)+1 ).as("W%01d") },
    #	) -> ($rx, $do) {
    #	if ( $iso ~~ $rx ) {
    #	    substr($iso, $/.from, $/.chars) = $rx();
    #	}
    #}

    ($iso ~~ rx:perl5/(YYYY)/) && (substr($iso,$/.from,$/.chars)= sprintf("%04d", $year))
        or
    ($iso ~~ rx:perl5/(YY)/) && (substr($iso,$/.from,$/.chars)= sprintf("%02d", ($year %= 100)))
        or undefine($year);

    $iso ~~ rx:perl5/(WNN)/ && (substr($iso,$/.from,$/.chars)=sprintf("W%02d", $week)) or undefine($week);
    $iso ~~ rx:perl5/(MM)/ && (substr($iso,$/.from,$/.chars)=sprintf("%02d", $month)) or undefine($month);
    $iso ~~ rx:perl5/(DDD)/ && (substr($iso,$/.from,$/.chars)=sprintf("%03d", $doy)) or undefine($doy);
    $iso ~~ rx:perl5/(DD)/ && (substr($iso,$/.from,$/.chars)=sprintf("%02d", $day)) or undefine($day);
    $iso ~~ rx:perl5/(D)/ && (substr($iso,$/.from,$/.chars)=sprintf("%1d", $dow)) or undefine($dow);

    my $seconds = (reduce { $^a * 60 + $^b } $hour, $min, $sec) + $frac;

    # here's a nice little higher order challenge ... reduce the
    # following code to only have one copy of the fractional bits :)
    my $fractional;
    if $iso ~~ rx:perl5/(HH([,\.]?)(F+))/ {
	my $comma = $1;
        my ($from, $chars) = ($/.from, $/.chars);
	$fractional = format_2_n_dp($2.chars, $seconds/3600);
	$min = int((numify($fractional) - $hour)*60);
	$sec = int((numify($fractional) - $hour)*3600 - $min * 60);
	$frac = (numify($fractional) - $hour)*3600 - $min * 60 - $sec;
	$fractional ~~ s:perl5/\./$comma/; #:
        substr($iso, $from, $chars) = $fractional;
    }

    elsif $iso ~~ rx:perl5/(HH)/ {

	substr($iso, $/.from, $/.chars) = sprintf("%02d", $hour);
        $seconds -= $hour * 3600;

        if $iso ~~ rx:perl5/(MN([,\.]?)(F+))/ {
	    my $comma = $1;
            my ($from, $chars) = ($/.from, $/.chars);
	    $fractional = format_2_n_dp($2.chars, $seconds/60);
	    $sec = int( ( numify($fractional) - $min )*60 );
            $frac = (numify($fractional) - $min)*60 - $sec;
	    $fractional ~~ s:perl5/\./$comma/; #:
            substr($iso, $from, $chars) = $fractional;
	}

        elsif $iso ~~ rx:perl5/(MN)/ {
	    substr($iso, $/.from, $/.chars) = sprintf("%02d", $min);
	    $seconds -= $min * 60;
            if $iso ~~ rx:perl5/(SS([,\.]?)(F+))/ {
                my $comma = $1;
		my ($from, $chars) = ($/.from, $/.chars);
		$fractional = format_2_n_dp($2.chars, $seconds);
	        $frac = numify($fractional) - $sec;
	        $fractional ~~ s:perl5/\./$comma/;  #:
                substr($iso, $from, $chars) = $fractional;
            }

            elsif $iso ~~ rx:perl5/(SS)/ {
		substr($iso, $/.from, $/.chars) = sprintf("%02d", $sec);
		undefine($frac);
	    }
	    else { undefine($sec); undefine($frac) }
	}
	else { undefine($min); undefine($sec); undefine($frac); }
    }
    else { undefine($hour); undefine($min); undefine($sec); undefine($frac) }

    # ok!  now, test that we can parse what we've got...
    if ($frac.defined) { $frac = int(1000 * $frac) }
    diag("$form became $iso - expecting ($year-$month-$day T $hour:$min:$sec.$frac / W $week $dow / D $doy)");
    my $date = eval { date($iso) };
    if ( $! ) {
        fail("exception parsing ISO form $form (used: $iso)");
	skip (4 + ($doy ?? 1 :: 0) + ($week ?? 1 :: 0)
		+ ( ($doy||$week) ?? 0 :: 2 )), "failed to parse";
    } else {
        if ( $year > 100 ) {
	    is($date.year, $year, "$form - year");
	} else {
	    is($date.yy, $year, "$form - yy");
	}
 	if $doy {
	    $doy = 365 if $doy == 366 and $year % 4;
            is($date.doy, $doy, "$form - doy")
	}
 	if $week {
	    # FIXME - figure out which years are "short" in ISO weeks
	    $week-- while ($week > 52 and $date.week and $date.week < $week);
            is($date.week, $week, "$form - week") if $week;
	}
        is($date.month, $month, "$form - month") unless $week or $doy;
        is($date.day, $day, "$form - day") unless $week or $doy;

        is($date.hour, $hour, "$form - hour");
        is($date.minute, $min, "$form - minute");
        is(int($date.second), $sec, "$form - sec");
        is($date.millisecond, $frac, "$form - millisecond");
    }
}

sub r { int(rand($^r)) }
sub format_2_n_dp (Int $dp, Num $frac) returns Str {
    sprintf("%02d", int($frac)) ~ "." ~ sprintf('%0'~"{$dp}d", ($frac - int($frac)) * 10**$dp);
}
sub numify (Str $str) returns Num {
    $str ~~ rx:perl5/^0*([1-9].*)/;
    my $num = +($0);
    #say "numified $str to $num";
    return $num;
}

#sub horner($base, @numerals) { reduce { $^a * $base + $^b } @numerals }
#our &horner60 ::= &horner.assuming(:base(60));
#sub divmod(Num $a, Num $b) returns (Int, Num) {
#    my $ratio = $a / $b;
#    my $dividend = int($ratio);
#    my $remainder = $ratio - $dividend;
#    return ($dividend, $remainder);
#}

# and now, it's time for some implementation...
