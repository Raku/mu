module PugsTest-0.0.1;

use v6;

my $loop;

sub ok ($cond, Str $descr) is export {
    $loop++;

    if ($cond) {
	say("ok ", $loop, " # ", $descr, " (", $cond, ")");
    }
    else {
	say("not ok ", $loop, " # TODO ", $descr);
    }
}

sub done () {
    say "1..", $loop;
}
