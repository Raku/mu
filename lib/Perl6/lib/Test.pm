module Test-0.0.1;
use v6;

my $loop = 0;
my $plan = 0;
my $failed = 0;

sub plan (Int $number_of_tests) returns Int is export {
    $plan = $number_of_tests;
    say "1.." ~ $number_of_tests;
    return $number_of_tests;
}

sub proclaim (Bool $cond, Str ?$desc, Str ?$context) returns Bool {
    my $ok := $cond ?? "ok " :: "not ok ";
	
    my $out := defined($desc) ?? (" - " ~ $desc) :: "";
    my $context_out := defined($context) ?? ( " # " ~ $context) :: "";
    
    $loop++;
    say $ok, $loop, $out, $context_out;
    return $cond;
}

sub ok (Bool $cond, Str ?$desc) returns Bool is export {
    proclaim($cond, $desc);
    if (!$cond) {
        $*ERR.say("#     Failed test (", $?CALLER::POSITION, ")");
        $failed++;
    }
    return $cond;
}

sub is (Str $got, Str $expected, Str ?$desc) returns Bool is export {
    my $test := $got eq $expected; 
    proclaim($test, $desc);
    if (!$test) {
        $*ERR.say("#     Failed test (", $?CALLER::POSITION, ")");
        $*ERR.say("#          got: '", $got, "'");
        $*ERR.say("#     expected: '", $expected, "'");
        $failed++;
    }
    return $test;
}

sub todo_ok (Bool $cond, Str ?$desc) returns Bool is export {
    proclaim($cond, $desc, "TODO");
    if (!$cond) {
        say("#     Failed (TODO) test (", $?CALLER::POSITION, ")");
    }
    return $cond;
}

sub todo_is (Str $got, Str $expected, Str ?$desc) returns Bool is export {
    my $test = $got eq $expected;
    proclaim($test, $desc, "TODO");
    if (!$test) {
        say("#     Failed (TODO) test (", $?CALLER::POSITION, ")");
        say("#          got: '", $got, "'");
        say("#     expected: '", $expected, "'");
    }
    return $test;
}

sub skip (Str ?$reason) returns Bool is export {
    proclaim(1, "", 'skip ' ~ $reason);
    return 1;
}

sub pass (Str ?$desc) returns Bool is export {
    proclaim(1, $desc);
    return 1;
}

sub fail (Str ?$desc) returns Bool is export {
    proclaim(0, $desc);
    say("#     Failed test (", $?CALLER::POSITION, ")");
    return 0;
}

sub todo_fail (Str ?$desc) returns Bool is export {
    proclaim(0, $desc, 'TODO');
    say("#     Failed (TODO) test (", $?CALLER::POSITION, ")");
    return 0;
}

sub diag (Str $diag) {
    say "# ", $diag;
}


END {
    if (!$plan) {
        say("1..", $loop);
    } else {
        if ($plan != $loop) {
	    $*ERR.say("# Looks like you planned ", $plan, " tests, but ran ",
	        $loop);
        }
    }
    if ($failed) {
        $*ERR.say("# Looks like you failed ", $failed, " tests of ", $loop);
    }
}
