module Test-0.0.1;
use v6;

my $loop = 0;
my $failed = 0;

sub ok (Bool $cond, Str ?$desc) returns Bool is export {
    my $ok  := $cond ?? "ok " :: "not ok ";
    my $out := defined($desc) ?? (" - " ~ $desc) :: "";
    $loop++;
    say $ok, $loop, $out;
    if ($ok ne "ok ") {
        $*ERR.say("#     Failed test (", $?CALLER::POSITION, ")");
        $failed++;
    }
}

sub is (Str $got, Str $expected, Str ?$desc) is export {
    my $ok  := ($got eq $expected) ?? "ok " :: "not ok ";
    my $out := defined($desc) ?? (" - " ~ $desc) :: "";
    $loop++;
    say $ok, $loop, $out;
    if ($ok ne "ok ") {
        $*ERR.say("#     Failed test (", $?CALLER::POSITION, ")");
        $*ERR.say("#          got: '", $got, "'");
        $*ERR.say("#     expected: '", $expected, "'");
        $failed++;
    }
}

sub todo_ok (Bool $cond, Str ?$desc) returns Bool is export {
    my $ok  := $cond ?? "ok " :: "not ok ";
    my $out := defined($desc) ?? (" - " ~ $desc) :: "";
    $loop++;
    say $ok, $loop, $out, " # TODO";
    if ($ok ne "ok ") {
        say("#     Failed (TODO) test (", $?CALLER::POSITION, ")");
    }
}

sub todo_is (Str $got, Str $expected, Str ?$desc) is export {
    my $ok  := ($got eq $expected) ?? "ok " :: "not ok ";
    my $out := defined($desc) ?? (" - " ~ $desc) :: "";
    $loop++;
    say $ok, $loop, $out, " # TODO";
    if ($ok ne "ok ") {
        say("#     Failed (TODO) test (", $?CALLER::POSITION, ")");
        say("#          got: '", $got, "'");
        say("#     expected: '", $expected, "'");
        $failed++;
    }
}

END {
    say("1..", $loop);
    if ($failed) {
        $*ERR.say("# Looks like you failed ", $failed, " tests of ", $loop);
    }
}
