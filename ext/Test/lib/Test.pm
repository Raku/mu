module Test-0.0.1;
use v6;

my $loop = 0;
my $plan = 0;
my $failed = 0;

sub plan (Int $number_of_tests) returns Int is export {
    $plan = $number_of_tests;
    say "1..$number_of_tests";
    return $number_of_tests;
}

sub proclaim (Bool $cond, Str ?$desc, Str ?$context) returns Bool {
    my $ok := $cond ?? "ok " :: "not ok ";
    my $out := defined($desc) ?? " - $desc" :: "";
    my $context_out := defined($context) ?? " # $context" :: "";
    
    $loop++;
    say $ok, $loop, $out, $context_out;
    return $cond;
}

sub ok (Bool $cond, Str ?$desc) returns Bool is export {
    proclaim($cond, $desc);
    if (!$cond) {
        $*ERR.say("#     Failed test ($?CALLER::POSITION)");
        $failed++;
    }
    return $cond;
}

sub is (Str $got, Str $expected, Str ?$desc) returns Bool is export {
    my $test := $got eq $expected; 
    proclaim($test, $desc);
    if (!$test) {
        $*ERR.say("#     Failed test ($?CALLER::POSITION)");
        $*ERR.say("#          got: '$got'");
        $*ERR.say("#     expected: '$expected'");
        $failed++;
    }
    return $test;
}

sub todo_ok (Bool $cond, Str ?$desc) returns Bool is export {
    proclaim($cond, $desc, "TODO");
    if (!$cond) {
        say("#     Failed (TODO) test ($?CALLER::POSITION)");
    }
    return $cond;
}

sub todo_is (Str $got, Str $expected, Str ?$desc) returns Bool is export {
    my $test = $got eq $expected;
    proclaim($test, $desc, "TODO");
    if (!$test) {
        say("#     Failed (TODO) test ($?CALLER::POSITION)");
        say("#          got: '$got'");
        say("#     expected: '$expected'");
    }
    return $test;
}

sub skip (Str ?$reason) returns Bool is export {
    proclaim(1, "", "skip $reason");
    return 1;
}

sub pass (Str ?$desc) returns Bool is export {
    proclaim(1, $desc);
    return 1;
}

sub fail (Str ?$desc) returns Bool is export {
    proclaim(0, $desc);
    say("#     Failed test ($?CALLER::POSITION)");
    return 0;
}

sub todo_fail (Str ?$desc) returns Bool is export {
    proclaim(0, $desc, 'TODO');
    say("#     Failed (TODO) test ($?CALLER::POSITION)");
    return 0;
}

sub diag (Str $diag) is export {
    say "# $diag";
}

END {
    if (!$plan) {
        say("1..$loop");
    } else {
        if ($plan != $loop) {
	    $*ERR.say("# Looks like you planned $plan tests, but ran $loop");
        }
    }
    if ($failed) {
        $*ERR.say("# Looks like you failed $failed tests of $loop");
    }
}

=kwid

= NAME

Test - Test support module for perl6

= SYNOPSIS

  use v6;
  require Test;
  
  plan 8;
  
  ok(2 + 2 == 4, '2 and 2 make 4');
  is(2 + 2, 4, '2 and 2 make 4');
  
  todo_ok(2 + 2 == 5, '2 and 2 make 5');
  todo_is(2 + 2, 5, '2 and 2 make 5');  
  
  pass('This test passed');
  fail('This test failed');

  skip('skip this test for now');
  
  todo_fail('this fails, but might work soon');
  
  diag('some misc comments and documentation');

= DESCRIPTION

= FUNCTIONS

* `plan (Int $number_of_tests) returns Int`

* `ok (Bool $cond, Str ?$desc) returns Bool`

* `is (Str $got, Str $expected, Str ?$desc) returns Bool`

* `todo_ok (Bool $cond, Str ?$desc) returns Bool`

* `todo_is (Str $got, Str $expected, Str ?$desc) returns Bool`

* `skip (Str ?$reason) returns Bool`

* `pass (Str ?$desc) returns Bool`

* `fail (Str ?$desc) returns Bool`

* `todo_fail (Str ?$desc) returns Bool`

* `diag (Str $diag)`

= SEE ALSO

= AUTHORS

Aurtrijus Tang <autrijus@autrijus.org>

Benjamin Smith

Norman Nunley

Steve Peters

Stevan Little <stevan@iinteractive.com>

Brian Ingerson <ingy@cpan.org>

= COPYRIGHT

Copyright (c) 2005. Autrijus Tang. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
