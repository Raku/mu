module Test-0.0.3;
use v6;

my $loop = 0;
my $plan;
my $failed = 0;
my $log_file = %ENV{'TEST_LOG_FILE'};
my $always_caller = %ENV{'TEST_ALWAYS_CALLER'};

# If @forcetodo_tests[$testnum] is true, test $testnum is always todo_ed. This
# is to ease release preparation.
# We fill this array with actual data at the end of this file.
my @forcetodo_tests;
# Example:
# Test 17 of t/foo/bar.t is:
#   ok some_sub(), "some_sub worked";
# If we want to forcetodo that test, we add a line to t/force_todo:
#   t/foo/bar.t 17
# Now, Test.pm will treat that test 17 of t/foo/bar.t as you'd have written:
#   todo_ok some_sub(), "some_sub worked";

sub plan (Int $number_of_tests) returns Int is export {
    $plan = $number_of_tests;
    say "1..$number_of_tests";
    return $number_of_tests;
}

## ok

sub ok (Bool $cond, Str ?$desc) returns Bool is export {
    proclaim($cond, $desc, undef);
}

sub todo_ok (Bool $cond, Str ?$desc) returns Bool is export {
    proclaim($cond, $desc, "TODO");
}

## is

sub is (Str $got, Str $expected, Str ?$desc) returns Bool is export {
    my $test := $got eq $expected;
    proclaim($test, $desc, undef, $got, $expected);
}

sub todo_is (Str $got, Str $expected, Str ?$desc) returns Bool is export {
    my $test = $got eq $expected;
    proclaim($test, $desc, "TODO", $got, $expected);
}

## like

sub like (Str $got, Rule $expected, Str ?$desc) returns Bool is export {
    my $test = $got ~~ $expected;
    proclaim($test, $desc, undef, $got, $expected);
}

sub todo_like (Str $got, Rule $expected, Str ?$desc) returns Bool is export {
    my $test = $got ~~ $expected;
    proclaim($test, $desc, "TODO", $got, $expected);
}

## eval_ok

sub eval_ok (Str $code, Str ?$desc) returns Bool is export {
    my $result = eval $code;
    if ($!) {
	proclaim(undef, $desc, undef, "eval was fatal");
    } else {
	&ok.goto($result, $desc);
    }
}

sub todo_eval_ok (Str $code, Str ?$desc) returns Bool is export {
    my $result = eval $code;
    if ($!) {
	proclaim(undef, $desc, "TODO", "eval was fatal");
    } else {
	&todo_ok.goto($result, $desc);
    }
}

## eval_is

sub eval_is (Str $code, $expected, Str ?$desc) returns Bool is export {
    my $result = eval $code;
    if ($!) {
	proclaim(undef, $desc, undef, "eval was fatal", $expected);
    } else {
	&is.goto($result, $expected, $desc);
    }
}

sub todo_eval_is (Str $code, $expected, Str ?$desc) returns Bool is export {
    my $result = eval $code;
    if ($!) {
	proclaim(undef, $desc, "TODO", "was fatal", $expected);
    } else {
	&todo_is.goto($result, $expected, $desc);
    }
}

## cmp_ok

sub cmp_ok (Str $got, Code $compare_func, Str $expected, Str ?$desc) returns Bool is export {
    my $test := $compare_func($got, $expected);
    proclaim($test, $desc, undef); # << needs better error message handling
}

sub todo_cmp_ok (Str $got, Code $compare_func, Str $expected, Str ?$desc) returns Bool is export {
    my $test := $compare_func($got, $expected);
    proclaim($test, $desc, "TODO", 4, 5); # << needs better error message handling
}

## isa_ok

sub isa_ok ($ref, Str $expected_type, Str ?$desc) returns Bool is export {
    my $ref_type = ref($ref);
    my $out := defined($desc) ?? $desc :: "The object is-a '$expected_type'";
    my $test := $ref_type eq $expected_type;
    proclaim($test, $out,  undef, $ref_type, $expected_type);
}

sub todo_isa_ok ($ref, Str $expected_type, Str ?$desc) returns Bool is export {
    my $ref_type = ref($ref);
    my $out := defined($desc) ?? $desc :: "The object is-a '$expected_type'";
    my $test := $ref_type eq $expected_type;
    proclaim($test, $out, "TODO", $ref_type, $expected_type);
}

## use_ok

sub use_ok (Str $module) is export {
    eval "require $module";
    if ($!) {
	proclaim(undef, "require $module;", undef, "Import error when loading $module: $!");
    } else {
        &ok.goto(1, "$module imported OK");
    }
}

sub todo_use_ok (Str $module) is export {
    eval "require $module";
    if ($!) {
	proclaim(undef, "require $module;", undef, "Import error when loading $module: $!");
    } else {
        &todo_ok.goto(1, "$module imported OK");
    }
}

## misc. test utilities

sub skip (Str ?$reason) returns Bool is export {
    proclaim(1, "", "skip $reason");
}

sub skip (Int $count, Str $reason) returns Bool is export {
  for (1..$count) {
    skip $reason;
  };
};

sub pass (Str ?$desc) returns Bool is export {
    proclaim(1, $desc);
}

sub fail (Str ?$desc) returns Bool is export {
    proclaim(0, $desc);
}

sub todo_fail (Str ?$desc) returns Bool is export {
    proclaim(0, $desc, 'TODO');
}

sub test_log_file (Str $filename) returns Str is export {
    $log_file = $filename;
    return $log_file;
}

sub diag (Str $diag) is export {
    for (split("\n", $diag)) -> $line {
	say "# $line";
    }
}

## 'private' subs

sub proclaim (Bool $cond, Str ?$desc, Str ?$c, Str ?$got, Str ?$expected) returns Bool {
    my $context = $c; # no C<is rw> yet
    $loop++;

    # Check if we have to forcetodo this test because we're preparing for a
    # release.
    $context = "TODO" if @forcetodo_tests[$loop];

    my $ok := $cond ?? "ok " :: "not ok ";
    my $out = defined($desc) ?? " - $desc" :: "";
       $out = "$out <pos:$?CALLER::CALLER::POSITION>" if $always_caller;

    my $context_out = defined($context) ?? " # $context" :: "";

    say $ok, $loop, $out, $context_out;

    report_failure($context, $got, $expected) if (!$cond);
    write_log(got => $got, expected => $expected, desc => $desc, context => $context) if $log_file and !$cond;

    return $cond;
}

sub report_failure (Str ?$todo, Str ?$got, Str ?$expected) returns Bool {
    if ($todo) {
	diag("  Failed ($todo) test ($?CALLER::CALLER::CALLER::POSITION)");
    } else {
	diag("  Failed test ($?CALLER::CALLER::CALLER::POSITION)");
        $failed++;
    }

    if ($?CALLER::CALLER::SUBNAME eq ('&is' | '&todo_is' | '&cmp_ok' | '&todo_cmp_ok' | '&eval_is' | '&todo_eval_is' | '&isa_ok' | '&todo_isa_ok')) {
        diag("  Expected: " ~ ($expected.defined ?? $expected :: "undef"));
        diag("       Got: " ~ ($got.defined ?? $got :: "undef"));
    } else {
        diag("       Got: " ~ ($got.defined ?? $got :: "undef"));
    }
}

sub write_log (+$got, +$expected, Str +$desc, Str +$errstr, Str +$context, Str +$operator = 'eq') returns Bool {
    # return 0 but true unless $log_file; # not yet implemented
    return 1 unless $log_file;
    # until we have 'given'/'when'
    my $status = 'FAILED';
    if (index($?CALLER::CALLER::SUBNAME, 'todo') >= 0) {
        $status = 'TODO';
    }
    if (index($?CALLER::CALLER::SUBNAME, 'skip') >= 0) {
        $status = 'SKIPPED';
    }
    my $out;
    if ($out = open(">>$log_file")) {
        $out.say $?CALLER::CALLER::CALLER::FILE ~ " $loop $status";
        $out.say $desc if $desc;
        $out.say $errstr if $errstr;
        $out.say $context if $context;
        $out.say '### Expected ###';
        $out.say $expected;
        $out.say '### Actual Results ###';
        $out.say $got, "\n";
        $out.close;
        return 1;
    }
    return 0;
}

sub read_forcetodo_tests() {
    # In the file describing which tests to "force todo", we only use Unix
    # filenames. So, we (may) have to convert our win32 filename:
    my $unixfn = $?FILE;
    $unixfn ~~ s:perl5:g{\\}{/};

    my $force_todo_fh = open "< t/force_todo";
    # If the file doesn't exist, simply return -- there's nothing we could
    # read from that file.
    return() unless $force_todo_fh;

    # Otherwise, continue:
    my @tests_to_forcetodo;
    for =$force_todo_fh -> $l {
	my $line = $l; # no C<is rw> yet
	# FYI, an example line might look like:
	#   t/foo/bar.t 13 15 42
	# If $line is not a comment and concerns us...
	if(substr($line, 0, 1) ne "#" and index($line, $unixfn) >= 0) {
	    chomp $line;
	    my @tests = split " ", $line;
	    # We have to shift @tests to remove the test filename.
	    shift @tests;
	    push @tests_to_forcetodo, @tests;
	}
    }

    if(@tests_to_forcetodo) {
	@forcetodo_tests[$_] = 1 for @tests_to_forcetodo;
	diag "Will forcetodo test(s) @tests_to_forcetodo[]."
    }
}
read_forcetodo_tests();

END {
    if (!defined($plan)) {
        say("1..$loop");
    } elsif ($plan != $loop) {
	$*ERR.say("# Looks like you planned $plan tests, but ran $loop");
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

  plan 10;
  test_log_file('test.log');

  use_ok('Some::Module');
  todo_use_ok('Some::Other::Module');

  ok(2 + 2 == 4, '2 and 2 make 4');
  is(2 + 2, 4, '2 and 2 make 4');
  isa_ok([1, 2, 3], 'List');

  todo_ok(2 + 2 == 5, '2 and 2 make 5');
  todo_is(2 + 2, 5, '2 and 2 make 5');
  todo_isa_ok({'one' => 1}, 'Hash');

  use_ok('My::Module');

  pass('This test passed');
  fail('This test failed');

  skip('skip this test for now');

  todo_fail('this fails, but might work soon');

  diag('some misc comments and documentation');

= DESCRIPTION

This module was built to facilitate the Pugs test suite. It has the
distinction of being the very first module written for Pugs.

It provides a simple set of common test utility functions, and is
an implementation of the TAP protocol.

This module, like Pugs, is a work in progress. As new features are
added to Pugs, new test functions will be defined to facilitate the
testing of those features. For more information see the FUTURE PLANS
section of this document.

= FUNCTIONS

- `plan (Int $number_of_tests) returns Int`

All tests need a plan. A plan is simply the number of tests which are
expected to run. This should be specified at the very top of your tests.

- `test_log_file (Str $filename) returns Str`

If you specify a log file, any failed tests will log some diagnostics
there.  The filename 'test.log' is recommended.

== Testing Functions

- `use_ok (Str $module) returns Bool`

*NOTE:* This function currently uses `require()` since Pugs does not yet have
a proper `use()` builtin.

- `ok (Bool $cond, Str ?$desc) returns Bool`

- `is (Str $got, Str $expected, Str ?$desc) returns Bool`

- `like (Str $got, Rule $expected, Str ?$desc) returns Bool is export`

This function should work with most reg-exps, but given that they are still a
somewhat experimental feature in Pugs, it is suggested you don't try anything
too funky.

- `cmp_ok (Str $got, Code $compare_func, Str $expected, Str ?$desc) returns Bool`

This function will compare `$got` and `$expected` using `$compare_func`. This will
eventually allow Test::More-style cmp_ok() though the following syntax:

  cmp_ok('test', &infix:<gt>, 'me', '... testing gt on two strings');

However the `&infix:<gt>` is currently not implemented, so you will have to wait
a little while. Until then, you can just write your own functions like this:

  cmp_ok('test', sub ($a, $b) { ?($a gt $b) }, 'me', '... testing gt on two strings');

- `isa_ok ($ref, Str $expected_type, Str ?$desc) returns Bool`

This function currently on checks with ref() since we do not yet have
object support. Once object support is created, we will add it here, and
maintain backwards compatibility as well.

- `eval_ok (Str $code, Str ?$desc) returns Bool`

- `eval_is (Str $code, $expected, Str ?$desc) returns Bool`

These functions will eval a code snippet, and then pass the result to is or ok
on success, or report that the eval was not successful on failure.

== TODO Testing functions

Sometimes a test is broken because something is not implemented yet. So
in order to still allow that to be tested, and those tests to knowingly
fail, we provide a set of todo_* functions for all the basic test
functions.

- `todo_use_ok(Str $module) returns Bool`

- `todo_ok (Bool $cond, Str ?$desc) returns Bool`

- `todo_is (Str $got, Str $expected, Str ?$desc) returns Bool`

- `todo_like (Str $got, Rule $expected, Str ?$desc) returns Bool is export`

- `todo_cmp_ok (Str $got, Code $compare_func, Str $expected, Str ?$desc) returns Bool`

- `todo_isa_ok ($ref, Str $expected_type, Str ?$desc) returns Bool`

- `todo_eval_ok (Str $code, Str ?$desc) returns Bool`

- `todo_eval_is (Str $code, $expected, Str ?$desc) returns Bool`

You can use `t/force_todo` to set the tests which should get a temporary
`todo_`-prefix because of release preparation. See `t/force_todo` for more
information.

== Misc. Functions

- `skip (Str ?$reason) returns Bool`
- `skip (Int $count, Str ?$reason) returns Bool`

If for some reason a test is to be skipped, you can use this
function to do so.

- `pass (Str ?$desc) returns Bool`

Sometimes what you need to test does not fit into one of the standard
testing functions. In that case, you can use the rather blunt pass()
functions and its compliment the fail() function.

- `fail (Str ?$desc) returns Bool`

This is the opposite of pass()

- `todo_fail (Str ?$desc) returns Bool`

On occasion, one of these odd tests might fail, but actually be a TODO
item. So we give you todo_fail() for just such an occasion.

- `diag (Str $diag)`

This will print each string with a '#' character appended to it, this is
ignored by the TAP protocol.

= FUTURE PLANS

This module is still a work in progress. As Pugs grows, so will it's
testing needs. This module will be the code support for those needs. The
following is a list of future features planned for this module.

- better error handling for cmp_ok

The error handling capabilities need to be expanded more to handle the
error reporting needs of the cmp_ok() function.

- is_deeply

Once nested data structures are implemented, we will need an easy way
to test them. So we will implement the Test::More function is_deeply.
The plan currently is to implement this as a mutually recursive multi-
sub which will be able to handle structures of arbitrary depth and of
an arbitrary type. The function signatures will likely look something
like this:

  multi sub is_deeply (Array @got, Array @expected, Str ?$desc) returns Bool;
  multi sub is_deeply (List  $got, List  $expected, Str ?$desc) returns Bool;
  multi sub is_deeply (Hash  %got, Hash  %expected, Str ?$desc) returns Bool;
  multi sub is_deeply (Pair  $got, Pair  $expected, Str ?$desc) returns Bool;

Because these functions will be mutually recursive, they will easily be
able handle arbitrarily complex data structures automatically (at least
that is what I hope).

- throws_ok, lives_ok

These are functions taken directly from Test::Exception. They will
accept a block to execute and then either an Exception type, a reg-exp
or a string to match against the error.

= ENVIRONMENT

Setting the environment variable TEST_LOG_FILE sets the default
filename where test diagnostics should be written.

= SEE ALSO

The Perl 5 Test modules

- Test

- Test::More

Information about the TAP protocol can be found in the Test::Harness
distribution.

= AUTHORS

Aurtrijus Tang <autrijus@autrijus.org>

Benjamin Smith

Norman Nunley

Steve Peters

Stevan Little <stevan@iinteractive.com>

Brian Ingerson <ingy@cpan.org>

Jesse Vincent <jesse@bestpractical.com>

Yuval Kogman <nothingmuch@woobling.org>

Nathan Gray <kolibrie@graystudios.org>

Max Maischein <corion@cpan.org>

Ingo Blechschmidt <iblech@web.de>

= COPYRIGHT

Copyright (c) 2005. Autrijus Tang. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut