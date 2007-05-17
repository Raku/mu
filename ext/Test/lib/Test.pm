
# this is needed by v6.pm (perl6-in-perl5)
use v6-alpha;

module Test-0.0.7;

### CONSTANTS

# some options available through the environment
$Test::ALWAYS_CALLER = %*ENV<TEST_ALWAYS_CALLER>;

### GLOBALS

# globals to keep track of our tests
$Test::num_of_tests_run    = 0;
$Test::num_of_tests_failed = 0;
$Test::num_of_tests_badpass = 0;
$Test::num_of_tests_planned;

$Test::todo_next_test = False;

# a Junction to hold our FORCE_TODO tests.  It's actually a string so
# Test.pm doesn't require back to implement Junction.
$Test::force_todo_test_junction = '';

# for running the test suite multiple times in the same process
$Test::testing_started = 1;

### FUNCTIONS

## plan

sub plan (Int $number_of_tests) returns Void is export {
    $Test::testing_started = 1;
    $Test::num_of_tests_planned = $number_of_tests;
    say "1..$number_of_tests";
}

sub force_todo (*@todo_tests) returns Void is export {
     $Test::force_todo_test_junction = join ' ', '', @todo_tests, '';
}

## ok

sub ok (Bool $cond, Str $desc?, :$todo, :$depends) returns Bool is export {
    Test::proclaim($cond, $desc, $todo, :$depends);
}

## is

sub is (Str $got, Str $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $test := $got eq $expected;
    Test::proclaim($test, $desc, $todo, $got, $expected, $depends);
}

## is_deeply
sub is_deeply(Any $got, Any $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    # hack for now
    my $got_perl = $got.perl;
    my $expected_perl = $expected.perl;
    my $test := ($got_perl eq $expected_perl);
    Test::proclaim($test, $desc, $todo, $got_perl, $expected_perl, $depends);
}


## isnt

sub isnt (Str $got, Str $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $test := not($got eq $expected);
    Test::proclaim($test, "Should not match: $desc", $todo, $got, $expected, $depends, :negate);
}

## like

sub like (Str $got, Rule $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $test := $got ~~ $expected;
    Test::proclaim($test, $desc, $todo, $got, $expected, $depends);
}

## unlike

sub unlike (Str $got, Rule $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $test := not($got ~~ $expected);
    Test::proclaim($test, $desc, $todo, $got, $expected, $depends, :negate);
}

# eval_ok eval_is - removed.

## eval_dies_ok

sub eval_dies_ok (Str $code, Str $desc?, :$todo, :$depends) returns Bool is export {
    eval $code;
    if (defined $!) {
        &Test::ok.nextwith(1, $desc, :$todo);
    }
    else {
        Test::proclaim(undef, $desc, $todo, "No exception thrown", :$depends);
    }
}

## cmp_ok

sub cmp_ok (Str $got, Code &compare_func, Str $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $test := compare_func($got, $expected);
    Test::proclaim($test, $desc, $todo, $got, "&compare_func.name() $expected", $depends);
}

## isa_ok

sub isa_ok (Any|Junction|Pair $ref is rw, Str $expected_type, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $out := defined($desc) ?? $desc !! "The object is-a '$expected_type'";
    my $test := $ref.isa($expected_type);
    Test::proclaim($test, $out, $todo, $ref.WHAT, $expected_type, $depends);
}

## use_ok

sub use_ok (Str $module, :$todo, :$depends) is export {
    my $caller = caller().package;

    eval "package $caller; require $module";

    #try {
    #    &::($module)::import.nextwith();
    #};

    if ($!) {
        Test::proclaim(undef, "require $module;", $todo, "Import error when loading $module: $!", :$depends);
    }
    else {
        &Test::ok.nextwith(1, "$module imported OK", :$todo, :$depends);
    }
}

## throws ok

sub throws_ok (Code &code, Any $match, Str $desc?, :$todo, :$depends) returns Bool is export {
    try &code;
    if ($!) {
        &Test::ok.nextwith($! ~~ $match, $desc, :$todo, :$depends);
    }
    else {
        Test::proclaim(undef, $desc, $todo, "No exception thrown", :$depends);
    }
}

## dies_ok

sub dies_ok (Code &code, Str $desc?, :$todo, :$depends) returns Bool is export {
    try &code;
    if ($!) {
        &Test::ok.nextwith(1, $desc, :$todo);
    }
    else {
        Test::proclaim(undef, $desc, $todo, "No exception thrown", :$depends);
    }
}

## lives ok

sub lives_ok (Code &code, Str $desc?, :$todo, :$depends) returns Bool is export {
    try &code;
    if ($!) {
        Test::proclaim(undef, $desc, $todo, "An exception was thrown : $!", :$depends);
    }
    else {
        &Test::ok.nextwith(1, $desc, :$todo, :$depends);
    }
}

## misc. test utilities

sub version_lt (Str $version1, Str $version2) returns Bool {
    my @num1 = split '.', $version1;
    my @num2 = split '.', $version2;
    #warn ">>> compare $version1 <=> $version2\n";
    for each(@num1; @num2) -> $a, $b {
        next if $a == $b;
        return True if $a < $b;
        return False;
    }
    return False;
}

sub todo (*%deadline) returns Bool is export {
    #warn "!!!", %deadline;
    return if ! $?COMPILER.defined;
    my $spec_ver = %deadline{lc($?COMPILER)};
    if (!$spec_ver.defined or $spec_ver eq '1' or Test::version_lt($?VERSION, $spec_ver)) {
        $Test::todo_next_test = True;
        return True;
    }
    return False;
}

multi sub skip (Str $reason?, :$depends) returns Bool is export {
    Test::proclaim(1, "", "skip $reason", :$depends);
}

multi sub skip (Int $count, Str $reason, :$depends) returns Bool is export {
    for (1 .. $count) {
        # Hack -- PIL2JS doesn't support multisubs correctly yet
        if $*OS eq "browser" {
            Test::proclaim(1, "", "skip $reason", :$depends);
        } else {
            Test::skip $reason, :$depends;
        }
    }
}

sub skip_rest (Str $reason?, :$depends) returns Bool is export {
    Test::skip($Test::num_of_tests_planned - $Test::num_of_tests_run, $reason // "", :$depends);
}

sub pass (Str $desc?) returns Bool is export {
    Test::proclaim(1, $desc);
}

sub flunk (Str $desc?, :$todo, :$depends) returns Bool is export {
    Test::proclaim(0, $desc, $todo, :$depends);
}

sub diag (Str $diag) is export {
    for (split("\n", $diag)) -> $line {
        if ($diag ~~ m:P5/Failed \(TODO.*?\) test/) {
            $*OUT.say("# $line");
        } else {
            $*ERR.say("# $line");   # we need warn to work with prove6
        }
    }
}

## 'private' subs

sub proclaim (Bool $cond, Str $desc? is copy, $todo?, Str $got?, Str $expected?, $depends?, $negate?) returns Bool {
    $Test::testing_started = 1;
    $Test::num_of_tests_run++;

    # $context is now the raw TODO, so we have to check it
    my $context;

    # Check if we have to forcetodo this test
    # because we're preparing for a release.
    $context = "TODO for release"
        if index($Test::force_todo_test_junction, ' '~$Test::num_of_tests_run~' ') >= 0;

    #warn "todo_next_test: $Test::todo_next_test";
    if $Test::todo_next_test {
        $context =  "TODO" ~ ($todo.isa('Str') ?? " $todo" !! '');
        $Test::todo_next_test = False;
    } elsif $todo {
        if (substr($todo, 0, 4) eq 'skip') {
            $context = $todo;
        }
        else {
            $context =  "TODO" ~ ($todo.isa('Str') ?? " $todo" !! '');
            if ( $cond ) {
                $Test::num_of_tests_badpass ++;
            }
        }
    }

    if ( $depends ) {
        $context ~= " (depends on $depends working)";
    }

    my $out = $desc.defined ?? " - $desc" !! "";
    $out = "$out <pos:$?CALLER::CALLER::POSITION>" if $Test::ALWAYS_CALLER;
    # message like "test #1 # TODO" confuse the harness. Escape desc #s.
    $out ~~ s:P5:g/#/\\#/;

    my $context_out = $context.defined ?? " # $context" !! "";

    say(($cond ?? "ok " !! "not ok "), $Test::num_of_tests_run, $out, $context_out);

    Test::report_failure($context, $got, $expected, $negate) unless $cond;

    return $cond;
}

sub report_failure (Str $todo?, Str $got?, Str $expected?, Bool $negate?) returns Bool {
    if ($todo) {
        Test::diag("  Failed ($todo) test ($?CALLER::CALLER::CALLER::POSITION)");
    }
    else {
        Test::diag("  Failed test ($?CALLER::CALLER::CALLER::POSITION)");
        $Test::num_of_tests_failed++;
    }
    my $wanted = $negate ?? "Unwanted" !! "Expected";

    # As PIL2JS doesn't support junctions yet, skip the junction part when
    # running under PIL2JS.
    if (index('&Test::is &Test::isnt &Test::cmp_ok &Test::isa_ok &Test::is_deeply &Test::todo_is &Test::todo_isnt &Test::todo_cmp_ok &Test::todo_eval_is &Test::todo_isa_ok ', &?CALLER::CALLER::ROUTINE.name ~ ' ') >= 0) {
        Test::diag("  $wanted: '" ~ ($expected.defined ?? $expected !! "undef") ~ "'");
        Test::diag("    Actual: '" ~ ($got.defined ?? $got !! "undef") ~ "'");
    }
    else {
        Test::diag("    Actual: " ~ ($got.defined ?? $got !! "undef"));
    }
}


sub test_ends {
    #XXX this fixes an extremely strange bug
    # my $nr = $Test::num_of_tests_run;        #AAA
    return() unless $Test::testing_started;
    if (!defined($Test::num_of_tests_run)) {   #BBB
    # if (!defined($Test::num_of_tests_run)) returned true when running t/operators/precedence.t
    # even though $Test::num_of_tests_run was defined and equal to 49 at the top of this sub
    # the bug only occurs when Test.pm is precompiled
    # for the full story see the IRC conversation beginning at
    # http://moritz.faui2k3.org/irclog/out.pl?channel=perl6;date=2007-05-14#id_l267
    # r16289 of this file is the one that was being tested.
    # my fix was to comment the line marked BBB and uncomment the lines marked AAA
    # -rhr
    # if (!defined($nr)) {                    #AAA
        say("1..$Test::num_of_tests_run");
    }
    elsif ($Test::num_of_tests_planned != $Test::num_of_tests_run) {
        $*ERR.say("# Looks like you planned $Test::num_of_tests_planned tests, but ran $Test::num_of_tests_run");
    }

    if ($Test::num_of_tests_failed) {
        $*ERR.say("# Looks like you failed $Test::num_of_tests_failed tests of $Test::num_of_tests_run");
    }
    if ($Test::num_of_tests_badpass) {
        $*ERR.say("# Looks like $Test::num_of_tests_badpass tests of $Test::num_of_tests_run passed unexpectedly");
    }
    $Test::num_of_tests_run    = 0;
    $Test::num_of_tests_failed = 0;
    $Test::num_of_tests_badpass = 0;
    $Test::num_of_tests_planned = undef;
    $Test::force_todo_test_junction = undef;
    $Test::testing_started = 0;
}

END { Test::test_ends() }

=pod

=head1 NAME

Test - Test support module for perl6

=head1 SYNOPSIS

  use v6-alpha;
  require Test;

  plan 10;
  force_todo(1, 3 .. 5, 9);

  use_ok('Some::Module');
  use_ok('Some::Other::Module', todo => 1);

  ok(2 + 2 == 4, '2 and 2 make 4');
  is(2 + 2, 4, '2 and 2 make 4');
  isa_ok([1, 2, 3], 'List');

  ok(2 + 2 == 5, '2 and 2 make 5', :todo(1));
  is(2 + 2, 5, desc => '2 and 2 make 5', todo => 1);
  isa_ok({'one' => 1}, 'Hash', :todo(1));

  use_ok('My::Module');

  pass('This test passed');
  flunk('This test failed');

  skip('skip this test for now');

  flunk('this fails, but might work soon', :todo(1));

  diag('some misc comments and documentation');

  # TODO the next test with respect to the "deadline" specified.
  todo :pugs<6.2.13>, :foo<1.23>;
  is foo, bar, '...';

=head1 DESCRIPTION

This module was built to facilitate the Pugs test suite. It has the
distinction of being the very first module written for Pugs.

It provides a simple set of common test utility functions, and is
an implementation of the TAP protocol.

This module, like Pugs, is a work in progress. As new features are
added to Pugs, new test functions will be defined to facilitate the
testing of those features. For more information see the FUTURE PLANS
section of this document.

=head1 FUNCTIONS

  plan (Int $number_of_tests) returns Void

All tests need a plan. A plan is simply the number of tests which are
expected to run. This should be specified at the very top of your tests.

  force_todo (*@todo_tests) returns Void

If you have some tests which you would like to force into being TODO tests
then you can pass them through this function. This is primarily a release
tool, but can be useful in other contexts as well.

=head2 Testing Functions

=head3 use_ok

  use_ok (Str $module, Bool :$todo, Str :$depends) returns Bool

=head3 ok

  ok (Bool $cond, Str $desc?, Bool :$todo, Str :$depends) returns Bool

=head3 is

=head3 isnt

  is   (Str $got, Str $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool
  isnt (Str $got, Str $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool

=head3 like

=head3 unlike

  like   (Str $got, Rule $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool is export
  unlike (Str $got, Rule $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool is export

These functions should work with most reg-exps, but given that they are still a
somewhat experimental feature in Pugs, it is suggested you don't try anything
too funky.

=head3 cmp_ok

  cmp_ok (Str $got, Code &compare_func, Str $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool

This function will compare C<$got> and C<$expected> using C<&compare_func>. This will
eventually allow Test::More-style cmp_ok() though the following syntax:

  cmp_ok('test', &infix:<gt>, 'me', '... testing gt on two strings');

However the C<< &infix:<gt> >> is currently not implemented, so you will have to wait
a little while. Until then, you can just write your own functions like this:

  cmp_ok('test', sub ($a, $b) { ?($a gt $b) }, 'me', '... testing gt on two strings');
  isa_ok ($ref, Str $expected_type, Str $desc?, Bool :$todo, Str :$depends) returns Bool

This function currently on checks with WHAT() since we do not yet have
object support. Once object support is created, we will add it here, and
maintain backwards compatibility as well.

=head3 eval_dies_ok

  eval_dies_ok (Str $code, Str $desc?,                Bool :$todo, Str :$depends) returns Bool

This is the function to use if you have a piece of code that would otherwise
failed to be parsed. If the code parses, but may die at run time, consider 
using C<dies_ok> or C<lives_ok>, which have lower overhead. 

C<eval> a string - unsuccessful or failure was expected.

The C<eval> does not occur in the context of the caller.
Non-global lexicals will not be accessible, and the package will be different.

=head3 throws_ok

  throws_ok (Code &code, Any $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool

This function takes a block of code and runs it. It then smart-matches (C<~~>) any C<$!>
value with the C<$expected> value.

=head3 dies_ok

=head3 lives_ok

  dies_ok  (Code &code, Str $desc?, Bool :$todo, Str :$depends) returns Bool
  lives_ok (Code &code, Str $desc?, Bool :$todo, Str :$depends) returns Bool

These functions both take blocks of code, and test whether they live or die using C<try>

The code must at least be parsable. If the code might not parse, wrap it in C<eval>.

=head3 is_deeply

 is_deeply(Any $got, Any $expected, Str $desc?, :$todo, :$depends) returns Bool

Similar to is(), except that if $this and $that are references, it
does a deep comparison walking each data structure to see if they are
equivalent.  

=begin is_deeply_comment

The plan currently is to implement this as a mutually recursive multi-
sub which will be able to handle structures of arbitrary depth and of
an arbitrary type. The function signatures will likely look something
like this:

  multi sub is_deeply (Array @got, Array @expected, Str $desc?) returns Bool;
  multi sub is_deeply (List  $got, List  $expected, Str $desc?) returns Bool;
  multi sub is_deeply (Hash  %got, Hash  %expected, Str $desc?) returns Bool;
  multi sub is_deeply (Pair  $got, Pair  $expected, Str $desc?) returns Bool;

Because these functions will be mutually recursive, they will easily be
able handle arbitrarily complex data structures automatically (at least
that is what I hope).

=end is_deeply_comment

=head2 Misc. Functions

=head3 todo

  todo (*%deadline) returns Bool is export

If and only if the deadline has been hit (or passed), the next one test will
be marked as TODO.

For example:

   todo :pugs<6.28.0>;
   is($got, $expected, $desc);

The call to the C<todo> function will mark the next one test as TODO if
and only if the current compiler's $?COMPILER holds a string whose lowercase
version equals to 'pugs' and $?VERSION holds a value less than '6.28.0'.
The C<todo> functuion will perform partial ordering comparison between version
numbers.

More implementation-specific deadlines can be appended to a single C<todo> call:

  todo :pugs<6.28.0>, :p6p5<0.011>, :parrot<0.45>;

=head3 skip

  skip (Str $reason?) returns Bool
  skip (Int $count, Str $reason?) returns Bool

If for some reason a test is to be skipped, you can use this
function to do so.

=head3 pass

=head3 flunk

  pass (Str $desc?) returns Bool

Sometimes what you need to test does not fit into one of the standard
testing functions. In that case, you can use the rather blunt pass()
functions and its compliment the flunk() function.

  flunk (Str $desc?, Bool :$todo) returns Bool

This is the opposite of pass()

=head3 diag

  diag (Str $diag)

This will print each string with a '#' character appended to it, this is
ignored by the TAP protocol.

=head1 Common options

=head2 :todo

Sometimes a test is broken because something is not implemented yet. So
in order to still allow that to be tested, and those tests to knowingly
fail, we provide the C<todo> function to TODO the next one test according
to a given deadline. (See below.)

Take the Pugs implementation. When TODOing failing tests before
the Pugs release (say, 6.2.12), the following form of todo should be used:

    todo :pugs<6.2.13>;  # version of the next point release

By doing this, temporarily TODO'd tests can get unTODO'd automatically once
the the release is done (and the version number gets updated).

The version number fed to C<todo> is optional. If omitted,
the corresponding tests won't get expired unless we unTODO them manually.

It is also possible to use the C<force_todo()> function to do large scale
TODO-ing of tests.

=head2 :depends

The C<:depends("string")> parameter to most of the functions is a way to
provide more context in the case of a failure.  It should refer to another file
or test which must be made to pass before this test can pass (or before an
implementation could be started).  This is most useful when writing modules and
you find there is some language feature missing, or core bug that needs to be
sorted out before you can continue.

=head1 FUTURE PLANS

This module is still a work in progress. As Pugs grows, so will it's
testing needs. This module will be the code support for those needs. The
following is a list of future features planned for this module.

- better error handling for cmp_ok

The error handling capabilities need to be expanded more to handle the
error reporting needs of the cmp_ok() function.

=head1 ENVIRONMENT

Setting the environment variable TEST_ALWAYS_CALLER to force Test.pm to always
append the caller information to the test's C<$desc>.

=head1 SEE ALSO

The Perl 5 Test modules

- Test

- Test::More

Information about the TAP protocol can be found in the Test::Harness
distribution.

=head1 AUTHORS

Audrey Tang <autrijus@autrijus.org>

Benjamin Smith

Norman Nunley

Steve Peters

Stevan Little <stevan@iinteractive.com>

Brian Ingerson <ingy@cpan.org>

Jesse Vincent <jesse@bestpractical.com>

Yuval Kogman <nothingmuch@woobling.org>

Darren Duncan <perl@DarrenDuncan.net>

Nathan Gray <kolibrie@graystudios.org>

Max Maischein <corion@cpan.org>

Ingo Blechschmidt <iblech@web.de>

Gaal Yahas <gaal@forum2.org>

Mark Stosberg

Simon Sun <dolmens@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2005, 2006. Audrey Tang. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
