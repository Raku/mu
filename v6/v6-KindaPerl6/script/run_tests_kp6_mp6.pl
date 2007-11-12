#!/usr/bin/env perl

use strict;
use warnings;
use Test::Harness;
use Getopt::Long;
use English; # $EXECUTABLE_NAME ( aka $^X )

my $section = undef;
my $verbose = undef;
GetOptions(

    # section is a string that specifies a of code to test in
    # $section is defined in Makefile.PL
    "section=s" => \$section, # = string

    "verbose|v" => \$verbose, # --verbose or -v
);

$verbose ||= defined $ENV{TEST_VERBOSE} && $ENV{TEST_VERBOSE};


my $args = { section => $section,
             verbose => $verbose,
             debug   => 0,
};

# Test::Harness 3.0 better known as TAP::Harness (Nov. 7th, 2007) was released
# to CPAN.  The invocation of using $ENV{HARNESS_PERL} is not compatible with
# the new standard: http://search.cpan.org/~andya/Test-Harness-3.00/lib/TAP/Harness.pm#new
# see "new( exec => ... )"
# I am breaking the code into 2 parts, one to run on systems without TAP::Harness
# and another to run on systems with TAP::Harness
# daniel lo; dlocaus on #perl6 irc.freenode.net Nov. 12th, 2007.

eval {
    require TAP::Harness;
	TAP::Harness->import;
};

my $ok;

if ( $@ ) {
    $ok = run_test_harness( $args );
} else {
    $ok = run_tap_harness( $args );
}

if ( !$ok ) {
    print STDERR "some tests failed\n";
    exit 1;
} else {
    exit 0;
}

sub run_test_harness {
    my $args = shift;

    if ( $args->{ debug } ) {
        $Test::Harness::Debug = $args->{ debug };
    }

    if ( $args->{ verbose } ) {
        local $ENV{TEST_VERBOSE} = 1;
        $Test::Harness::Verbose = 1;
    }

    my $section = $args->{ section }; # may be undefined

    my $ok = 1;

    unless ( defined $section ) {    # Perl5 tests
                                     # Use $ENV{HARNESS_PERL} or $^X
        local $Test::Harness::Switches = "$Test::Harness::Switches -Icompiled/perl5-kp6-mp6/lib";
        my @tests = glob("t/p5/*.t");
        if (@tests) {
            $ok &&= eval { runtests glob("t/p5/*.t") };
        }
        else {
            warn "t/p5/ has no tests, this is not an error, just a warning";
        }
        warn $@ if $@;
    }

    if ( defined $section ) {        # kp6-perl5.pl tests
        my $perl5 = $ENV{HARNESS_PERL} || $EXECUTABLE_NAME;
        local $ENV{HARNESS_PERL}       = "$EXECUTABLE_NAME script/kp6 -Bperl5";
        local $ENV{PERL5LIB}           = '';
        local $Test::Harness::Switches = '';
        open( TESTS, "TESTS" ) || die "Can not open test list";
        $ok &&= eval { runtests( glob("t/kp6/$section/*.t") ) };
        warn $@ if $@;
    }
    else                             # all
    {                                # kp6-perl5.pl tests
        my $perl5 = $ENV{HARNESS_PERL} || $EXECUTABLE_NAME;
        warn $@ if $@;
        local $ENV{HARNESS_PERL}       = "$EXECUTABLE_NAME script/kp6 -Bperl5";
        local $ENV{PERL5LIB}           = '';
        local $Test::Harness::Switches = '';
        open( TESTS, "TESTS" ) || die "Can not open test list";
        $ok &&= eval {
            runtests( ( map { chomp; "../../t/$_" } <TESTS> ), glob("t/kp6/*.t"), glob("t/kp6/*/*.t") );
        };
    }

    return $ok;
}

sub run_tap_harness {
    my $args = shift;

    my $TAP = { };
    if ( $args->{ debug } ) {
        # needs to be converted to TAP::Harness::???
        $Test::Harness::Debug = $args->{ debug };
    }

    # KP6 scripts do not use TEST_VERBOSE (at least, I did not find any)
    # $TAP->{ verbose } = $args->{ verbose } ? 1 : 0;

    # section is a string that specifies a of code to test in
    # $section is defined in Makefile.PL

    my $section = $args->{ section }; # may be undefined

    my $ok = 1;

    # perl5 Tests
    unless ( defined $section ) {
        my $tap = { %{ $TAP } }; # clone
        $tap->{ lib } = [ qw | compiled/perl5-kp6-mp6/lib | ];

        my @tests = glob("t/p5/*.t");

        if ( @tests ) {
            my $test = TAP::Harness->new( $tap );
            my $aggregator = $test->runtests( @tests );
            $ok = 0 if $aggregator->failed();
            if ( $args->{ verbose } ) {

            }
        }
    }

    if ( defined $section ) {        # kp6-perl5.pl tests
        my $tap = { %{ $TAP } }; # clone
        $tap->{ exec } = [ $EXECUTABLE_NAME, 'script/kp6', '-Bperl5' ];

        my @tests = glob("t/kp6/$section/*.t");

        if ( @tests ) {
            my $test = TAP::Harness->new( $tap );
            my $aggregator = $test->runtests( @tests );
            $ok = 0 if $aggregator->failed();
        } else {
            warn "t/kp6/$section/ has no tests, this is not an error, just a warning.";
        }
    } else {    # kp6-perl5.pl tests
        my $tap = { %{ $TAP } }; # clone
        $tap->{ exec } = [ $EXECUTABLE_NAME, 'script/kp6', '-Bperl5' ];

        my @tests;
        { # read in tests from file: 'TESTS'
            open my $tests, '<', 'TESTS' || die "cannot open test list: 'test' $!";
            while ( ! eof ( $tests ) ){
                $_ = <$tests>;
                chomp; next unless $_;
                push @tests, "../../t/$_";
            }
            close $tests;
        }

        push @tests, glob("t/kp6/*.t");
        push @tests, glob("t/kp6/*/*.t");

        if ( @tests ) {
            my $test = TAP::Harness->new( $tap );
            my $aggregator = $test->runtests( @tests );
            $ok = 0 if $aggregator->failed();
        } else {
            warn "t/kp6/$section/ has no tests, this is not an error, just a warning.";
        }
    }

    return $ok;
}

