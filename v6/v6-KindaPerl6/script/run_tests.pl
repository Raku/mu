#!/usr/bin/env perl

=head1 NAME

script/run_tests.pl

=head1 VERSION

Version 0.02

=cut

our $VERSION = '0.02';

=head1 SYNOPSIS

 pugs/v6/v6-KindaPerl6> perl script/run_tests.pl

This will execute the basic tests for the Perl 5 backend.  Other backends are
available.

=head2 Options

=over

=item --section=[Section Name]

Section name may be "io", "grammar", "math".  Not specifying a [Section Name]
will cause all sections to run.

=item --backend=[backend]

Available backends are

 perl5    = Perl 5 (using the MiniPerl6 regex engine)     <= default
 perl5rx  = Perl 5 (using the Perl 5 regex engine)
 cl-sbcl  = lisp_sbcl
 cl-ecl   = lisp ecl
 cl-clisp = lisp clisp

=back

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

use strict;
use warnings;
use English;
use Test::Harness;
use Getopt::Long;

# use TAP::Harness

use constant DEFAULT_BACKEND => 'perl5';

#$Test::Harness::Debug = 1;
$Test::Harness::Verbose = 1 if $ENV{TEST_VERBOSE};

my $extra_libs = '';

my %opt = (
    section => undef,
    backend => undef,
    verbose => '0', # TAP::Harness - normal
);

GetOptions(

    # section is a string that specifies a of code to test in
    # $section is defined in Makefile.PL
    # grep -- '--section' ../Makefile.PL
    "section:s" => \$opt{section},

    # specify a backend
    "backend:s" => \$opt{backend},

    # TAP::Harness uses 'verbosity'
    "verbose|verbosity|v=i" => \$opt{verbose},    # --verbose or -v
);

if ( $ENV{TEST_VERBOSE} && $ENV{TEST_VERBOSE} ) {
    $opt{verbose} ||= $ENV{TEST_VERBOSE};
}

unless ( $opt{backend} ) {
    my $message = <<EOT;
No backend specified defaulting to: "perl5"
EOT

    warn $message;
    $opt{backend} = DEFAULT_BACKEND;
}

if ( $opt{backend} eq "perl5rx" ) {
    $extra_libs = '-Ilib5regex';
}

{    # main
    # get our tests
    my @tests = get_tests( \%opt );
    die "No tests!" unless @tests;

    eval {
        require TAP::Harness;
        TAP::Harness->import;
    };

    my $ok;
    if ($@) {
        # print STDERR "running with Test::Harness < 3.0\n";
        $ok = run_test_harness(
            {   tests   => \@tests,
                backend => $opt{backend},
            }
        );
    }
    else {
        # print STDERR "running with Test::Harness 3.0+\n";
        $ok = run_tap_harness(
            {   tests   => \@tests,
                backend => $opt{backend},
                tap_new => {
                    # in 3.03, the verbose is properly stated as verbosity
                    # See: vi +66 /usr/local/share/perl/5.8.8/TAP/Harness.pm
                    verbosity => $opt{verbose},
                },
            }
        );
    }

    if ($ok) {
        exit 0;
    }
    else {
        print STDERR "some tests failed\n";
        exit 1;
    }
}

sub run_test_harness {
    my $args = shift;

    # PRE Test::Harness 3.0
    local $ENV{HARNESS_PERL} = "$EXECUTABLE_NAME $extra_libs script/kp6 -B$args->{ backend }";

    my $ok = eval { runtests( @{ $args->{tests} } ); };
    warn $@ if $@;

    return $ok;
}

sub run_tap_harness {
    my $args = shift;

    my $tap = { %{ $args->{tap_new} } };    # clone
    if ( defined $tap->{verbosity} ) {
        my $t = $tap->{verbosity}; # I am lazy
        # make sure this is the right value
        die "verbosity has an invalid value: $t ( 1, 0, -1, or -2 )\nDid you set \$ENV{TEST_VERBOSE} to a bad value?" unless
            $t == 1 or $t == 0 or $t == -1 or $t == -2;
    }

    # TAP::Harness 3.00 documentation is wrong
    # the correct invocation is { exec => [ $prog,$arg1,$arg2 ] }
    my @exec = ( $EXECUTABLE_NAME );

    # perl takes the code from STDIN if there is a blank argument IE
    # perl '' test.pl
    push @exec, $extra_libs if $extra_libs; # do not put in an "empty" argument
    push @exec, 'script/kp6';
    push @exec, '-B' . $args->{backend};
    $tap->{exec} = \@exec;

    my $test       = TAP::Harness->new($tap);
    my $aggregator = $test->runtests( @{ $args->{tests} } );

    return 0 if $aggregator->failed();
    return 1;
}

###############################################################################
# Supporting subroutines

sub get_tests {
    my $args = shift;

    my @tests;

    if ( defined $args->{section}
        && $args->{section} =~ /^todo/
       )
    {
        die "$args->{ section } does not exist" unless -d "t/$args->{ section }";
        @tests = glob "t/$args->{ section }/*.t";
        push @tests, glob("t/$args->{ section }/*/*.t");
    }
    elsif ( defined $args->{section} ) {
        die "$args->{ section } does not exist" unless -d "t/kp6/$args->{ section }";
        @tests = glob "t/kp6/$args->{ section }/*.t";
    }
    else {
        @tests = map { chomp; $_ } <DATA>;
        push @tests, glob("t/kp6/*.t");
        push @tests, glob("t/kp6/*/*.t");
    }

    test_existance(@tests); # will die if there is a problem.

    return @tests;
}

sub test_existance {
    for my $test ( @_ ) {
        die "$test: does not exist" unless -e $test;
        die "$test: does not have read permission" unless -r _;
    }
}

# this tests are basic tests, that we run "just to do a sanity check"

=head2 Changes

Change      Description

    0.02    Nov. 12th, 2007
            dlocaus on #perl6 irc.freenode.net
            Test::Harness 3.0 better known as TAP::Harness was released to CPAN
            on (Nov. 7th, 2007).  The invocation of using $ENV{HARNESS_PERL} is
            not compatible with the new standard:

            http://search.cpan.org/~andya/Test-Harness-3.00/lib/TAP/Harness.pm#new

            see "new( exec => ... )"

            I am breaking the code into 2 parts, one to run on systems without
            TAP::Harness and another to run on systems with TAP::Harness

    0.01    ????
            original code.

=cut

# these tests below can be run manually by
# perl script/kp6 ../../t/01-sanity/01-tap.t
# or if you want to see the perl output
# perl script/kp6 -o test.pl ../../t/01-sanity/01-tap.t

__DATA__
../../t/01-sanity/01-tap.t
../../t/01-sanity/03-equal.t
../../t/01-sanity/04-if.t
../../t/01-sanity/05-sub.t
../../t/01-sanity/06-use.t
../../t/01-sanity/07-binding.t
../../t/01-sanity/07-substr.t
