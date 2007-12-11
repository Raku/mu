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

=item --include=[Directory] or -I[Directory]

Specify libraries to include. You can specifiy multiple libraries to include

--include=Dir1 --include=Dir2

--backend=perl5rx automatically implies --include=lib5regex

B<Note>: Getopt::Long < 2.37 will not support multiple --include properly
for Getopt::Long < 2.37 only 1 --include will be supported.

Note, that --backend=perl5rx will automatically still include.

=item --backend=[backend]  or -B[backend]

Available backends are

 perl5    = Perl 5 (using the MiniPerl6 regex engine)     <= default
 perl5rx  = Perl 5 (using the Perl 5 regex engine)
 perl6    = Perl 6
 cl-sbcl  = lisp_sbcl
 cl-ecl   = lisp ecl
 cl-clisp = lisp clisp

B<Note>: if you use the perl6 backend, then the code will be run though the
perl6 backend twice.

Ex:

 script/kp6 --backend=perl6 myTestCase.pl | script/kp6

The purpose of this is to make sure the perl6 emiter works.

See: perldoc script/perl6_to_perl6.pl

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

# Not all people have TAP::Harness yet.
# use TAP::Harness

#$Test::Harness::Debug = 1;
$Test::Harness::Verbose = 1 if $ENV{TEST_VERBOSE};

my %opt = (
    section => undef,
    backend => 'perl5',            # <=== DEFAULT BACKEND
    include => [],
    verbose => '0',                # TAP::Harness - normal
    exec    => $EXECUTABLE_NAME,
);

my @bugfix;
if ($Getopt::Long::VERSION < 2.37 ) {
    warn("Getopt::Long 2.37 or less does not support \@s properly, multiple --include is not supported");
    @bugfix = ( 'include|I=s', \$opt{include});
    $opt{include}='';
} else {
    @bugfix = ('include|I=s@', \$opt{include});
}

Getopt::Long::Parser->new( config => [qw( bundling no_ignore_case pass_through require_order)], )->getoptions(
    # section is a string that specifies a of code to test in
    # $section is defined in Makefile.PL
    # grep -- '--section' ../Makefile.PL
    "section|s=s" => \$opt{section},

    # specify a backend
    "backend|B=s" => \$opt{backend},

    # specify libraries to include
    # =s@ means, strings, can have more than 1
    @bugfix,

    # use this execution string
    "exec=s" => \$opt{exec},

    # TAP::Harness uses 'verbosity'
    "verbose|verbosity|v=i" => \$opt{verbose},    # --verbose or -v
);

if ( $Getopt::Long::VERSION < 2.37 ) {
    # put it to what we want it to be
    $opt{include}=[ $opt{include} ];
}

if ( $ENV{TEST_VERBOSE} && $ENV{TEST_VERBOSE} ) {
    $opt{verbose} ||= $ENV{TEST_VERBOSE};
}

if ( $opt{backend} eq 'perl6' ) {
    die "You cannot have --backend=perl6 and --exec=something\n-because --backend will set --exec=script/perl6_to_perl6.pl"
        if defined $opt{exec} && $opt{exec} ne $EXECUTABLE_NAME;
    $opt{exec} = 'script/perl6_to_perl6.pl';
}

if ( $opt{backend} eq 'perl5rx' ) {
    push @{ $opt{include} }, 'lib5regex';
}

# reduce perl include options to just a single string
# or set to undef if there is none
if ( scalar @{ $opt{include} } ) {

    # put the -I back into $opt{include}'s
    my @libs = @{ $opt{include} };
    @libs = map { '-I' . $_ } @libs;
    $opt{include} = join ' ', @libs;
}
else {
    $opt{include} = undef;
}

######################################################################
# main
{    # get our tests
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
                include => $opt{include},
                exec    => $opt{exec},
            }
        );
    }
    else {

        # print STDERR "running with Test::Harness 3.0+\n";
        $ok = run_tap_harness(
            {   tests   => \@tests,
                backend => $opt{backend},
                include => $opt{include},
                tap_new => {

                    # in 3.03, the verbose is properly stated as verbosity
                    # See: vi +66 /usr/local/share/perl/5.8.8/TAP/Harness.pm
                    verbosity => $opt{verbose},
                    exec      => $opt{exec},
                },
            }
        );
    }

    exit 0 if $ok;

    print STDERR "Some tests failed\n";
    exit 1;
}

sub run_test_harness {
    my $args = shift;

    my $libs = defined $args->{include} ? $args->{include} : '';

    # PRE Test::Harness 3.0
    local $ENV{HARNESS_PERL} = "$args->{exec} $libs script/kp6 -backend=$args->{ backend }";

    my $ok = eval { runtests( @{ $args->{tests} } ); };
    warn $@ if $@;

    return $ok;
}

sub run_tap_harness {
    my $args = shift;

    my $tap = { %{ $args->{tap_new} } };    # clone
    if ( defined $tap->{verbosity} ) {
        my $t = $tap->{verbosity};          # I am lazy

        # make sure this is the right value
        die "verbosity must be a number: '$t' is invalid" unless $t =~ /^[+-]?\d+$/;
        die "verbosity has an invalid value: $t ( 1, 0, -1, or -2 )\nDid you set \$ENV{TEST_VERBOSE} to a bad value?"
            unless $t == 1
                or $t == 0
                or $t == -1
                or $t == -2;
        delete $tap->{verbosity} unless $tap->{verbosity};
    }

    # TAP::Harness 3.00 documentation is wrong
    # the correct invocation is { exec => [ $prog,$arg1,$arg2 ] }
    my @exec = ( $tap->{exec} );

    # perl takes the code from STDIN if there is a blank argument IE
    # perl '' test.pl
    push @exec, $args->{include} if defined $args->{include};
    push @exec, 'script/kp6';
    push @exec, '--backend=' . $args->{backend};
    $tap->{exec} = \@exec;

    # print STDERR "Exec = ", (join " ", @exec ), "\n";

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
        && $args->{section} =~ /^todo/ )
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

    test_existance(@tests);    # will die if there is a problem.

    return @tests;
}

sub test_existance {
    for my $test (@_) {
        die "$test: does not exist"                unless -e $test;
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
