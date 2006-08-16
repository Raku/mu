#!perl
# Copyright (C) 2001-2003, The Perl Foundation.

use strict;
use warnings;
use Test::More tests => 494;

# XXX: Port this to Perl6.

=head1 DESCRIPTION

These tests are based on L<http://dev.perl.org/perl6/doc/design/syn/S05.html>.
Individual tests are stored in the regex_tests file in the same
directory; each test consist of the following (separated by one or more
tabs):

    pattern being tested
    target string for the pattern
    expected result -- one of
        y     = successful match
        n     = failed match
        /rx/  = output matches result (note that /rx/ is a perl *5* regex)
    description of the test

=head1 SYNOPSIS

	% prove t/compilers/pge/01-regex.t

=cut

my $tests = '../../t/p6regex/regex_tests';

##   Perform the tests.
open TESTS, "<$tests" or die "Can't open $tests";
while (<TESTS>) {
    ##   skip lines without tabs
    next unless /\t/;
    next if m/^#/;
    chomp;
my $x = m/perl5/ ? 1 : 0;
    my($pattern, $target, $result, $description) = split /\t+/, $_;

    $target =~ s/^'(.*)'$/$1/;
    $target =~ s/\\n/\n/g;
    $target =~ s/\\r/\r/g;
    $target =~ s/\\e/\e/g;
    $target =~ s/\\t/\t/g;
    $target =~ s/\\f/\f/g;
    $target =~ s/\\(\d{3})/chr(oct($1))/eg;
    $target =~ s/\\x(..)/chr(hex($1))/eg;

    my @todo = ();
    if ($description =~ m{TODO:}) {
	@todo = ('todo' => $description);
    }

    if ($result =~ m{^/(.*)/$}) {
        p6rule_like($target, $pattern, qr/$1/, $description, @todo);
    } elsif ($result eq 'y') {
        p6rule_is($target, $pattern, $description, @todo);
    } elsif ($result eq 'n') {
        p6rule_isnt($target, $pattern, $description, @todo);
    }
}

use Pugs::Compiler::Rule;
use Pugs::Runtime::Match::Ratchet;
use base 'Pugs::Grammar::Base';

sub match {
    my ($target, $pattern) = @_;
    my $opt;
    while ($pattern =~ s/^:(\w+)(?:\((.*)\))\s+//) {
	$opt->{$1} = defined $2 ? $2 : 1;
    }
    eval { Pugs::Compiler::Rule->compile($pattern, $opt)->match($target) };
}

sub p6rule_is {
    my ($target, $pattern, $description, @todo) = @_;
    my $match = match($target, $pattern);
    ok( !$@ && $match, $description );
    diag $@ if $@;
}

sub p6rule_isnt {
    my ($target, $pattern, $description, @todo) = @_;
    my $match = match($target, $pattern);
    ok( !$@ && !$match, $description );
    diag $@ if $@;
}

sub p6rule_like {
    my ($target, $pattern, $expected, $description, @todo) = @_;
    # XXX: expanded match
    my $match = match($target, $pattern);
    like($@ ? $@ : "$match", $expected, $description);
}

close TESTS;
