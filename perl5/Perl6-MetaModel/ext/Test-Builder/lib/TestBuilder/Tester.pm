#!/usr/bin/perl

use strict;
use warnings;

use Perl6::MetaModel;

1;

__END__

module Test::Builder::Tester-0.2.1;

use v6-alpha;

use Test::Builder;
use Test::Builder::Output;

class Test::Builder::Tester::Output
{
    has @:output;
    has @:diagnostics;

    method write ( Str $message is copy )
    {
        push @:output, $message;
    }

    method diag ( Str $message is copy )
    {
        push @:diagnostics, $message;
    }

    method output returns Str
    {
        # XXX - hack or pugsbug?
        return '' unless +@:output;

        my $output = @:output.join( "\n" );
        @:output   = ();
        return $output;
    }

    method diagnostics returns Str
    {
        # XXX - hack or pugsbug?
        return '' unless +@:diagnostics;

        my $diagnostics = @:diagnostics.join( "\n" );
        @:diagnostics   = ();
        return $diagnostics;
    }
}

my $Test      = Test::Builder.create( output => Test::Builder::Output.new() );

my @expect_out;
my @expect_diag;

# populate the Test::Builder singleton with a controlled object
my $tb_output = Test::Builder::Tester::Output.new();
my $tb_test   = Test::Builder.new( output => $tb_output );
$tb_test.plan( 'no_plan' );

# remove header from output object; it gets in the way of the first test
$tb_output.output();

sub plan ( Int $tests ) is export
{
    $Test.plan( tests => $tests );
}

sub line_num is export
{
}

sub test_pass ( Str $diagnostic? ) is export
{
    report_test( 'ok', $diagnostic );
}

sub test_fail ( Str $diagnostic? ) is export
{
    report_test( 'not ok', $diagnostic );
}

sub report_test ( Str $type, Str $diagnostic? )
{
    my $number = $tb_test.get_test_number();
    my $line   = "$type $number";
    $line     ~= " - $diagnostic" if defined $diagnostic;
    test_out( $line );
}

sub test_out ( Str $line ) is export
{
    push @expect_out, $line;
}

sub test_err ( Str $line ) is export 
{
    push @expect_diag, $line;
}

sub test_diag ( Str $line ) is export
{
    push @expect_diag, $line;
}

sub test_test ( Str $description = '' ) returns Bit is export 
{
    my $expect_out    = @expect_out.join(  "\n" ) || '';
    my $expect_diag   = @expect_diag.join( "\n" ) || '';
    @expect_out       = ();
    @expect_diag      = ();

    my $received_out  = $tb_output.output();
    my $received_diag = $tb_output.diagnostics();

    my $out_matches   = $expect_out  eq $received_out;
    my $diag_matches  = $expect_diag eq $received_diag;

    return 1 if $Test.ok( ($out_matches && $diag_matches), $description );

    $Test.diag(
        "output mismatch\nexpected: $expect_out\nreceived: $received_out\n"
    ) unless $out_matches;

    $Test.diag(
        "diagnostics mismatch\n" ~
        "expected: '$expect_diag'\nreceived: '$received_diag'\n"
    ) unless $diag_matches;

    return 0;
}

=pod

=head1 NAME

Test::Builder::Tester

=head1 SYNOPSIS

  use Test::Builder;
  use Test::Builder::Tester;

  plan 2;

  my $Test = Test::Builder.new();

  test_out( 'ok 1 - Hello' );
  $Test.ok( 1, 'Hello' );
  test_test( 'passing_test );

  test_out( 'not ok 2 - Goodbye' );
  test_diag( "The test failed!" );
  $Test.ok( 0, 'Goodbye' ) || $Test.diag( 'The test failed!' );
  test_test( 'failing test with diagnostics' );

=head1 DESCRIPTION

This test module allows you to test test modules that use Test::Builder.  It
does this by you setting the expected output and diagnostic output of tests,
running those tests, and then telling this module to compare the received
output to the expected output.

=head1 EXPORTED FUNCTIONS

=over 4

=item B<plan( Int $tests )>

Plans the number of tests to run.  Someday there will be a C<no_plan> version
of this too.  For now, this is what you have.

=item B<test_pass( Str $description )>

Marks that the next test should pass.  C<$description> is the optional
description of the test.  This is the easiest way to match a passing test.

=item B<test_fail( Str $description )>

Marks that the next test should fail.  C<$description> is the optional
description of the test.  This is the easiest way to match a failing test.

=item B<test_out( Str $line )>

Adds a line to the list of lines of expected output for the test you're going
to run.

=item B<test_diag( Str $line )>

Adds a line to the list of lines of expected diagnostics for the test you're
going to run.

=item B<test_test( Str $description? )>

Compares the receivied and expected lines from the previously run tests,
passing if both the diagnostics and output match exactly.  If you pass
C<$description>, this will use it as the test description.

=back

=head1 AUTHOR

based on L<Test::Tester> by Mark Fowler

ported to Perl 6 by chromatic C<< chromatic at wgz dot org >>

=cut
