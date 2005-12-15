#!/usr/bin/pugs

use v6;
use Test;

sub pil_output( Str $code, Str $optional_args? ) returns Str {
  my $tempfilename = "pil_test_" ~ $Test::num_of_tests_run;
  my $codefh = open( $tempfilename , :w);
  $codefh.print($code);
  $codefh.close();
  my $pilfh = Pipe::open("./pil $optional_args $tempfilename");
  my $pilresult = $pilfh.readline();
  $pilfh.close();
  return $pilresult;
}

sub pil_is_eq( Str $code, Str $expected, Str $description? ) returns Bool {
  my $pilresult = pil_output( $code );
  is( chomp($pilresult), chomp($expected), $description );
}

sub pil_parsed_is_eq( Str $code, Str $expected, Str $description? ) returns Bool {
  my $pilresult = pil_output( $code, "-P");
  is( chomp($pilresult), chomp($expected), $description );
}

