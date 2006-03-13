#!/usr/bin/perl -w

if(!@ARGV) {
  die "Usage: $0 [STRING|--file FILENAME] REGEXP [--bench [DURATION-SEC]] \n";
}
my $str = shift;
if($str eq '--file') {
  my $file = shift;
  $str = `cat $file`;
}
my $re = shift;
my $bench = shift;
my $duration = shift || 10;

use Benchmark;
require 'regexp_engine_demo.pl';

#my $m = match_re($re,$str);
my $r = compile($re) || die "Compile failed";
my $m;
if($bench) {
  timethis (-1*$duration, sub {$m = match($r,$str)});
} else {
  $m = match($r,$str);
}
print $m->describe,"\n";
