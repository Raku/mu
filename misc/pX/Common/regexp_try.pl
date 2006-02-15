#!/usr/bin/perl -w

my $re = shift;
my $str = shift;
if($str eq '--file') {
  my $file = shift;
  $str = `cat $file`;
}

require 'regexp_engine_demo.pl';
my $m = match_re($re,$str);
print $m->describe,"\n";
