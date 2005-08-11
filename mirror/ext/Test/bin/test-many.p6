#!/usr/bin/pugs

# a little script to run multiple tests, re-using the loaded Test
# suite.

# preload some useful modules
use Test;

for @*ARGS -> $test
{
  my $stuff = eval '$pid=open$fh,"-|";{fh=>$fh,pid=>$pid}', :lang<perl5>; # :)

  my $pid = $stuff<pid>;
  my $fh  = $stuff<fh>;

  die unless $pid.defined;

  if ( $pid ) {
      say "In parent!";
      while =$fh {
          say "read a line!  $_";
      }
  } else {
      say "In child!";
      plan 5;
      pass; pass; fail; pass; pass;
      exit;
  }

}
