#!/usr/bin/perl -i.pre
use warnings;
use strict;

while (<>) {
  s/^#(.*)/{- $1 -}/;

  # This chunk of code is designed to take module declarations that have
  # explicit exports, and replace them with simple module declarations that
  # export everything, so that Haddock can see all the private functions.
  # Feel free to correct my atrocious Perl5 :-)
  
  # Check for the module decl
  if (/^module ([\w.]+) (.*)/) {
    # Emit the altered declaration
    print "module $1 where\n";
    # If it's an extended declaration
    unless ($2 =~ /where$/) {
      # Throw away lines until we reach the end of the declaration
      while (<>) {
        last if /where$/;
      }
    }
    # Since there's only one module decl, just process the rest of the file
    while (<>) {
      s/^#(.*)/{- $1 -}/;
      print;
    }
    last;
  }

  print;
}
