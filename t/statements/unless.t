#!/usr/bin/pugs

use v6;
require Test;

=kwid

Basic "unless" tests

=cut

plan 6;

my $x = 'test';
unless ($x ne $x) { pass("unless ($x eq $x) {} works");  } 
             else { fail("unless ($x eq $x) {} failed"); }
             
unless ($x eq $x) { fail("unless ($x ne $x) {} failed"); } 
             else { pass("unless ($x ne $x) {} works");  }
             
unless (0) { pass("unless (1) {} works");  } 
      else { fail("unless (1) {} failed"); }
      
unless (1) { fail("unless (0) {} failed"); } 
      else { pass("unless (0) {} works");  }
      
unless (undef) { pass("unless (undef) {} works");  } 
          else { fail("unless (undef) {} failed"); }

# die called in the condition part of an if statement should die immediately
# rather than being evaluated as true
my $foo = 1;
eval 'unless (die "should die") { $foo = 3 } else { $foo = 2; }';
#say '# $foo = ' ~ $foo;
is $foo, 1, "die should stop execution immediately.";
