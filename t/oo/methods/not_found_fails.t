#!/usr/bin/pugs

use v6;
use Test;

plan 6;

# See thread "undef.chars" on p6l started by Ingo Blechschmidt:
# http://www.nntp.perl.org/group/perl.perl6.language/22598
{
  # No "use fatal".
  lives_ok { "hi".this_method_does_not_exist(); 1 },
    "'method not found errors' fail() (1)", :todo<bug>;

  lives_ok { my $interesting_undef = "hi".this_method_does_not_exist(); 1 },
    "'method not found errors' fail() (2)", :todo<bug>;

  dies_ok {
    my $interesting_undef = "hi".this_method_does_not_exist();
    $interesting_undef + 3;
  }, "'method not found errors' fail() (3)";
}

skip_rest "No 'use fatal' yet"; exit;

{
  #use fatal;
  dies_ok { "hi".this_method_does_not_exist(); 1 },
    "'method not found errors' die() under use fatal (1)";

  dies_ok { my $interesting_undef = "hi".this_method_does_not_exist(); 1 },
    "'method not found errors' die() under use fatal (2)";

  dies_ok {
    my $interesting_undef = "hi".this_method_does_not_exist();
    $interesting_undef + 3;
  }, "'method not found errors' die() under use fatal (3)";
}
