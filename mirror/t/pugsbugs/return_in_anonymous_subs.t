#!/usr/bin/pugs

use v6;
use Test;

plan 2;

# See thread "return() in pointy blocks" by Ingo Blechschmidt on p6l
# http://www.nntp.perl.org/group/perl.perl6.language/21745
# We may have to change the expected results of this test if Larry changes his
# mind (which I don't hope).

# { return }
{
  sub foo (Code $code) {
    my $return_to_caller = -> $ret { return $ret }; 

    $code($return_to_caller); 
    return 23; 
  } 

  sub bar (Code $return) { $return(42) } 

  is foo(&bar), 42, "return() inside anonymous subs works", :todo<bug>; 
}

# &return
{
  sub baz (Code $return) { $return(42); return 23 }

  is baz(&return), 42, 'calling &return works', :todo<bug>;
}
