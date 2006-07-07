

###
### iterator-to-stream.pl
###
### Originally from Higher-Order Perl by Mark Dominus, published by Morgan
### Kaufmann Publishers, Copyright 2005 by Elsevier Inc
###
### Ported to Perl6 by Dan Brook
###

## Chapter 8 section 1.4

use lib '../Chap6';
use Stream 'node';

sub iterator_to_stream(Code $it) { # is exported(:all)
  my $v = $it.();
  return
    unless defined $v;
  return node($v, -> { iterator_to_stream($it) });
}

1;
