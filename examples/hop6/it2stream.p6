

###
### iterator-to-stream.pl
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
