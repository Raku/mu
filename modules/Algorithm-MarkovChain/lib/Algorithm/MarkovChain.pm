module Algorithm::MarkovChain-0.06;
class Algorithm::MarkovChain isa Algorithm::MarkovChain::Base;
use v6;

# XXX -- correct?
has Hash of shape(Any) %:chains is shape(Array);
has %:totals is shape(Any);

# -- I love that BUILD constructor thing! :)
submethod BUILD(%.chains) {}

method increment_seen(Array $sequence, $symbol) {
    %:totals{$sequence}++;
    %:sequence{$symbol}++;
}

method get_options(Array $sequence) {
  my %res = map {
    $_ => %:chains{$sequence}{$_} / %:totals{$sequence}
  } keys %:chains{$sequence};
}

method longest_sequence() {
  my $l = 0;

  for keys %:chains -> @tmp { # XXX -- correct?
    my $length = +@tmp;
    $l = $length if $length > $l;
  }

  return $l;
}


method sequence_known(Array $sequence) { %:chains{$sequence} }

method random_sequence() { pick %:chains{*} } # XXX -- correct?

1;

__END__

=head1 NAME

Algorithm::MarkovChain - Object oriented Markov chain generator

=head1 SYNOPSIS

  use Algorithm::MarkovChain;
  
  my Algorithm::MarkovChain $chain .= new();
  
  # learn about things from @symbols
  $chain.seed(:symbols(@symbols), :longest(6));
  
  # attempt to tell me something about the sky
  my @newness = $chain.spew(:length(20), :complete(<the sky is>));

=head1 DESCRIPTION

Algorithm::MarkovChain is an implementation of the Markov Chain
algorithm within an object container.

It is implemented as a base class, C<Algorithm::MarkovChain::Base>,
with storage implementations of a hash (C<Algorithm::MarkovChain>),
and an fairly memory efficent implementation using C<glib>
(C<Algorithm::MarkovChain::GHash>).  DBI and MLDBM-friendly versions
are planned.

Deriving alternate representations is intended to be straightforward.

=head1 METHODS

=over

=item Algorithm::MarkovChain.new

Creates a new instance of the Algorithm::MarkovChain class.

=item $obj.seed

Seeds the markov chains from an example symbol stream.

Takes two parameters, one required C<symbols>, one optional C<longest>

C<symbols> presents the symbols to seed from

C<longest> sets an upper limit on the longest chain to
construct. (defaults to 4)

=item $obj.spew

Uses the constructed chains to produce symbol streams

Takes four optional parameters C<complete>, C<length>,
C<longest_subchain>, C<force_length>, C<stop_at_terminal> and
C<strict_start>

C<complete> provides a starting point for the generation of output.
Note: the algorithm will discard elements of this list if it does not
find a starting chain that matches it, this is infinite-loop avoidance.

C<length> specifies the minimum number of symbols desired (default is 30)

C<stop_at_terminal> directs the spew to stop chaining at the first
terminal point reached

C<force_length> ensures you get exactly C<length> symbols returned
(note this overrides the behaviour of C<stop_at_terminal>)

C<strict_start> makes the spew operation always take a known start
state rather than selecting a sequence at random

=item $obj.increment_seen($sequence, $symbol)

Increments the seeness of a symbol following a sequence.

=item $obj.recompute($sequence)

Recompute the probabilities for a branch of the tree.  Called towards
the end of the seed operation for 'dirty' sequences.

=item $obj.get_options($sequence)

Returns possible next symbols and probablities as a hash.

=back

=head1 TODO

=over 4

=item Documentation

I need to explain Markov Chains, and flesh out the examples some more.

=item Fix bugs/respond to feature requests

Just email me <richardc@unixbeard.net> and I'll hit it with hammers...

=back

=head1 BUGS

Hopefully not, though if they probably arise from my not understanding
Markov chaining as well as I thought I did when coding commenced.

That or they're jst stupid mistakes :)

=head1 AUTHOR

Richard Clamp <richardc@unixbeard.net>

Port to Perl 6 by Ingo Blechschmidt <iblech@web.de>

=head1 SEE ALSO

perl(1).

=cut
