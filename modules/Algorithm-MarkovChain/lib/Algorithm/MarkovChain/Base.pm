class Algorithm::MarkovChain::Base;
use v6;

# eww - it's a Base.pm

has %:symbols is shape(Any);
has @:start_states;

method seed(Int ?$longest = 4 is copy, *@symbols) {
  die ".seed: No symbols given" unless @symbols;

  push @:start_states, @symbols[0];

  if ($self->{_recover_symbols}) {
      $self->{_symbols}{$_} = $_ for @symbols;
  }

  for 1..$longest -> $length {
    loop my $i = 0; $i + $length < @symbols; $i++ {
      my @link = @symbols[$i .. $i+$length- 1];
      .increment_seen(sequence => @link, symbol => @symbols[$i + $length]);
    }
  }
}

method spew(
  Int ?$length = 30, Int ?$longest_subchain = $length,
  ?@complete,
  Bool ?$force_length, Bool ?$stop_at_terminal, Bool ?$strict_start
) {
  local $; = $self->{seperator};

  my $longest_sequence = .longest_sequence()
    or die ".spew: Don't appear to be seeded!";

  my $length    = $args{length} || 30;
  my $subchain := $longest_subchain; # -- laziness :)

  my @fin; # final chain
  my @sub; # current sub-chain
  @sub = @complete if @complete;

  while @fin < $length {
    if @sub and (not .sequence_known($sub[-1]) or @sub > $subchain) { # we've gone terminal
      push @fin, @sub;
      @sub = ();
      next if $force_length; # ignore stop_at_terminal
      last if $stop_at_terminal;
    }

    unless @sub {
      if $strict_start {
	@sub = pick %:start_states; # XXX -- correct?
      } else {
	@sub = .random_sequence();
      }
    }

    my $consider = 1;
    if @sub > 1 {
      $consider = int rand($longest_sequence - 1);
    }

    my @start = @sub[-$consider .. -1];

    next unless .sequence_known($start); # loop if we missed

    my $cprob;
    my $target = rand;

    my %options = .get_options($start);
    for keys %options -> $word {
      $cprob += %options{$word};
      if $cprob >= $target {
	push @sub, $word;
	last;
      }
    }
  }

  # XXX -- the original author had $#fin = $length here, not sure if his
  # version was a bug.
  @fin.elems = $length if $force_length;

  return @fin;
}

1;
