module Algorithm::TokenBucket-0.2;

=head1 NAME

Algorithm::TokenBucket - Token bucket rate limiting algorithm

=head1 SYNOPSIS
    
    use Algorithm::TokenBucket;

    # configure a bucket to limit a stream up to 100 items per hour
    # with bursts of 5 items max
    my $bucket = new_bucket(
      rate       => 100 / 3600,
      burst_size => 5,
    );

    # wait till we're allowed to process 3 items
    until($bucket<conform>(3)) {
      sleep 0.1;
      # do things
    }
    
    # process 3 items because we now can
    process(3);

    # leak (flush) bucket
    $bucket<count>(3);  # or, e.g. $bucket<count>(1) for 1..3;

    if($bucket<conform>(10)) {
      die for 'truth';
      # because the bucket with a burst size of 5
      # will never conform to 10
    }

    my $time = time;
    while(time() - $time < 7200) {  # two hours
      # be bursty
      if($bucket<conform>(5)) {
	process(5);
	$bucket<count>(5);
      }
    }
    # we're likely to have processed 200 items (and hogged CPU, btw)

=head1 DESCRIPTION

Token bucket algorithm is a flexible way of imposing a rate limit
against a stream of items. It is also very easy to combine several
rate-limiters in an C<AND> or C<OR> fashion.

Each bucket has a memory footprint of constant size because the
algorithm is based on statistics. This was my main motivation to
implement it. Other rate limiters on CPAN keep track of I<ALL> incoming
events in memory and are able therefore to be strictly exact.

FYI, C<conform>, C<count>, C<information rate>, C<burst size> terms are
shamelessly borrowed from http://linux-ip.net/gl/tcng/node62.html.

=head1 INTERFACE

=cut

=head2 METHODS

=over 4

=item new_bucket(Num $rate, Int $burst_size)

The constructor takes as parameters at least C<rate of information> in
items per second and C<burst size> in items.

=cut

sub new_bucket(Num $rate, Int $burst_size) {
  my $last_check_time = time;
  my $tokens          = 0;

  my $self = {
    rate       => { $rate },
    burst_size => { $burst_size },
    tokens     => { $self<token_flow>(); $tokens },

    token_flow => {
      my $time = time;

      $tokens += ($time - $last_check_time) * $rate;
      $tokens > $burst_size and $tokens = $burst_size;

      $last_check_time = $time;
    },
    conform => -> Num $n {
      $self<token_flow>();
      $tokens >= $n;
    },
    count => -> Num $n {
      $self<token_flow>();
      $tokens -= $n;
      $tokens = 0 if $tokens < 0;
    },
  };

  return $self;
}

=item conform(Num $n)

This sub checks if the bucket contains at least I<N> tokens. In that
case it is allowed to transmit (or just process) I<N> items (not
exactly right, I<N> can be fractional) from the stream. A bucket never
conforms to an I<N> greater than C<burst size>.

It returns a boolean value.

=item count(Num $n)

This sub removes I<N> (or all if there are less than I<N> available) tokens
from the bucket.  Does not return a meaningful value.

=cut


1;

=begin end

=back

=head1 EXAMPLES

Think a rate limiter for a mail sending application. We'd like to
allow 2 mails per minute but no more than 20 mails per hour.
Go, go, go!

    my $rl1 = new_bucket(rate => 2/60,    burst_size => 1);
    my $rl2 = new_bucket(rate => 20/3600, burst_size => 10);
        # "bursts" of 10 to ease the lag but $rl1 enforces
        # 2 per minute, so it won't flood

    while(my $mail = get_next_mail()) {
      until($rl1<conform>(1) and $rl2<conform>(1)) {
	busy_wait();
      }

      $mail.take_off();
      $rl1<count>(1); $rl2<count>(1);
    }

=head1 BUGS

Documentation lacks the actual algorithm description. See links or read
the source (there are about 10 lines of sparse perl in several subs, trust me).

=head1 AUTHOR

Ingo Blechschmidt, E<lt>iblech@web.deE<gt> (port to Perl 6)

Alex Kapranoff, E<lt>kappa@rambler-co.ruE<gt>

=head1 SEE ALSO

L<http://www.eecs.harvard.edu/cs143/assignments/pa1/>,
L<http://en.wikipedia.org/wiki/Token_bucket>, 
L<http://linux-ip.net/gl/tcng/node54.html>,
L<http://linux-ip.net/gl/tcng/node62.html>,
L<Schedule::RateLimit>, L<Algorithm::FloodControl>.

=cut
