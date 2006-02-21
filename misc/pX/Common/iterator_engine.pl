# pX/Common/iterator_engine.pl - fglock
#

use strict;
use warnings;

=pod

A "rule" function gets as argument:

- a "continuation" (or a zero, to get the first match)
- a string to match

it returns (or "yields"):

- a "continuation" or undef
- a "match" or undef
- the string tail or undef

Continuations are used for backtracking.

A "ruleop" function gets some arguments and returns a "rule".

=cut

# XXX - optimization - pass the string index around, 
# XXX   instead of copying the whole string to $tail every time

sub ruleop::greedy_plus { 
    my $node = shift;
    my $alt;
    $alt = ruleop::concat( 
        $node, 
        ruleop::optional( sub{$alt->(@_)} )
    );
    return $alt;
}
sub ruleop::greedy_star { 
    my $node = shift;
    return ruleop::optional( ruleop::greedy_plus( $node ) );
}

sub ruleop::non_greedy { 
    my $node = shift;

    # TODO - this sub is not completely implemented / tested

    # XXX - possible implementation strategy - reuse rule::concat ?
    return sub {
        my ( $n, $tail ) = shift;
        my @matches;
        my @cont;
        for (1 .. ++$n) {
                my ($state, $match, $new_tail) = $node->(0, $tail);
                return unless $match;
                $tail = $new_tail;
                push @matches, $match;
                push @cont, $state;
        }
        # XXX - return ( @continuation, ...
        # TODO - post-process @cont for proper permutation
        return ( \@cont, { 'non_greedy' => [ @matches ] }, $tail );
    }
}

sub ruleop::alternation {
    # XXX - is this supposed to return the longest match first?
    # XXX   in which case it would have to test all possibilities before returning
    my @nodes = @_;
    return sub {
        my $n = shift;
        #print "testing alternations on @_\n";
        return unless @nodes;
        my $match;
        my $tail;
        $n = [ 0, 0 ] if $n == 0;
        my $state = [ $n->[0], $n->[1] ];
        while( defined $state ) {
            ($state->[1], $match, $tail) = $nodes[ $state->[0] ]->( $state->[1], @_);
            if ( ! defined $state->[1] ) {
                $state->[0]++;
                $state->[1] = 0;
                $state = undef if $state->[0] > $#nodes;
            }
            #return ( $state, { 'alternation' =>$match }, $tail) if $match;
            return ( $state, $match, $tail) if $match;
        }
        return;
    }
}

sub ruleop::concat {
    my @concat = @_;
    # TODO: generalize for @concat > 2
    return sub {
        my $n = shift;
        my @matches;
        my $tail;
        my $state;
        my $state0 = ref($n) ? $n->[0] : 0;
        my $state1 = ref($n) ? $n->[1] : 0;
        while (1) {
            ($state, $matches[0], $tail) = $concat[0]->($state0, @_);
            #print "  1st match: ", Dumper( [ $state, $matches[0], $tail ] );
            if ( ! defined $matches[0] ) {
                return unless defined $state;
                $state0 = $state;
                $state1 = 0;
                next;
            }
            ($state1, $matches[1], $tail) = $concat[1]->($state1, $tail);
            #print "  2nd match: ", Dumper( [ $state, $matches[1], $tail ] );
            if ( defined $matches[1] ) {
                my $succ;
                if ( ! defined( $state1 ) ) {
                    $succ = [ $state, 0 ] if defined $state;
                }
                else {
                    $succ = [ $state0, $state1 ];
                }
                #return ( $succ, { 'concat'=>[ @matches ] }, $tail);
                return ( $succ, \@matches, $tail);
            }
            if ( !defined( $state1 ) ) {
                return unless defined $state;
                $state0 = $state;
                $state1 = 0;
            }
            #print "backtracking: $state1 - $state0\n";
        }
    }
}

sub ruleop::constant { 
    my $const = shift;
    return sub {
        return if +shift;  # no continuation
        return unless $_[0] =~ m/^(\Q$const\E)(.*)/;
        return ( undef, { constant => $1 }, $2 );
    }
}

sub ruleop::optional {
    return ruleop::alternation( $_[0], ruleop::null() );
}

sub ruleop::null {
    return sub {
        return if +shift;  # no continuation
        ( undef, [], $_[0] );
    }
};


1;

__END__


# old tests

print "greedy\n", Dumper( 
  rule::greedy( rule::constant('a') )->( 0, qw(a a a b c) ) 
);
print "greedy backtrack\n", Dumper( 
  rule::concat( 
    rule::greedy( rule::constant('a') ),
    \&{'rule::.'} 
  )->( 0, qw(a a a a) ) 
);
print "greedy no-match\n", Dumper( 
  rule::concat(
    rule::greedy( rule::constant('a') ),
    \&{'rule::.'}
  )->( 0, qw(b a a a a) ) 
);
print "word\n", Dumper( 
  &{'rule::<word>'}( 0, qw(b a a ! !) ) 
);
print "word concat\n", Dumper( 
  rule::concat( \&{'rule::<word>'}, \&{'rule::<ws>'} )->( 0, qw(b a ),' ' ) 
);
print "non_greedy + backtracking\n", Dumper( 
  rule::concat(
    rule::non_greedy( rule::constant('a') ),
    rule::constant('ab')
  )->( 0, qw(a a a a b) ) 
);
print "alternation + backtracking\n", Dumper( 
  rule::concat(
    rule::alternation( rule::constant('a'), rule::constant('ab') ),
    rule::constant('ab')
  )->( 0, qw(a b a b) ) 
);
print "alternation + greedy + backtracking -- (ab,a,ab)(ab)\n", Dumper( 
  rule::concat(
    rule::greedy(
      rule::alternation( rule::constant('a'), rule::constant('ab') )
    ),
    rule::constant('ab')
  )->( 0, qw(a b a a b a b) ) 
);
