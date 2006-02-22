# pX/Common/iterator_engine.pl - fglock
#

use strict;
use warnings;

=pod

A "rule" function gets as argument:

- a string to match
- an optional "continuation"

it returns (or "yields"):

- a "continuation" or undef
- a "match" or undef
- the string tail or undef

Continuations are used for backtracking.

A "ruleop" function gets some arguments and returns a "rule".

=cut

# XXX - api change: return ( $match, $tail, $next ) instead of ( $next, $match, $tail )
# XXX   because most of the time only ( $match ) is needed.

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
        my ( $tail, $n ) = shift;
        my @matches;
        my @cont;
        for (1 .. ++$n) {
                my ($state, $match, $new_tail) = $node->($tail, 0);
                return unless $match;
                $tail = $new_tail;
                push @matches, $match;
                push @cont, $state;
        }
        # TODO - post-process @cont for proper permutation
        return ( \@cont, { 'non_greedy' => [ @matches ] }, $tail );
    }
}

sub ruleop::alternation {
    # alternation is first match (not longest).  though we need a 
    # separate longest match for tokens (putter on #perl6)
    my @nodes = @_;
    return sub {
        my $n = $_[1] || [ 0, 0 ];
        #print "testing alternations on @_\n";
        return unless @nodes;
        my $match;
        my $tail;
        my $state = [ $n->[0], $n->[1] ];
        while( defined $state ) {
            ($state->[1], $match, $tail) = 
                $nodes[ $state->[0] ]->( $_[0], $state->[1] );
            if ( ! defined $state->[1] ) {
                $state->[0]++;
                $state->[1] = 0;
                $state = undef if $state->[0] > $#nodes;
            }
            #print "alternate: match \n".Dumper($match) if $match;
            return ( $state, $match, $tail) if $match;
        }
        return;
    }
}

sub ruleop::concat {
    my @concat = @_;
    # TODO: generalize for ( @concat > 2 ) would help reduce stack usage
    return sub {
        my $n = $_[1] || [ 0, 0 ];
        my @matches;
        my $tail;
        my $state;
        my $state0 = $n->[0];
        my $state1 = $n->[1];
        while (1) {
            ($state, $matches[0], $tail) = $concat[0]->( $_[0], $state0 );
            #print "  1st match: ", Dumper( [ $state, $matches[0], $tail ] );
            if ( ! defined $matches[0] ) {
                return unless defined $state;
                $state0 = $state;
                $state1 = 0;
                next;
            }
            ($state1, $matches[1], $tail) = $concat[1]->( $tail, $state1 );
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
        return unless $_[0] =~ m/^(\Q$const\E)(.*)/s;
        return ( undef, { constant => $1 }, $2 );
    }
}

sub ruleop::optional {
    return ruleop::alternation( $_[0], ruleop::null() );
}

sub ruleop::null {
    return sub {
        ( undef, [], $_[0] );
    }
};

sub ruleop::label {
    my $label = shift;
    my $node = shift;
    return sub {
        my ($state, $match, $tail) = $node->( @_ );
        return unless $match;
        return ( $state, { $label => $match }, $tail )
    }
}

1;
