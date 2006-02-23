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
# XXX - or: return hash{ match, tail, state, capture ... }

# XXX - optimization - pass the string index around, 
# XXX   instead of copying the whole string to $tail every time

# XXX - weaken self-referential things

sub ruleop::greedy_plus { 
    my $node = shift;
    my $alt;
    $alt = ruleop::concat( 
        $node, 
        ruleop::optional( sub{ goto $alt } ),  
    );
    return $alt;
}
sub ruleop::greedy_star { 
    my $node = shift;
    return ruleop::optional( ruleop::greedy_plus( $node ) );
}
sub ruleop::non_greedy_star { 
    my $node = shift;
    ruleop::alternation( [ 
        ruleop::null(),
        ruleop::non_greedy_plus( $node ) 
    ] );
}
sub ruleop::non_greedy_plus { 
    my $node = shift;

    # XXX - needs optimization for faster backtracking

    return sub {
        my $tail = $_[0];
        my $state = $_[1] || { state => undef, op => $node };
        my $match;
        my $state_post;
        my $tail_post;
        ($state_post, $match, $tail_post) = 
            $state->{op}->( $tail );
            #$state->{op}->( $tail, $state->{state} );   # XXX - didin't work
        return unless $match;
        $state->{state} = $state_post;
        $state->{op} = ruleop::concat( $node, $state->{op} );
        return ( $state, $match, $tail_post );
    }
}

sub ruleop::alternation {
    # alternation is first match (not longest).  though we need a 
    # separate longest match for tokens (putter on #perl6)

    # note: the list in @$nodes can be modified at runtime

    my $nodes = shift;
    die "alternation list is empty" unless ref($nodes) eq 'ARRAY' && @$nodes;
    return sub {
        # my $n = $_[1] || [ 0, 0 ];
        #print "testing alternations on ", Dumper(@_, $nodes);
        # return unless @$nodes;
        my $match;
        my $tail;
        my $state = $_[1] ? [ @{$_[1]} ] : [ 0, 0 ];
        while( defined $state ) {
            #print "alternation string to match: $_[0] - (node,state)=@$state\n";
            ($state->[1], $match, $tail) = 
                $nodes->[ $state->[0] ]->( $_[0], $state->[1] );
            #print "match: ", Dumper( $match );
            if ( ! defined $state->[1] ) {
                $state->[0]++;
                $state->[1] = 0;
                #print "next alternation state: (node,state)=@$state\n";
                $state = undef if $state->[0] > $#$nodes;
            }
            #print "alternate: match \n".Dumper($match) if $match;
            return ( $state, $match, $tail) if $match;
        }
        return;
    }
}

sub ruleop::concat {
    # note: the list in @nodes can NOT be modified at runtime
    return ruleop::concat( +shift, ruleop::concat( @_ ) )
        if @_ > 2;
    my @nodes = @_;
    return sub {
        my $n = $_[1] || [ 0, 0 ];
        my @matches;
        my $tail;
        my $_state;
        my @state = @$n;
        while (1) {
            ($_state, $matches[0], $tail) = $nodes[0]->( $_[0], $state[0] );
            #print "  1st match: ", Dumper( [ $_state, $matches[0], $tail ] );
            if ( ! defined $matches[0] ) {
                return unless defined $_state;
                @state = ( $_state, 0 );
                next;
            }
            ($state[1], $matches[1], $tail) = $nodes[1]->( $tail, $state[1] );
            #print "  2nd match: ", Dumper( [ $_state, $matches[1], $tail ] );
            if ( defined $matches[1] ) {
                my $succ;
                if ( ! defined( $state[1] ) ) {
                    $succ = [ $_state, 0 ] if defined $_state;
                }
                else {
                    $succ = \@state;
                }
                #return ( $succ, { 'concat'=>[ @matches ] }, $tail);
                return ( $succ, \@matches, $tail);
            }
            if ( !defined( $state[1] ) ) {
                return unless defined $_state;
                @state = ( $_state, 0 );
            }
            #print "backtracking: @state\n";
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
    return ruleop::alternation( [ $_[0], ruleop::null() ] );
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
