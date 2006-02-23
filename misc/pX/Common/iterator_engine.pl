# pX/Common/iterator_engine.pl - fglock
#

use strict;
use warnings;

=pod

A "rule" function gets as argument:

- a string to match
- an optional "continuation"
- an optional "flags" hashref
    'capture'=>1 means 'return whatever matches'

it returns (or "yields") a hash containing:

    state - a "continuation" or undef
    bool - an "assertion" (true/false)
    match - the "match" tree or undef
    tail - the string tail or undef
    capture - the tree of captured things

Continuations are used for backtracking.

A "ruleop" function gets some arguments and returns a "rule".

=cut

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

    # XXX - needs optimization for faster backtracking, less stack usage

    return sub {
        my $tail =  $_[0];
        my $state = $_[1] || { state => undef, op => $node };
        my $flags = $_[2];

        # XXX - didn't work
        # my $match = $state->{op}->( $tail, $state->{state}, $flags ); 

        my $match = $state->{op}->( $tail, undef, $flags );
        return unless $match->{bool};
        $match->{state} = {
            state => $match->{state},
            op    => ruleop::concat( $node, $state->{op} ),
        };
        return $match;
    }
}

sub ruleop::alternation {
    # alternation is first match (not longest).  though we need a 
    # separate longest match for tokens (putter on #perl6)

    # note: the list in @$nodes can be modified at runtime

    my $nodes = shift;
    die "alternation list is empty" unless ref($nodes) eq 'ARRAY' && @$nodes;
    return sub {
        #print "testing alternations on ", Dumper(@_, $nodes);
        # return unless @$nodes;

        my $tail =  $_[0];
        my $state = $_[1] ? [ @{$_[1]} ] : [ 0, 0 ];
        my $flags = $_[2];
        my $match;
        while ( defined $state ) {
            #print "alternation string to match: $tail - (node,state)=@$state\n";
            $match = 
                $nodes->[ $state->[0] ]->( $tail, $state->[1], $flags );
            #print "match: ", Dumper( $match );
            if ( $match->{state} ) {
                $state->[1] = $match->{state};
            }
            else
            {
                $state->[0]++;
                $state->[1] = 0;
                #print "next alternation state: (node,state)=@$state\n";
                $state = undef if $state->[0] > $#$nodes;
            }
            $match->{state} = $state;
            #print "alternate: match \n".Dumper($match) if $match->{bool};
            return $match if $match->{bool};
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
        my $tail  = $_[0];
        my @state = $_[1] ? ( @{$_[1]} ) : ( 0, 0 );
        my $flags = $_[2];
        my @matches;
        while (1) {
            $matches[0] = $nodes[0]->( $tail, $state[0], $flags );
            #print "  1st match: ", Dumper( $matches[0] );
            if ( ! $matches[0]{bool} ) {
                return unless defined $matches[0]{state};
                @state = ( $matches[0]{state}, 0 );
                next;
            }
            $matches[1] = $nodes[1]->( $matches[0]{tail}, $state[1], $flags );
            #print "  2nd match: ", Dumper( $matches[1] );
            if ( $matches[1]{bool} ) {
                my $succ;
                if ( ! defined( $matches[1]{state} ) ) {
                    $succ = [ $matches[0]{state}, 0 ] if defined $matches[0]{state};
                }
                else {
                    $succ = [ $state[0], $matches[1]{state} ];
                }

                my $capture = [];
                # print "capture: ", Dumper( $matches[0]{capture}, $matches[1]{capture} );
                $capture = $matches[0]{capture} 
                    if $matches[0]{capture};
                push @$capture, @{$matches[1]{capture}} 
                    if $matches[1]{capture};
                undef $capture unless @$capture;

                return { 
                    bool =>  1,
                    match => [ @matches ], 
                    tail =>  $matches[1]{tail},
                    state => $succ,
                    capture => $capture,
                };
            }
            if ( ! defined( $matches[1]{state} ) ) {
                return unless defined $matches[0]{state};
                @state = ( $matches[0]{state}, 0 );
            }
            # print "backtracking: state ",Dumper(@state);
            # print "backtracking: match ",Dumper(@matches);
            # die;
        }
    }
}

sub ruleop::constant { 
    my $const = shift;
    return sub {
        my $flags = $_[2];
        return unless $_[0] =~ m/^(\Q$const\E)(.*)/s;
        return { bool => 1,
                 match => { constant => $1 }, 
                 capture => [ $1 ], 
                 tail => $2,
               }
           if $flags->{capture};
        return { bool => 1,
                 match => { constant => $1 }, 
                 tail => $2,
               }
    }
}

sub ruleop::optional {
    return ruleop::alternation( [ $_[0], ruleop::null() ] );
}

sub ruleop::null {
    return sub {
        return { bool => 1,
                 match => 'null',
                 ( $_[2]->{capture} ? ( capture => [ '' ] ) : () ),
                 tail => $_[0],
               }
    }
};

sub ruleop::capture {
    # sets the 'capture' flag and return a labeled capture
    # XXX - generalize to: set_flag('capture',1)
    my $label = shift;
    my $node = shift;
    sub {
        my @param = @_;
        $param[2] = {} unless defined $param[2];
        $param[2] = { %{$param[2]}, capture=>1 };
        my $match = $node->( @param );
        return unless $match;
        my $new_match = { %$match };
        $new_match->{capture} = [ { $label => $match->{capture} } ];
        return $new_match;
    }
}

1;
