package Pugs::Runtime::Rule;

# pX/Common/iterator_engine.pl - fglock
#
use strict;
use warnings;
#use Smart::Comments; for debugging, look also at Filtered-Comments.pm
use Data::Dump::Streamer;
use PadWalker qw( peek_my );  # peek_our ); ???

=pod

A "rule" function gets as argument a list:

0 - a string to match 
1 - an optional "continuation"
2 - a partially built match tree
3 - a leaf pointer in the match tree
4 - a grammar name
5 - pos - TODO - change #0 ???
6 - the whole string to match - TODO - unify with $_[0]
7 - argument list - <subrule($x,$y)>

it returns (or "yields"):

    undef - match failed

or a hash containing: - TODO - no longer needed

    state - a "continuation" or undef
    bool - an "assertion" (true/false)
    match - the "match" tree or undef
    tail - the string tail or undef - TODO - don't need this if 'pos' is used
    capture - the tree of captured things
    abort - the match was stopped by a { return } or a fail(),
           and it should not backtrack or whatever
    return - the block (sub-reference) that contains 'return'
    label - the capture name (ident or '')

Continuations are used for backtracking.

A "ruleop" function gets some arguments and returns a "rule".

=cut

# XXX - optimization - pass the string index around, 
# XXX   instead of copying the whole string to $tail every time

# XXX - weaken self-referential things

sub alternation {
    # alternation is first match (not longest).  though we need a 
    # separate longest match for tokens (putter on #perl6)
    # update: <%var> does longest match based on the keys length() (TimToady on #perl6)

    # note: the list in @$nodes can be modified at runtime

    my $nodes = shift;
    # die "alternation list is empty" unless ref($nodes) eq 'ARRAY' && @$nodes;
    return sub {
        ### testing alternations on : @_, $nodes
        return unless @$nodes;

        my $tail =  $_[0];
        my $state = $_[1] ? [ @{$_[1]} ] : [ 0, 0 ];
        $_[3] = {};

        my $match;
        while ( defined $state ) {
            ### alternation string to match: "$tail - (node,state)=@$state"
            $match = 
                $nodes->[ $state->[0] ]->( $tail, $state->[1], $_[2], $_[3]{match}, @_[4,5,6,7] );
            $match = $match->data if ref($match) eq 'Pugs::Runtime::Match';
            ### match: $match
            if ( $match->{state} ) {
                $state->[1] = $match->{state};
            }
            else
            {
                $state->[0]++;
                $state->[1] = 0;
                ### next alternation state - (node,state):@$state
                $state = undef if $state->[0] > $#$nodes;
            }
            $match->{state} = $state;
            return $_[3] = $match if $match->{bool} || $match->{abort};
        }
        undef $_[3];
        return;
    }
}

sub concat {
    
    # note: the list in @nodes can NOT be modified at runtime
    # update: this is ok, because we can use <$var><$var> instead
    
    return concat( +shift, concat( @_ ) )
        if @_ > 2;
    my @nodes = @_;
    return sub {
        my $tail  = $_[0];
        my @state = $_[1] ? ( @{$_[1]} ) : ( 0, 0 );
        my @matches;
        $_[3] = { match => [] };
        while (1) {
            
            $matches[0] = $nodes[0]->( $tail, $state[0], $_[2], $_[3]{match}[0], @_[4,5,6,7] );
            $matches[0] = $matches[0]->data if ref($matches[0]) eq 'Pugs::Runtime::Match';
            ### 1st match: $matches[0]
            return $_[3] = $matches[0] 
                if $matches[0]{abort};
            if ( ! $matches[0]{bool} ) {
                return unless defined $matches[0]{state};
                @state = ( $matches[0]{state}, 0 );
                next;
            }

            $_[3]{capture} = $_[3]{match}[0]{capture};
            #print "Matched concat 0, tree:", Dump($_[2]);

            $matches[1] = $nodes[1]->( $matches[0]{tail}, $state[1], $_[2], $_[3]{match}[1], @_[4,5,6,7] );
            $matches[1] = $matches[1]->data if ref($matches[1]) eq 'Pugs::Runtime::Match';
            ### 2nd match: $matches[1]
            if ( ! $matches[1]{abort} ) {
                # die "not implemented";
                # XXX - untested (use ':' to test this)
                # XXX - what to do with $matches[0] ???
                #return $_[3] = $matches[1];
            #}
            
            if ( ! $matches[1]{bool} ) {
                if ( ! defined( $matches[1]{state} ) ) {
                    return unless defined $matches[0]{state};
                    @state = ( $matches[0]{state}, 0 );
                }
                ### backtracking - state: @state
                ### backtracking - match: @matches
                next;
            }
            
            }
            #print "Matched concat 1, tree:", Dump($_[2]) if defined $_[2];

            my $succ;
            if ( ! defined( $matches[1]{state} ) ) {
                $succ = [ $matches[0]{state}, 0 ] if defined $matches[0]{state};
            }
            else {
                $succ = [ $state[0], $matches[1]{state} ];
            }

            # XXX - cleanup!
            my $match2 = { %{$matches[1]} };

            if ( defined $match2->{tail} ) {
                my $len = length( $match2->{tail} );
                my $head = $len?substr($_[0], 0, -$len):$_[0];
                $match2->{capture} = $head;  
            }
            else {
                $match2->{capture} = $_[0];
            }

            $match2->{match} = \@matches;
            $match2->{state} = $succ;
            delete $match2->{label};
            #delete $matches[1]{abort};
            delete $matches[1]{return};

            # print "concat: ",Dump( $match2 );

            return $_[3] = $match2;
        }
    }
}

sub constant { 
    my $const = shift;
    my $lconst = length( $const );
    no warnings qw( uninitialized );
    return sub {
        #print "Pos: $_[5] - ", length($_[6])-length($_[0]), "\n";
        #print "Runtime.constant: $const \n";
        if ( $const eq substr( $_[0], $_[5], $lconst ) ) {
            return $_[3] = { 
                bool => 1,
                match => $const, 
                tail => substr( $_[0], $_[5]+$lconst ),
            };
        }
        return $_[3] = { bool => 0 };
    }
}

sub perl5 {
    my $rx = qr(^($_[0])(.*)$)s;
    no warnings qw( uninitialized );
    return sub {
        if ( $_[0] =~ m/$rx/ ) {
            return $_[3] = { 
                bool  => 1,
                match => $1,
                tail  => $2,
            };
        }
        return $_[3] = { bool => 0 };
    };
}

sub null {
    return sub {
        return $_[3] = { 
            bool => 1,
            tail => $_[0],
        };
    }
};

sub capture {
    # return a labeled capture
    my $label = shift;
    my $node = shift;
    sub {
        $_[3] = { label => $label };
        my $match = $node->( @_[0,1,2], $_[3]{match}, @_[4,5,6,7] );
        $match = $match->data if ref($match) eq 'Pugs::Runtime::Match';
        return unless $match->{bool};
        ## return if $match->{abort}; - maybe a { return }
        my $new_match = { %$match };
        
        $new_match->{label}   = $label;
    
        if ( ! defined $new_match->{capture} ) {
            if ( defined $match->{tail} ) {
                my $len = length( $match->{tail} );
                my $head = $len?substr($_[0], 0, -$len):$_[0];
                $new_match->{capture} = $head;   # XXX -- array ref not needed
                #print 'got capture: ',do{use Data::Dump::Streamer; Dump($new_match)};
            }
            else {
                $new_match->{capture} = $_[0];
            }
        }
        $new_match->{match}   = $match ;  # XXX - workaround

        # print "Capturing ", Dump($_[2]);

        return $_[3] = $new_match;
    }
}

# experimental!
sub try { 
    my $op = shift;
    return sub {
        my $match = $op->( @_ );
        ### abortable match...
        $match->{abort} = 0;
        return $match;
    };
};

# experimental!
sub abort { 
    my $op = shift;
    return sub {
        #print __PACKAGE__ . "->abort\n";
        #print caller;
        my $match = $op->( @_ );
        ### aborting match: $match
        $match->{abort} = 1;
        return $match;
    };
};

sub fail { 
    return abort( 
        sub {
            return { bool => 0 };
        } 
    );
};

# experimental!
sub negate { 
    my $op = shift;
    return sub {
        #my $tail = $_[0];
        my $match = $op->( @_ );
        return if $match->{bool};
        return { bool => 1,
                 tail => $_[0],
               }
    };
};

sub before { 
    my $op = shift;
    return sub {
        my $match = $op->( @_ );
        #return $match unless $match->{bool};
        return { bool => $match->{bool},
                 tail => $_[0],
               }
    };
};

# experimental!
=for example
    # adds an 'before' or 'after' sub call, which may print a debug message 
    wrap( { 
            before => sub { print "matching variable: $_[0]\n" },
            after  => sub { $_[0]->{bool} ? print "matched\n" : print "no match\n" },
        },
        \&variable
    )
=cut
sub wrap {
    my $debug = shift;
    my $node = shift;
    sub {
        $debug->{before}( @_ ) if $debug->{before};
        my $match = $node->( @_ );
        $debug->{after}( $match, @_ ) if $debug->{after};
        return $match;
    }
}


# ------- higher-order ruleops

sub optional {
    return alternation( [ $_[0], null() ] );
}

sub null_or_optional {
    return alternation( [ null(), $_[0] ] );
}

sub greedy_plus { 
    my $node = shift;
    my $alt;
    $alt = concat( 
        $node, 
        optional( sub{ goto $alt } ),  
    );
    return $alt;
}

sub greedy_star { 
    my $node = shift;
    return optional( greedy_plus( $node ) );
}

sub non_greedy_star { 
    my $node = shift;
    alternation( [ 
        null(),
        non_greedy_plus( $node ) 
    ] );
}

sub non_greedy_plus { 
    my $node = shift;

    # XXX - needs optimization for faster backtracking, less stack usage

    return sub {
        my $tail =  $_[0];
        my $state = $_[1] || { state => undef, op => $node };
        #my $flags = $_[2];

        # XXX - didn't work
        # my $match = $state->{op}->( $tail, $state->{state}, $flags ); 

        my $match = $state->{op}->( $tail, undef, $_[2], $_[3]{match}, @_[4..6] );
        return unless $match->{bool};
        $match->{state} = {
            state => $match->{state},
            op    => concat( $node, $state->{op} ),
        };
        return $match;
    }
}

# interface to the internal rule functions
# - creates a 'capture', unless it detects a 'return block'
sub rule_wrapper {
    my ( $str, $match ) = @_;
    $match = $match->data if ref($match) eq 'Pugs::Runtime::Match';
    return unless $match->{bool};
    if ( $match->{return} ) {
        #warn 'pre-return: ', Dump( $match );
        my %match2 = %$match;
        $match2{capture} = $match->{return}( 
            Pugs::Runtime::Match->new( $match ) 
        );
        #warn "return ",ref($match2{capture});
        #warn 'post-return: ', Dump( $match2{capture} );
        delete $match->{return};
        delete $match->{abort};
        delete $match2{return};
        delete $match2{abort};
        #warn "Return Object: ", Dump( \%match2 );
        return \%match2;
    }
    #warn "Return String";
    # print Dump( $match );
    my $len = length( $match->{tail} );
    my $head = $len ? substr($str, 0, -$len) : $str;
    $match->{capture} = $head;
    delete $match->{abort};
    return $match;
}

# not a 'rule node'
# gets a variable from the user's pad
# this is used by the <$var> rule
sub get_variable {
    my $name = shift;
    
    local $@;
    my($idx, $pad) = 0;
    while(eval { $pad = peek_my($idx) }) {
        $idx++, next
          unless exists $pad->{$name};

        #print "NAME $name $pad->{$name}\n";
        return ${ $pad->{$name} } if $name =~ /^\$/;
        return $pad->{$name};  # arrayref/hashref
    }
    die "Couldn't find '$name' in surrounding lexical scope.";
}

sub _preprocess_hash {
    my $h = shift;
    if ( ref($h) eq 'CODE') {
        return sub {
            $h->();
            return { bool => 1, match => '', tail => $_[0] };
        };
    } 
    if ( UNIVERSAL::isa( $h, 'Pugs::Compiler::Regex') ) {
        #print "compiling subrule\n";
        #return $h->code;
        return sub { 
            #print "into subrule - $_[0] - grammar $_[4]\n"; 
            #print $h->code;
            my $match = $h->match( $_[0], $_[4], { p => 1 } );
            #print "match: ",$match->(),"\n";
            return $_[3] = $match->data;
        };
    }
    # fail is number != 1 
    if ( $h =~ /^(\d+)$/ ) {
        return sub{} unless $1 == 1;
        return sub{ { bool => 1, match => '', tail => $_[0] } };
    }
    # subrule
    warn "uncompiled subrule: $h - not implemented";
    return sub {};
}

# see commit #9783 for an alternate implementation
sub hash {
    my %hash = %{shift()};
    #print "HASH: @{[ %hash ]}\n";
    my @keys = sort {length $b <=> length $a } keys %hash;
    #print "hash keys: @keys\n";
    @keys = map {
        concat(
            constant( $_ ),
            _preprocess_hash( $hash{$_} ),
        )
    } @keys;
    return alternation( \@keys );
}

sub end_of_string {
    return sub {
        return $_[3] = { 
            bool  => ($_[0] eq ''),
            match => '',
            tail  => $_[0],
        };
    };
}

1;
