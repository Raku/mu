package Pugs::Runtime::Regex;

# documentation after __END__

use strict;
use warnings;
use warnings FATAL => qw(recursion);

#use Smart::Comments; #for debugging, look also at Filtered-Comments.pm
use Data::Dumper;
use Pugs::Runtime::Match;

# note: alternation is first match (not longest). 
# note: the list in @$nodes can be modified at runtime
sub alternation {
    my $nodes = shift;
    return sub {
        my @state = $_[1] ? @{$_[1]} : ( 0, undef );
        while ( $state[0] <= $#$nodes ) {
            #print "alternation $state[0] ",Dumper($nodes->[ $state[0] ]);
            $nodes->[ $state[0] ]->( $_[0], $state[1], @_[2..7] );
            last unless defined $_[3];  # test case ???
            $state[1] = $_[3]->state;
            $state[0]++ unless $state[1];
            if ( $_[3] || $_[3]->data->{abort} ) {
                $_[3]->data->{state} = $state[0] > $#$nodes 
                    ? undef
                    : \@state;
                return;
            }
        }
        $_[3] = failed()->(@_);
    }
}

sub concat {
    my $nodes = shift;
    $nodes = [ $nodes, @_ ] unless ref($nodes) eq 'ARRAY';  # backwards compat
    if ( @$nodes > 2 ) {
        return concat(
            concat( [ $nodes->[0], $nodes->[1] ] ),
            @$nodes[ 2 .. $#$nodes ],
        );
    }
    return sub {
        my @state = $_[1] ? @{$_[1]} : ( undef, undef );
        #print "enter state ",Dumper(\@state);
        my $m2;
        do {
            $nodes->[0]->( $_[0], $state[0], @_[2..7] );
            return if ! $_[3] 
                   || $_[3]->data->{abort};
            my $param = { ( defined $_[7] ? %{$_[7]} : () ), 
                          p => $_[3]->to };     
            # TODO - retry the second submatch only, until it fails
            my $next_state = $_[3]->state;
            #print "next_state ",Dumper($next_state);
            $nodes->[1]->( $_[0], $state[1], $_[2], $m2, 
                           $_[4], $_[3]->to, $_[6], $param );
            $state[1] = $m2->state;
            $state[0] = $next_state unless $state[1];
            #print "return state ",Dumper(\@state);
        } while ! $m2 && 
                ! $m2->data->{abort} &&
                defined $state[0]; 

        # push capture data
        # print "Concat positional: ", Dumper( $_[3]->data->{match}, $m2->data->{match} );
        for ( 0 .. $#{ @$m2 } ) {
            if ( ref $m2->[$_] eq 'ARRAY' ) {
                # TODO - fully static count
                # push @{ $_[3]->data->{match}[$_] }, @{ $m2->[$_] };
                $_[3]->data->{match}[$_] = [
                    ( ref( $_[3]->data->{match}[$_] ) eq 'ARRAY' 
                      ? @{ $_[3]->data->{match}[$_] }
                      : defined( $_[3]->data->{match}[$_] ) 
                      ?    $_[3]->data->{match}[$_] 
                      :    () 
                    ), 
                    @{ $m2->[$_] },
                ];
            }
            elsif ( defined $m2->[$_] ) {
                $_[3]->data->{match}[$_] = $m2->[$_];
            }
        }
        #print "Concat named: ", Dumper( $_[3]->data->{named}, $m2->data->{named} );
        for ( keys %{$m2} ) {
            if ( ref $m2->{$_} eq 'ARRAY' ) {
                # TODO - fully static count
                #push @{ $_[3]->data->{named}{$_} }, @{ $m2->{$_} };
                $_[3]->data->{named}{$_} = [
                    ( ref( $_[3]->data->{named}{$_} ) eq 'ARRAY'
                      ? @{ $_[3]->data->{named}{$_} }
                      : defined( $_[3]->data->{named}{$_} ) 
                      ?    $_[3]->data->{named}{$_} 
                      :    () 
                    ),
                    @{ $m2->{$_} },
                ];
            }
            elsif ( defined $m2->{$_} ) {
                $_[3]->data->{named}{$_} = $m2->{$_};
            }
        }
        # /push capture data

        %{$_[3]->data} = (
                %{$_[3]->data},
                bool    => \($m2->bool),
                to      => \($m2->to),
                capture => $m2->data->{capture},
                abort   => $m2->data->{abort},
                state   => ( defined $state[0] || defined $state[1] 
                             ? \@state 
                             : undef ),
        );
    }
}

sub constant { 
    my $const = shift;
    my $lconst = length( $const );
    no warnings qw( uninitialized );
    return sub {
        my $bool = $const eq substr( $_[0], $_[5], $lconst );
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \$bool,
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \($_[5] + $lconst),
                named => {},
                match => [],
            });
    }
}

sub perl5 {
    my $rx = qr(^($_[0]))s;
    no warnings qw( uninitialized );
    return sub {
        my $bool = substr( $_[0], $_[5] ) =~ m/$rx/;
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \$bool,
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \($_[5] + length $1),
                named => {},
                match => [],
            });
    };
}

sub null {
    no warnings qw( uninitialized );
    return sub {
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \1,
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \(0 + $_[5]),
                named => {},
                match => [],
            });
    }
};

sub failed {
    no warnings qw( uninitialized );
    return sub {
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \0,
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \(0 + $_[5]),
                named => {},
                match => [],
            });
    }
};

sub failed_abort {
    no warnings qw( uninitialized );
    return sub {
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \0,
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \(0 + $_[5]),
                named => {},
                match => [],
                abort => 1,
            });
    }
};

sub named {
    # return a named capture
    my $label = shift;
    my $capture_to_array = shift;  
    my $node = shift;
    sub {
        my $match;
        $node->( @_[0,1,2], $match, @_[4,5,6,7] );
        my %matches;
        $matches{ $label } = $capture_to_array ? [ $match ] : $match;
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \( $match->bool ),
                str   => \$_[0],
                from  => \( $match->from ),
                to    => \( $match->to ),
                named => \%matches,
                match => [],
                capture => $match->data->{capture},
                state => $match->state,
            });
    }
}
sub capture { named(@_) } # backwards compat

sub positional {
    # return a positional capture
    my $num = shift;  
    my $capture_to_array = shift;  
    my $node = shift;
    sub {
        my $match;
        $node->( @_[0,1,2], $match, @_[4,5,6,7] );
        my @matches;
        $matches[ $num ] = $capture_to_array ? [ $match ] : $match;
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \( $match->bool ),
                str   => \$_[0],
                from  => \( $match->from ),
                to    => \( $match->to ),
                named => {},
                match => \@matches,
                capture => $match->data->{capture},
                state => $match->state,
            });
    }
}

sub ___abort { 
    my $op = shift;
    return sub {
        print "ABORTING\n";
        $op->( @_ );
        print "ABORT: [0] ",Dumper(@_);  #$_[3]->perl;
        $_[3]->data->{abort} = 1;
        print "ABORT: ",$_[3]->perl;
    };
};

sub ___fail { 
    my $op = shift;
    return abort( 
        sub {
            print "FAILING\n";
            $op->( @_ );
            $_[3]->data->{bool} = \0;
            print "FAIL: ",Dumper( $_[3] );
        } 
    );
};

sub before { 
    my $op = shift;
    return sub {
        my $match;
        $op->( @_[0,1,2], $match, @_[4,5,6,7] );
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \( $match->bool ),
                str   => \$_[0],
                from  => \( $match->from ),
                to    => \( $match->from ),
                named => {},
                match => [],
                state => $match->state,
            });
    };
}

sub at_start {
    no warnings qw( uninitialized );
    return sub {
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \( $_[5] == 0 ),
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \(0 + $_[5]),
                named => {},
                match => [],
                abort => 0,
            });
    }
};

sub at_line_start {
    no warnings qw( uninitialized );
    return sub {
        my $bool = $_[5] == 0
            ||  substr( $_[0], 0, $_[5] ) =~ /\n$/s;
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \$bool,
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \(0 + $_[5]),
                named => {},
                match => [],
                abort => 0,
            });
    }
};

sub at_line_end {
    no warnings qw( uninitialized );
    return sub {
        my $bool = $_[5] >= length( $_[0] )
            ||  substr( $_[0], $_[5] ) =~ /^\n/s;
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \$bool,
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \(0 + $_[5]),
                named => {},
                match => [],
                abort => 0,
            });
    }
};

sub at_end_of_string {
    no warnings qw( uninitialized );
    return sub {
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \( $_[5] == length( $_[0] ) ),
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \(0 + $_[5]),
                named => {},
                match => [],
                abort => 0,
            });
    }
};

# ------- higher-order ruleops

sub optional {
    my $node = shift;
    alternation( [ $node, null() ] );
}

sub null_or_optional {
    my $node = shift;
    alternation( [ null(), $node ] );
}

sub greedy_plus { 
    my $node = shift;
    my $alt;
    $alt = concat( [
        $node, 
        optional( sub{ goto $alt } ),  
    ] );
    return $alt;
}

sub greedy_star { 
    my $node = shift;
    optional( greedy_plus( $node ) );
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
        my $state = $_[1] || $node;
        $state->( $_[0], undef, @_[2..7] );
        $_[3]->data->{state} = concat( [ $node, $state ] );
    }
}


sub _preprocess_hash {
    my $h = shift;
    #print "hash: ",ref($h),"\n";
    if ( ref($h) eq 'CODE') {
        return sub {
            $h->();
            return null()->(@_);
        };
    } 
    if ( ref($h) =~ /^Pugs::Compiler::/ ) {
        #print "compiling subrule\n";
        #return $h->code;
        return sub { 
            #print "into subrule - $_[0] - grammar $_[4] - ", Dumper($_[7]); 
            #print $h->{perl5};
            my $match = $h->match( $_[0], $_[4], $_[7], $_[1] );
            #print "match: ",$match->(),"\n";
            $_[3] = $match;
        };
    }
    # fail is number != 1 
    if ( $h =~ /^(\d+)$/ ) {
        return failed unless $1 == 1;
        return null;
    }
    # subrule
    warn "uncompiled subrule: $h - not implemented";
    return failed;
}

# see commit #9783 for an alternate implementation
sub hash {
    my %hash = %{shift()};
    #print "HASH: @{[ %hash ]}\n";
    my @keys = sort {length $b <=> length $a } keys %hash;
    #print "hash keys [ @keys ]\n";
    @keys = map {
        concat( [
            constant( $_ ),
            _preprocess_hash( $hash{$_} ),
        ] )
    } @keys;
    return alternation( \@keys );
}

# not a 'rule node'
# gets a variable from the user's pad
# this is used by the <$var> rule
sub get_variable {
    my $name = shift;
    
    local $@;
    my($idx, $pad) = 0;
    while(eval { require PadWalker; $pad = PadWalker::peek_my($idx) }) {
        $idx++, next
          unless exists $pad->{$name};

        #print "NAME $name $pad->{$name}\n";
        return ${ $pad->{$name} } if $name =~ /^\$/;
        return $pad->{$name};  # arrayref/hashref
    }
    die "Couldn't find '$name' in surrounding lexical scope.";
}


1;

__END__

=for About

Original file: pX/Common/iterator_engine.pl - fglock

TODO

- There are no tests yet for <before>, hashes, end_of_string

- It needs a 'direction' flag, in order to implement <after>.

- Quantified matches could use less stack space.

- Simplify arg list - the functions currently take 8 arguments.

- weaken self-referential things

=cut

=pod

A "rule" function gets as argument a list:

0 - the string to match 
1 - an optional "continuation"
2 - the partially built match tree
3 - a leaf pointer in the match tree
4 - the grammar name
5 - pos 
#6 - the whole string to match 
7 - argument list - <subrule($x,$y)>

it modifies argument #3 to a Match object:

    bool  - an "assertion" (true/false)
    from  - string pointer for start of this match
    to    - string pointer for next match (end+1)
    match - positional submatches
    named - named submatches
    capture - return'ed things
    
    state - a "continuation" or undef
    abort - the match was stopped by a { return } or a fail(),
           and it should not backtrack or whatever

A "ruleop" function gets some arguments and returns a "rule" funtion.

=cut

=for later
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

=cut

=for later

sub fail { 
    return abort( 
        sub {
            return { bool => \0 };
        } 
    );
};

# experimental!
sub negate { 
    my $op = shift;
    return sub {
        #my $str = $_[0];
        my $match = $op->( @_ );
        return if $match->{bool};
        return { bool => \1,
                 #tail => $_[0],
               }
    };
};
=cut

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

=for later
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
=cut

