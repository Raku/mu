package Pugs::Runtime::Regex;

# documentation after __END__

use strict;
use warnings;
no warnings qw(recursion);

use Data::Dumper;
use Pugs::Runtime::Match;
use Carp qw(croak);

# note: alternation is first match (not longest). 
# note: the list in @$nodes can be modified at runtime
# Closure data for alternation:
#   $nodes => array of regex-node
# State structure for alternation is an array:
#   0 - current node index in @$nodes
#   1 - continuation for the current node
sub alternation {
    my $nodes = shift;
    return sub {
        my ($index, $state) = defined $_[1] ? @{$_[1]} : ( 0, undef );
        $nodes->[$index]->( $_[0], 
                            $state, 
                            @_[2..7],
                          );
        $state = $_[3]->state;
        $index++ if !defined($state);
        $_[3]->data->{state} = $index > $#$nodes || $_[3]->data->{abort}
            ? undef     
            : [$index, $state];
    }
}

# note: parallel_alternation is longest match. 
# Closure data for parallel_alternation:
#   $nodes => array of regex-node
# State structure for alternation is an array of "threads". Each thread is a hash:
#   id      - thread id. Index to a regex node in @$node
#   state   - state of this thread
#   match   - the running match object for this thread
#   running - is the thread still running
sub parallel_alternation {
    my $nodes = shift;
    return sub {
        my @threads = defined $_[1] 
            ? grep { defined $_ } @{$_[1]} 
            : map { { id    => $_, 
                      state => undef, 
                      match => ( defined $_[3] ? $_[3]->clone : undef ), 
                      running => 1,
                  } } ( 0 .. $#$nodes );
        if ( !defined $_[3] ) {
            $_[3] = Pugs::Runtime::Match->new({ bool => \0, });
        }
        my $all_done = 1;
        for my $t ( @threads ) {
            next unless $t->{running};
            $nodes->[$t->{id}]->( $_[0], 
                                  $t->{state}, 
                                  $_[2],
                                  $t->{match},
                                  @_[4..7],
                                );
            $t->{state} = $t->{match}->state;
            $t->{running} = 0 if !defined $t->{state};
            $all_done = 0 if $t->{running};
            if ( $t->{match}->data->{abort} ) {
                $_[3]->data->{state} = undef;
                return;
            }
        }
        $_[3]->data->{state} = \@threads;
        if ( $all_done ) {
            # longest-token
            # TODO: backtracking to second-longest ???
            my $max = -1;
            for my $t ( @threads ) {
                $_[3] = $t->{match} if $t->{match} && $t->{match}->to > $max;
            }
        }
        return;
    }
}

# Closure data for concat:
#   $nodes => array of regex-node
# State structure for concat is a hash:
#   cont0 - continuation for the first node
#   cont1 - continuation for the second node
#   index - current node index (0 or 1) in @$nodes
sub concat {
    my $nodes = shift;
    $nodes = [ $nodes, @_ ] unless ref($nodes) eq 'ARRAY';  # backwards compat
    return null()      if ! @$nodes;
    return $nodes->[0] if @$nodes == 1;
    if ( @$nodes > 2 ) {
        return concat(
            concat( [ $nodes->[0], $nodes->[1] ] ),
            @$nodes[ 2 .. $#$nodes ],
        );
    }
    return sub {
        my %state = $_[1] ? %{$_[1]} : ( cont0 => undef, cont1 => undef, index => 0, pos => undef );        
        
        if ( $state{index} == 0 ) {
            $nodes->[0]->( $_[0], 
                           $state{cont0}, 
                           @_[2..7],
                         );
            $state{cont0} = $_[3]->state;
            if ( $_[3] && ! $_[3]->data->{abort} ) {
                $state{index} = 1;
                $state{cont1} = undef;
                $state{pos}   = $_[3]->to;
            }
            $_[3]->data->{state} = \%state;
            return;
        }

        my $m1;
        $nodes->[1]->( $_[0], $state{cont1}, 
                       $_[2], $m1, 
                       $_[4], $state{pos},  
                       $_[6], 
                       { ( defined $_[7] ? %{$_[7]} : () ), p => $state{pos}, } 
                     );
        $state{cont1} = $m1->state;
        if ( defined $state{cont1} ) {
            $_[3]->data->{state} = \%state;
        }
        elsif ( defined $state{cont0} ) {
            $state{index} = 0;  # backtrack
            $_[3]->data->{state} = \%state;
        }
        else {
            $_[3]->data->{state} = undef;
        }

        # bool==false and abort are handled automatically...
        
        # push capture data
        # print "Concat positional: ", Dumper( $_[3]->data->{match}, $m1->data->{match} );
        for ( 0 .. $#{ $m1 } ) { 
            if ( ref $m1->[$_] eq 'ARRAY' ) {
                # TODO - fully static count
                # push @{ $_[3]->data->{match}[$_] }, @{ $m1->[$_] };
                $_[3]->data->{match}[$_] = [
                    ( ref( $_[3]->data->{match}[$_] ) eq 'ARRAY' 
                      ? @{ $_[3]->data->{match}[$_] }
                      : defined( $_[3]->data->{match}[$_] ) 
                      ?    $_[3]->data->{match}[$_] 
                      :    () 
                    ), 
                    @{ $m1->[$_] },
                ];
            }
            elsif ( defined $m1->[$_] ) {
                $_[3]->data->{match}[$_] = $m1->[$_];
            }
        }
        #print "Concat named: ", Dumper( $_[3]->data->{named}, $m1->data->{named} );
        for ( keys %{$m1} ) {
            if ( ref $m1->{$_} eq 'ARRAY' ) {
                # TODO - fully static count
                #push @{ $_[3]->data->{named}{$_} }, @{ $m1->{$_} };
                $_[3]->data->{named}{$_} = [
                    ( ref( $_[3]->data->{named}{$_} ) eq 'ARRAY'
                      ? @{ $_[3]->data->{named}{$_} }
                      : defined( $_[3]->data->{named}{$_} ) 
                      ?    $_[3]->data->{named}{$_} 
                      :    () 
                    ),
                    @{ $m1->{$_} },
                ];
            }
            elsif ( defined $m1->{$_} ) {
                $_[3]->data->{named}{$_} = $m1->{$_};
            }
        }
        # /push capture data

        %{$_[3]->data} = (
                %{$_[3]->data},
                bool    => \($m1->bool),
                to      => \($m1->to),
                capture => $m1->data->{capture} || $_[3]->data->{capture},
                abort   => $m1->data->{abort},
                state   => ( defined $state{cont0} || defined $state{cont1} )
                    ? \%state
                    : undef,   
        );

        return;
    }
}

=for xxx
sub xxx {

            my $is_empty = ( $_[3]->from == $_[3]->to );
            #    && ( $param1{was_empty} )
            #    ; # fix a problem with '^'
            if ( $is_empty && $param1{was_empty} ) {
                #    # perl5 perlre says "the following match after a zero-length match
                #   is prohibited to have a length of zero"
                return unless $_[3]->from == 0;
            }
            
        # push capture data
        # print "Concat positional: ", Dumper( $_[3]->data->{match}, $m2->data->{match} );
        for ( 0 .. $#{ $m2 } ) { 
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
                capture => $m2->data->{capture} || $_[3]->data->{capture},
                abort   => $m2->data->{abort},
                state   => ( defined $state[0] || defined $state[1] 
                             ? \@state 
                             : undef ),
        );
    }
}
=cut

sub try_method { 
    my $method = shift;
    my $param_list = shift;  # XXX
    no warnings qw( uninitialized );
    # XXX method call must be inlined, due to inheritance problems
    my $sub = 'sub {
        my $bool = $_[0]->'.$method.'( '.$param_list.' ) ? 1 : 0;
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \$bool,
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \(0 + $_[5]),
                named => {},
                match => [],
            });
    }';
    #print "sub: $sub\n";
    return eval $sub;
}


sub ignorecase { 
    my $sub = shift;
    no warnings qw( uninitialized );
    return sub {
        my %param = ( ( defined $_[7] ? %{$_[7]} : () ), ignorecase => 1 );
        $sub->( @_[0..6], \%param );
    }
}

sub constant { 
    my $const = shift;
    my $lconst = length( $const );
    no warnings qw( uninitialized );
    return sub {
        my $bool = $_[7]{ignorecase}
            ? lc( $const ) eq lc( substr( $_[0], $_[5], $lconst ) )
            : $const eq substr( $_[0], $_[5], $lconst );
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
    my $rx;
    no warnings qw( uninitialized );
    { 
        local $@;
        $rx = eval " use charnames ':full'; qr(^($_[0]))s ";
        #print "regex perl5<< $_[0] >>\n";
        print "Error in perl5 regex: << $_[0] >> \n$@\n"
            if $@;
        #die "Error in perl5 regex: $_[0]"
        #    if $@;
    }
    return sub {
        #use charnames ':full';
        my $bool;
        eval {
            $bool = $_[7]{ignorecase}
                ? substr( $_[0], $_[5] ) =~ m/(?i)$rx/
                : substr( $_[0], $_[5] ) =~ m/$rx/;
            $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \$bool,
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \($_[5] + length $1),
                named => {},
                match => [],
            });
            1;
        } 
        or do {
            die "$@ in perl5 regex: /$rx/";
        };
        $_[3];
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
                state => undef,
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

sub capture_as_result {
    # return a capture as the result object
    my $node = shift;
    sub {
        my $match;
        $node->( @_[0,1,2], $match, @_[4,5,6,7] );
        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \( $match->bool ),
                str   => \$_[0],
                from  => \( $match->from ),
                to    => \( $match->to ),
                named => {},
                match => [],
                capture => ( 
                    sub {
                        # print "Match: ", Dumper( $match );
                        '' . $match 
                    } 
                ),
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

# experimental!
sub negate { 
    my $op = shift;
    return sub {
        #my $str = $_[0];
        my $match = $op->( @_ );
        my $bool = ! $match;

        $_[3] = Pugs::Runtime::Match->new({ 
                bool  => \( $bool ),
                str   => \$_[0],
                from  => \(0 + $_[5]),
                to    => \(0 + $_[5]),
                named => {},
                match => [],
                abort => 0,
            });

    };
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

sub greedy_star { 
    greedy_plus( $_[0], $_[1] || 0, $_[2] ) 
}

sub non_greedy_star { 
    non_greedy_plus( $_[0], $_[1] || 0, $_[2] ) 
}

# XXX - needs optimization for faster backtracking, less stack usage
# TODO - run-time ranges (iterator)
sub greedy_plus { 
    my $node = shift;
    my $min_count = defined( $_[0] ) ? $_[0] : 1;
    my $max_count = $_[1];  
    if (  defined $max_count 
       && $max_count < 1e99
       ) {
        return concat( [ 
            ( $node )             x $min_count, 
            ( optional( $node ) ) x ($max_count - $min_count) 
        ] );
    }
    # $max_count == infinity
    my $alt;
    $alt = concat( [
        $node, 
        optional( sub{ goto $alt } ),  
    ] );
    return optional( $alt ) if $min_count < 1;
    return concat( [ ( $node ) x ($min_count - 1), $alt ] );
}

# XXX - needs optimization for faster backtracking, less stack usage
# TODO - run-time ranges (iterator)
sub non_greedy_plus { 
    my $node = shift;
    my $min_count = defined( $_[0] ) ? $_[0] : 1;
    my $max_count = $_[1] || 1e99;
    return sub {
        my $state = $_[1] 
            || { node  => concat( [ ( $node ) x $min_count ] ), 
                 count => $min_count };
        return failed()->(@_)
            if $state->{count} > $max_count;
        $state->{node}->( $_[0], undef, @_[2..7] );
        $_[3]->data->{state} = 
            { node  => concat( [ $node, $state->{node} ] ), 
              count => $state->{count} + 1 };
    }
}

sub range {
    my $node = shift;
    my $min_count = shift;
    my $max_count = shift;
    my $greedy = not shift;
    return sub {

        my $continuation = $_[1]; #XXX how do optional continuations work?

        # Forward declarations

        my $try_getting_more;

        my $default_behavior;
        my $fallback_behavior;

        # Loop variables

        my $count = 0;
        my $previous_pos = -1;

        # Loop 1 - getting to min_count

        my $continue_towards_min;
        my $get_minimum = sub {
            if ( $count < $min_count ) {
                $count++;
                goto &$continue_towards_min;
            } else {
                goto &$try_getting_more;
            }
        };
        $continue_towards_min = concat( [ $node, $get_minimum ] );

        # Loop 2 - beyond the minimum

        $try_getting_more = sub {

            my $current_pos = $_[5];

            # (1) Stop when max_count is reached, or if pos does not move.

            if ( !( $count < $max_count ) ||
                 !( $previous_pos < $current_pos ) )
            {
                goto &$continuation;
            }
            $count++;
            $previous_pos = $current_pos;

            # (2) Attempt the default behavior.

            # XXX - This section needs to be filled in.
            # try $default_behavior
            #  if successful, return.
            #  if abort, do whatever is needed.
            #  if fail, we need to backtrack:
            #    undo any side-effects from trying the $default_behavior,
            #    so we can do the $fallback_behavior.

            # (3) Since the default behavior failed, do the fall-back beharvior.

            goto &$fallback_behavior;

        };
        my $get_one_and_maybe_more = concat( [ $node, $try_getting_more ] );

        # Final preparations.

        if ( $greedy ) {
            $default_behavior = $get_one_and_maybe_more;
            $fallback_behavior = $continuation;
        } else { # non-greedy
            $default_behavior = $continuation;
            $fallback_behavior = $get_one_and_maybe_more;
        }

        # Start.

        goto &$get_minimum;
    };
}


sub preprocess_hash {
    # TODO - move to Pugs::Runtime::Regex
    my ( $h, $key ) = @_;
    # returns AST depending on $h
    #print "preprocess_hash: ", Dumper( \@_ );
    if ( ref( $h->{$key} ) eq 'CODE') {
        return sub { 
            my ( $str, $grammar, $args ) = @_;
            #print "data: ", Dumper( \@_ );
            my $ret = $h->{$key}->( @_ ); 
            #print "ret: ", Dumper( $ret );
            
            return $ret 
                if ref( $ret ) eq 'Pugs::Runtime::Match';
            
            Pugs::Runtime::Match->new( { 
                bool => \1, 
                str =>  \$str,
                from => \( 0 + ( $args->{p} || 0 ) ),
                to =>   \( 0 + ( $args->{p} || 0 ) ),
                named => {},
                match => [],
            } ) }
    } 
    if ( ref( $h->{$key} ) =~ /Pugs::Compiler::/ ) {
        return sub { $h->{$key}->match( @_ ) };
    }
    # fail is number != 1 
    if ( $h->{$key} =~ /^(\d+)$/ ) {
        return sub { 
            my ( $str, $grammar, $args ) = @_;
            Pugs::Runtime::Match->new( { 
                bool => \0, 
                str =>  \$str,
                from => \( 0 + ( $args->{p} || 0 ) ),
                to =>   \( 0 + ( $args->{p} || 0 ) ),
                named => {},
                match => [],
            } ) } unless $1 == 1;
        return sub { 
            my ( $str, $grammar, $args ) = @_;
            Pugs::Runtime::Match->new( { 
                bool => \1, 
                str =>  \$str,
                from => \( 0 + ( $args->{p} || 0 ) ),
                to =>   \( 0 + ( $args->{p} || 0 ) ),
                named => {},
                match => [],
            } ) };
    }
    # subrule
    #print "compile: ",$h->{$key}, "\n";

    # XXX - compile to Token or to Regex ? (v6.pm needs Token)
    my $r = Pugs::Compiler::Token->compile( $h->{$key} );
    $h->{$key} = $r;
    return sub { $r->match( @_ ) };
    # return sub { warn "uncompiled subrule: $h->{$key} - not implemented " };
}

# see commit #9783 for an alternate implementation
sub hash {
    my %hash = %{shift()};
    #print "HASH: @{[ %hash ]}\n";
    my @keys = sort {length $b <=> length $a } keys %hash;
    #print "hash keys [ @keys ]\n";
    for ( @keys ) {
        my $h = preprocess_hash( \%hash, $_ );
        my $key = $_;
        $_ = 
          concat( [
            constant( $key ),
            sub { 
              # print "hash param: ",Dumper(\@_);
              # TODO - add $<KEY> to $_[7]
              $_[3] = $h->( $_[0], $_[4], $_[7], $_[1] );
              # print "result: ",Dumper($_[3]);
            }
          ] );
    }
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
    croak "Couldn't find '$name' in surrounding lexical scope.";
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


=for docs

Node types

  - matching node
       o--[a]--o
    
  - alternation 
        +-->o--+
    o---+-->o--+---o
        +-->o--+
        +-->o--+
        
  - concatenation 
    o--->o--->o--->o
    
Node states

  - matched (boolean True)
  - failed  (boolean False)
  - running
  - exception (aborted)
    
=cut

