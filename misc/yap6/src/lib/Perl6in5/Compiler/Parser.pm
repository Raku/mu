use strict;
use warnings;
no warnings qw{ reserved closure recursion };

package Perl6in5::Compiler::Parser;

use Exporter;
our @EXPORT_OK = qw(lit eoi nothing debug star opt %stat
                say all one flatten newline left ceoi
                trace %N parser check $Nothing to
                ch w keyword keywords panic p6ws parser2
                unspace optws manws opttws mantws through
                plus both match unmore ow now);
our @ISA = 'Exporter';
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

our (%N);

use Tie::IxHash;

use Perl6in5::Compiler::Trace; # set env var trace for trace output
# disregarding leading whitespace, lines that start with trace
# or end with trace,are discarded by the source filter in 
# Perl6in5::Compiler::Trace.  This allows execution time to be
# much faster than otherwise, since the strings to be traced
# are not only not sent to STDOUT; they aren't even built.
my $tracemode = 0;
$tracemode = 1; # trace

use Scalar::Util qw( weaken );

use Perl6in5::Compiler::Stream 'node', 'head', 'tail', 'promise', 'drop';

use overload
    '-'  => \&optws, # optional leading or intervening whitespace
    '+'  => \&manws, # mandatory leading or intervening whitespace
    '.'  => \&all, # intervening whitespace disallowed
    '|'  => \&one, # longest token matching by its nature.
    '!'  => \&unmore, # continue on failure, but don't eat input
    '~'  => \&iff, # continue on success, but don't eat input
    '...'=> \&nthru, # both($_[0],through($_[1]))
    '>>' => \&T, # unused currently
    '>'  => \&V, # unused currently
    '/'  => \&checkval, # unused currently
    '""' => \&overload::StrVal,  # this helps stringify parser names for %N
;

$| = 1;
 # trace

use Data::Dumper;
# so that trace output is on one line:
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;
$Data::Dumper::Useqq = 1;
$Data::Dumper::Quotekeys = 0;
#$Data::Dumper::Deparse = 1; # trace
#$Data::Dumper::Deparse = 1; # debug

sub say (@) { print($_,"\n") for @_ }

sub parser (&) {
    mapply3( @_ )
    #parser2( @_ )
}

sub parser2 (&) {
    return bless( $_[0] => __PACKAGE__ )
}

sub trace ($$) { # trace
  my $level = shift; # trace
  return unless $level <= 'tracelevel'; # trace
  my $msg = (shift) . "\n"; # trace
  my $i = 0; # trace
  $i++ while caller($i); # trace
  my $I = "-" x int($i/2-1); # trace
  warn " $i -- ", $msg; # trace
} # trace

sub callstack () { # trace
  my $i = 1; # trace
  my $msg; # trace
  while (my @stack = caller($i)) { # trace
    $i++; # trace
    $msg .= "\t".join(",",@stack)."\n" if @stack; # trace
  } # trace
  trace 4,$msg;
} # trace

sub debug ($) { # debug
  my $msg = (shift) . "\n"; # debug
  my $i = 0; # debug
  $i++ while caller($i); # debug
  my $I = "-" x int($i/2-1); # debug
  print STDERR $i.$I." ", $msg; # debug
} # debug

# ordered hash ref
sub ohr {
    tie my %h, 'Tie::IxHash', @_;
    return \%h;
}

# Randal Schwartz' deep_copy, modified
sub deep_copy {
    my $this = shift;
    my $r = ref $this;
    if (!$r || $r eq "CODE" || $r eq 'REF' || $r eq 'Perl6in5::Compiler::Parser') {
        $this;
    } elsif ($r eq "ARRAY") {
        [map deep_copy($_), @$this];
    } elsif ($r eq "HASH") {
        +{map { $_ => deep_copy($this->{$_}) } keys %$this};
    } else {
        die "ref was: ".$r." deep_copy failure: ".Dumper($this);
    }
}

# the memo table, storing memo entries
# key depth 0. position in input string ($pos)
#           1. coderef of target rule ($q)
#     value 2. hashref of rule result ($ans)
our %M;

# setter/getter for a memo table entry.
sub mmemo {
    unless (defined $_[1]) {
        trace 1,"mmemo got: ".Dumper(\@_)." from ".Dumper([caller(0),caller(1),caller(2)]);
        die "rule coderef empty";
    }
    # inputs: rule, position, $ans|$lr
    if (defined $_[2]) {
        trace 6,"mmemo setting something";#rule $_[1] at pos $_[0] to ans $_[2]";
        $M{$_[1]}->{$_[0]} = $_[2];
        return $_[2];
    }
    if (exists $M{$_[1]}) {
        trace 6,"mmemo found a memo entry existing already";
        return $M{$_[1]}->{$_[0]};
    } else {
        trace 6,"mmemo failed to find a memo entry";
        return undef;
    }
}

# The stack of possibly-left-recursive ops
# implemented as a perl array with push/pop,
# whose member items are references to @lrs.
# @lr items are arrayref tri-tuples:
# 0. The "seed" result-kernel
# 1. the "rule"'s coderef
# 2. the "head" rule of the seed, initially undef,
#    signifying non-left-recursive invocations.
#    A reference to the hashref stored in %H by pos
our @L;

# The currently-being-grown left recursions,
# implemented as a hashref, mapping position
# to the left recursion being grown there.
# key   depth 0: position
# value depth 1: "head": coderef to the rule
# key   depth 1: "iSet" - Tie::IxHash set of
#                rule coderefs known to be
#                'involved' in the left recursion
# key   depth 1: "eSet" - Tie::IxHash set of
#                rule coderefs planned to be
#                evaluated during this growth cycle
# key   depth 2: rules as hash entries keyed by
#                their coderef's values
our %H;

our %stat;

sub mapply0 {
    # the first edition of the apply-rule routine
    my $q = $_[0];
    trace 1,"Building a mapply0 parser";
    my $p;
    my $tmp = $p = parser2 {
        my ($in) = @_;
        my $pos = $in->{'pos'};
        my $m = mmemo($q,$pos);
        return (defined $m) ?
            $m:
            mmemo($q,$pos,$q->($in));
    };
    $N{$p} = "_mapply0_";
    weaken($p);
    $p;
}

sub mapply1 {
    # the second edition of the apply-rule routine
    # this one always fails on direct recursion.
    my $q = $_[0];
    trace 1,"Building a mapply0 parser";
    my $p;
    my $tmp = $p = parser2 {
        my ($in) = @_;
        my $pos = $in->{'pos'};
        my $m = mmemo($q,$pos);
        unless (defined $m) {
            # set "deeper" calls of this to fail
            $m = deep_copy($in);
            $m->{success} = 0;
            mmemo($q,$pos,$m);
            # send it to $p instead of $q
            # (so that this checker is called as well)
            return mmemo($q,$pos,$p->($in));
        }
        return $m;
    };
    $N{$p} = "_mapply1_";
    weaken($p);
    $p;
}

sub mapply2 {
    # the third edition of the apply-rule routine
    # this one supports only direct recursion.
    my $q = $_[0];
    my $p;
    my $tmp = $p = parser2 {
        my ($in) = @_;
        my $pos = $in->{'pos'};
        my $m = mmemo($q,$pos);
        unless (defined $m) {
            # $q has never been applied at this position.
            trace 1,"mapply2 $N{$p} has never been applied at this position ($pos)";
            # set the memoized response to denote possible lr
            $m = deep_copy($in);
            $m->{lr} = 0;
            # memoize the "in lr" designation for this rule.
            mmemo($q,$pos,$m);
            $m = mmemo($q,$pos,$q->($in));
            if (exists $m->{lr} && $m->{lr} && $m->{success}) {
                # we're in a left recursion, and we got a seed.
                while (1) {
                    # grow the seed of the left recursion while
                    # it succeeds ####and advances in position.####
                    my $pos2 = $m->{'pos'};
                    # reset the position of the result to the original
                    $m->{'pos'} = $pos;
                    # replace the result hashref with the result
                    # of sending it to this rule's body
                    $m = $q->($m);
                    ####if ( $m->{success} && $m->{'pos'} > $pos2 ) 
                    ####    # the rule was a success and position was advanced.
                    ####    # this may need to be adapted for zero-width assertions.
                    unless ( !$m->{success} || $m->{'pos'} < $pos2 ) {
                        # the rule was a success.
                        mmemo($q,$pos,$m);
                    } else {
                        last;
                    }
                }
                #m should now be set to the results of grow-lr.
            }
        } elsif (exists $m->{lr}) {    
            trace 1,"mapply2 $N{$p} has been marked as a potential recursion at this position ($pos)";
            # it has been marked as a potential left recursion.
            # mark it as a *definite* left recursion.
            $m->{lr} = 1;
            # mark it as a failure
            $m->{success} = 0;
        } else {
            trace 1,"mapply2 $N{$p} is definitely not a left recursion at this position ($pos)";
        }
        $m;
    };
    weaken($p);
    $N{$p} = "_mapply2_";
    $p;
}

sub mapply3 {
    # the fourth edition of the apply-rule routine
    # this one supports direct & indirect recursion.
    my $q = $_[0];
    my $p;
    my $tmp = $p = parser2 {
        my ($in) = @_;
        my $pos = $in->{'pos'};
        my $m = mmemo($q,$pos);
        if ( !exists $H{$pos} || keys(%{$H{$pos}}) == 0 || !defined $H{$pos}->{head} ) {
            trace 5,"mapply using ".Dumper($m)." since there was no head rule for this pos ($pos)";
        } elsif ( !defined $m && $H{$pos}->{head} ne $q &&
                !exists $H{$pos}->{iSet}->{$q} ) {
            trace 5,"mapply found no current memoized result for '$N{$p}' and this rule is not in the involved set";
            $m = err($in,"mapply failure");
        } elsif ( exists $H{$pos}->{eSet}->{$q} &&
                defined $H{$pos}->{eSet}->{$q} ) {
            delete $H{$pos}->{eSet}->{$q};
            trace 3,"mapply calling $N{$p} with ".Dumper($in);
            $m = mmemo($q,$pos,$q->($in));
        }
        unless (defined $m) {
            # $q has never been applied at this position.
            # by the time we get here, $p has been initialized as
            # a coderef to *this subroutine*, which has been given
            # a name *after* it was named "_mapply_" below.
            trace 3,"mapply $N{$p} has never been applied at this position ($pos)";
            
            # Initialize a new (lexical) @lr
            my @lr = ( {%$in, success=>0}, $q, undef );
            
            # push a reference to the newly created left-recursive
            # entry onto the left-recursive stack.
            push @L, \@lr;
            trace 5,"mapply size of \@L is now ".scalar(@L);
            
            # Set the initial memo table entry for
            # this rule/pos to be that $lr
            mmemo($q, $pos, { %{$in}, lr=>\@lr } );
            trace 5,"mapply \%M is ".Dumper(\%M);
            
            # This evaluates the body of the rule,
            # which of course calls their wrapper
            # mapply() parsers prior, in turn.
            # The resulting hashref is a little
            # different from the paper's original
            # specification (just the AST or a failure code)
            trace 5,"mapply sending ".Dumper($in)." to $N{$p}";
            my $ans = $q->($in);
            trace 5,"mapply got ".Dumper($ans)."from $N{$p}";
            
            trace 5,"mapply about to pop \@L: ".Dumper(\@_);
            # Pop the $lr back off the lr stack because
            # we're done generating its lr seed.
            pop @L;
            
            $m->{'pos'} = $ans->{'pos'};
            
            # check if its evaluation created a "head"
            if (defined $lr[2]) {
                trace 3,"mapply $pos - lr-head was defined";
                
                # store the result of $q
                # in the left-recursive item's seed slot.
                $lr[0] = deep_copy($ans);
                
                # if the LR's rule isn't our current rule
                # must dereference the head, since they're
                # all really stored in %H.
                if (!defined ${$lr[2]}->{rule} || ${$lr[2]}->{rule} ne $q) {
                    trace 1,"mapply $pos - rule was different from $N{$p}";
                    
                    # return the LR's seed
                    trace 1,"mapply $pos - returning the seed of this head: ".Dumper($lr[0]);
                    return $lr[0];
                    
                } else {
                    trace 1,"mapply $pos - rule was SAME AS q";
                    
                    # commit the seed to the memo table
                    mmemo($q,$pos,$lr[0]);
                    
                    if ($m->{success}) {
                        # we're in a left recursion, and we got a seed.
                        
                        # commit the head to head storage
                        $H{$pos} = ${$lr[2]};
                        
                        while (1) {
                            # grow the seed of the left recursion while
                            # it succeeds ####and advances in position.####
                            my $pos2 = $m->{'pos'};
                            # reset the position of the result to the original
                            $m->{'pos'} = $pos;
                            
                            trace 1,"resetting eSet";#.Dumper($H{$pos});
                            # at each iteration, the involved
                            # rules get another chance to hit.
                            trace 1,"ref Hpos is ".ref($H{$pos});
                            delete $H{$pos}->{eSet} if exists
                                $H{$pos}->{eSet};
                            $H{$pos}->{eSet} = ohr();
                            trace 1,"got here 234";
                            foreach (keys %{$H{$pos}->{iSet}}) {
                                $H{$pos}->{eSet}->{$_} = 
                                    $H{$pos}->{iSet}->{$_};
                            }

                            # run the rule *again*, this time
                            # the results should be different
                            $ans = $q->($in);
                            unless (
                                $ans->{success} &&
                                $ans->{'pos'} <= $pos
                            ) { last } else {
                                mmemo($q,$pos,deep_copy($ans));
                            }
                        }
                        #m should now be set to the results of grow-lr.
                        delete $H{$pos};
                        return deep_copy($ans);
                    }
                }
            } else {
                # commit the change to the memo table;
                mmemo($q,$pos,deep_copy($ans));
                
                return deep_copy($ans);
            }
        } else {
            # store the result's position
            # $m->[1]
            
            # if the answer was an LR
            if (ref $m eq 'ARRAY') {
                
                # initialize the LR
                # if there's not already a head
                if (!defined $m->[2]) {
                    
                    # define a head
                    my ($iSet1,$eSet1) = (ohr(),ohr());
                    my $hd = {
                        rule => $q,
                        iSet => $iSet1,
                        eSet => $eSet1
                    };
                    my $hdr = \$hd;
                    
                    if (my $s = pop @L) {
                        trace 5,"popped rule is ".Dumper($s);
                        while ( !defined $s->[2] || $s->[2] ne "$hdr" ) {
                            $s->[2] = $hdr;
                            $hd->{iSet}->{$s->[1]} = 1;
                            if (scalar(@L)) {
                                $s = pop @L;
                            } else {
                                last;
                            }
                        }
                    }
                }
                
                # return the LR's seed
                return $m->[0];
            } else {
                $stat{memohits}++;
                return deep_copy($m);
            }
        }
        $m;
    };
    weaken($p);
    $N{$p} = "_mapply_";
    $p;
}

sub mapply {
    # mapply wraps every other parser with:
    # 1. a memo table check/handler &
    # 2. a[n] [in]direct left recursion check/handler
    # 
    # for details, see packrat_TR-2007-002.pdf:
    # 
    # "Packrat Parsers Can Support Left Recursion"
    # VPRI Technical Report TR-2007-002
    # ACM SIGPLAN 2008 Workshop on Partial
    # Evaluation and Program Manipulation
    # (PEPM '08) January 2008
    # ACM 978-1-59593-977-7/08/0001
    # by Alessandro Warth
    #    James R. Douglass
    #    Todd Millstein

    my $q = $_[0];
    my $p;
    my $tmp = $p = parser2 {
        $stat{rulecalls}++;
        trace 1,"in rulecall $stat{rulecalls}";
        my ($in) = @_;
        trace 1,"Got to mapply. in: ".Dumper($in,[caller(0),caller(1),caller(2),caller(3),caller(4)]) unless defined $in->{'pos'};
        trace 1,"Got to mapply WITHOUT A RULE. in: ".Dumper($in,[caller(0),caller(1),caller(2),caller(3),caller(4)]) unless defined $q;
        die unless defined $in->{'pos'};
        my $pos = $in->{'pos'};
        trace 1,"mapply pos is ".$pos;
        my $m = mrecall($q,$pos);
        trace 1,"mrecall returned ".Dumper($m);
        if (defined $q && exists $N{$q}) {
            trace 5,"name of rule is $N{$q}";
        } else {
            trace 1,"rule had no name! ".Dumper($q);
        }
        if (!defined $m) {
            trace 1,"mapply $pos - m was not defined, so we have never before encountered this rule.";
            
            # Initialize a new (lexical) @lr
            my @lr = ( { %$in, success=>0 }, $q, undef );
            
            # push a reference to the newly created left-recursive
            # entry onto the left-recursive stack.
            push @L, \@lr;
            trace 6,"mapply size of \@L is now ".scalar(@L);
            
            # Set the initial memo table entry for
            # this rule/pos to be that $lr
            trace 1,"calling mmemo";
            mmemo($q, $pos, \@lr);
            trace 1,"done with mmemo";
            trace 4,"mapply \%M is ".Dumper(\%M);
            
            # This evaluates the body of the rule,
            # which of course calls their wrapper
            # mapply() parsers prior, in turn.
            # The resulting hashref is a little
            # different from the paper's original
            # specification (just the AST or a failure code)
            trace 1,"mapply sending ".Dumper($in)." to q";
            my $ans = $q->($in);
            trace 1,"mapply got ".Dumper($ans)."from the rule";
            
            trace 1,"mapply about to pop \@L: ".Dumper(\@_);
            # Pop the $lr back off the lr stack because
            # we're done generating its lr seed.
            pop @L;
            
            # check if its evaluation created a "head"
            if (defined $lr[2]) {
                trace 1,"mapply $pos - lr-head was defined";
                
                # store the result of $q
                # in the left-recursive item's seed slot.
                $lr[0] = deep_copy($ans);
                
                # if the LR's rule isn't our current rule
                # must dereference the head, since they're
                # all really stored in %H.
                if (defined ${$lr[2]}->{rule} && "${$lr[2]}->{rule}" ne "$q") {
                    trace 1,"mapply $pos - rule was different from q";
                    
                    # return the LR's seed
                    return $lr[0];
                    
                } else {
                    trace 1,"mapply $pos - rule was SAME AS q";
                    # there was no head created
                    
                    # commit the seed to the memo table
                    mmemo($q,$pos,$lr[0]);
                    
                    # if that branch was a failure
                    unless ($lr[0]->{success}) {
                        
                        # return that failure
                        return deep_copy($lr[0]);
                        
                    } else {
                        # the branch succeeded
                        
                        trace 1,"lr being committed is ".Dumper(\@lr);
                        # commit the head to head storage
                        $H{$pos} = ${$lr[2]};
                        
                        # 
                        while (1) {
                            trace 1,"resetting eSet";#.Dumper($H{$pos});
                            # at each iteration, the involved
                            # rules get another chance to hit.
                            trace 1,"ref Hpos is ".ref($H{$pos});
                            delete $H{$pos}->{eSet} if exists
                                $H{$pos}->{eSet};
                            $H{$pos}->{eSet} = ohr();
                            trace 1,"got here 234";
                            foreach (keys %{$H{$pos}->{iSet}}) {
                                $H{$pos}->{eSet}->{$_} = 
                                    $H{$pos}->{iSet}->{$_};
                            }
                            
                            # run the rule *again*, this time
                            # the results should be different
                            $ans = $q->($in);
                            unless (
                                $ans->{success} &&
                                $ans->{'pos'} <= $pos
                            ) { last } else {
                                mmemo($q,$pos,deep_copy($ans));
                            }
                        }
                        delete $H{$pos};
                        return deep_copy($ans);
                    }
                    
                }
                trace 1,"mapply $pos - lr-head was NOT defined";
                
            } else {
                # commit the change to the memo table;
                mmemo($q,$pos,deep_copy($ans));
                
                return deep_copy($ans);
            }
        } else {
            # store the result's position
            # $m->[1]
            
            # if the answer was an LR
            if (ref $m eq 'ARRAY') {
                
                # initialize the LR
                # if there's not already a head
                if (!defined $m->[2]) {
                    
                    # define a head
                    my ($iSet1,$eSet1) = (ohr(),ohr());
                    my $hd = {
                        rule => $q,
                        iSet => $iSet1,
                        eSet => $eSet1
                    };
                    my $hdr = \$hd;
                    
                    if (my $s = pop @L) {
                        trace 1,"popped rule is ".Dumper($s);
                        while ( !defined $s->[2] || $s->[2] ne "$hdr" ) {
                            $s->[2] = $hdr;
                            $hd->{iSet}->{$s->[1]} = 1;
                            if (scalar(@L)) {
                                $s = pop @L;
                            } else {
                                last;
                            }
                        }
                    }
                }
                
                # return the LR's seed
                return $m->[0];
            } else {
                $stat{memohits}++;
                return deep_copy($m);
            }
        }
    };
    $N{$p} = "_mapply_";
    weaken($p);
    $p;
}

sub mrecall {
    # inputs: rule, position, input
    my $m = mmemo($_[0],$_[1]);
    if (!exists $H{$_[1]} || keys(%{$H{$_[1]}}) == 0) {
        trace 1,"mrecall returning ".Dumper($m)." b/c there are no heads for this pos";
        return $m;
    } else {
        trace 1,"mrecall got some heads for this pos, at rule ".Dumper($m);
    }
    if (
        !defined $m &&
        !exists $H{$_[1]}->{head}->{$_[0]} &&
        !exists $H{$_[1]}->{iSet}->{$_[0]}
    ) {
        return err($_[2],"mrecall failure");
    }
    if (exists $H{$_[1]}->{eSet}->{$_[0]} && defined $H{$_[1]}->{eSet}->{$_[0]}) {
        delete $H{$_[1]}->{eSet}->{$_[0]};
        trace 1,"mrecall calling"." ".$N{$_[0]}." with ".Dumper($_[2]);
        my $r = $_[0]->($_[2]);
        mmemo($_[0],$_[1],$r);
        $m = [ $r,
                $r->{'pos'} ];
    }
    $m;
}

sub eoi {
    my $p;
    my $tmp = $p = parser {
        my ($in) = @_;
        trace 3,"Looking for EOI";
        if (!ceoi($in)) {
            trace 5,"Found unparsed input: ".Dumper(left($_[0]));
            return err($in,'EOI');
        }
        trace 3,"Found EOI";
        return {%$in,
            success=>1,
            fated=>1,
       #     ast=>[@{$in->{ast}},['EOI']],
            hit=>''
        };
    };
    weaken($p);
    $N{$p} = "EOI";
    $p;
}

sub nothing {
    my $p;
    my $tmp = $p = parser2 {
        $_[0];
    };
    weaken($p);
    $N{$p} = 'nothing';
    $p;
}

sub blank () {
    my $p;
    my $tmp = $p = match( qr|^(\s+)| );
    weaken($p);
    $N{$p} = 'blank';
    $p;
}

sub p6ws () {
    my $p;
    my $tmp = $p = plus(one(blank,lit("\n")));
    weaken($p);
    $N{$p} = 'WS';
    $p;
}

sub unmore {
    my $q = $_[0];
    # this parser acts like a NOT gate
    my ($p);
    my $tmp = $p = parser {
        my ($in) = @_;
        my $b = deep_copy($in);
        $in = $q->($in);
        $b->{success} = $in->{success}?0:1;
        $b;
    };
    weaken($p);
    $N{$p} = "fail( $N{$q} )";
    $p;
}

sub iff {
    my $q = shift;
    my $p;
    my $tmp = $p = parser {
        my ($in) = @_;
        my $b = deep_copy($in);
        $b->{success} = $in->{success}?1:0;
        $b;
    };
    weaken($p);
    $N{$p} = "iff( $N{q} )";
    $p;
}

sub match {
    my $q = shift;
    # $q is a regular expression of the form:
    # qr/^(match_pattern)/ or qr/(match_pattern)/
    # note that you must include the ^ or you will get
    # incorrect results, b/c match() uses the length of the captured group
    # to eat the proper length of input.  Probably your REs shouldn't be greedy...
    my $p;
    my $tmp = $p = parser {
        my ($in) = @_;
        $in->{want} = "RE $q";
        if (($in->{hit}) = (left($in) =~ $q)) {
            trace 3,"match matched ".$in->{hit}." on $N{$p}";
        } else {
            trace 3,"match failed on $N{$p}";
            return err($in,"$N{$p}");
        }
        $in->{pos} += length($in->{hit});
        trace 3,"advanced pos by ".length($in->{hit});
        push @{$in->{ast}->[0]},$in->{hit} unless $in->{hit} =~ /^[\s\n]+$/;
        lineify($in);
    };
    weaken($p);
    $N{$p} = "match( $q )";
    $p;
}

sub lineify {
    if ($_[0]->{hit} =~ /\n/) {
        my @lines = split "\n",$_[0]->{hit};
        my @lines2 = split //,$_[0]->{hit};
        @lines = @lines2 unless @lines;
        $_[0]->{col} = length($lines[-1]);
        # the number of lines is 1 greater than the line breaks
        $_[0]->{line} += scalar(@lines) - 1;
    } else {
        $_[0]->{col} += length($_[0]->{hit});
    }
    $_[0];
}

sub left {
    trace 6,"left got ".Dumper(\@_)." from ".Dumper([caller(0),caller(1),caller(2),caller(3),caller(4)]);
    substr($_[0]->{inp},$_[0]->{'pos'})
}

sub lit {
    my ($want,$count) = @_;
    # $want is the desired string.
    # $count is the number of occurrences to match.
    #   Defaults to 1 of course. 0 doesn't make sense.
    $count ||= 1;
    my $p;
    my $tmp = $p = parser {
        my ($in) = @_;
        trace 4,"lit got ".Dumper(left($in));
        my $l = length($want);
        $in->{want} = $want x $count;
        return err($in,"lit() is empty in the grammar") unless $l;
        my @tier = (); # the new tier in the AST
        for my $i (1..$count) {
            trace 5,"in lit:".Dumper($in)." l is $l  i is $i  want is ".Dumper($want);
            unless (substr(left($_[0]),$l*($i-1),$l*$i) eq $want) {
                trace 3,"lit missed $want";
                return err($in,$want);
            }
            trace 3,"lit matched $want";
            push @tier,$want;
        }
        $in->{'pos'} += $l * $count;
        trace 3,"advanced pos by ".($l*$count);
        push @{$in->{ast}->[0]},@tier;
        $in->{hit} = $in->{want};
        lineify($in);
    };
    weaken($p);
    $N{$p} = Dumper($want x $count);
    $p;
}

sub ceoi {
    left($_[0]) eq ''
}

sub err{
    trace 3,"errored ".Dumper($_[1]);
    return {%{$_[0]},success=>0,expected=>($_[1] || 'unknown')};
}

sub one {
    my @p = grep $_,@_;
    return nothing if @p == 0;
    return $p[0]             if @p == 1;
    my $p;
    my $tmp = $p = parser {
        my ($in) = @_;
        trace 2,"one  ".$in->{'pos'}."  trying >0   ".$N{$p}." on ".Dumper(left($in));
        my ($q, $np) = (0, scalar @p); # trace
        my ($v,$r,$z);
        my $b = deep_copy($in);
        for (@p) {
            $q++; # trace
            my $c = deep_copy($b);
            trace 2,"one  ".$in->{'pos'}."  trying $q/$np   ".$N{$p[$q-1]}." on ".Dumper(left($c));
            $r = $_->($c);
            trace 5,"one  ".$N{$p[$q-1]}." returned ".Dumper($r);
            unless ($r->{success}) {
                trace 2,"one  ".$in->{'pos'}."  failed $q/$np    ".$N{$p[$q-1]};
                # send back the shortest remaining input if none
                # succeed; this means the input was validly parsed
                # up till then.
                $z = deep_copy($r) if (!defined $z || length(left($r)) < length(left($z)));
            } else {
                trace 3,"one  ".$in->{'pos'}."  matched $q/$np    ".$N{$p[$q-1]};
                #return $r if $r->{fated};
                $v = deep_copy($r) if (!defined $v || length(left($r)) < length(left($v)));
            }
        }
        if (defined $v) {
            trace 3,"one  ".$in->{'pos'}."  matched >0   $N{$p}";
            return $v;
        }
        trace 2,"one  ".$in->{'pos'}."  failed all   $N{$p}";
        trace 5,"sending back: ".Dumper({%$z, 'pos'=>$b->{'pos'}});
        return (($z->{backed})?$z:{%$z, 'pos'=>$b->{'pos'},backed=>1});
    };
    weaken($p);
    $N{$p} = "( " . join(" | ", map $N{$_}, @p) . " )";
    $p;
}

my $cdepth = {};

sub both {
  my ($A, $B, $wsrule) = @_;
  $wsrule ||= '.';
  my $p;
  my $tmp = $p = parser {
    my ($in) = @_;
    my ($r);
    if ($wsrule eq '.') {
        # leave $A the way it is (this function's base case)
    } elsif ($wsrule eq '+') {
        $A = mantws($A)
    } elsif ($wsrule eq '-') {
        $A = opttws($A)
    }
    trace 2,"both  ".$in->{'pos'}."  attempting   $N{$A}   then   $N{$B}";
    $r = $A->($in);
    unless ($r->{success}) {
      trace 2,"both  ".$in->{'pos'}."  failed 1   $N{$A}";
      return ($r->{backed})?$r:{%$r, 'pos'=>$in->{'pos'},backed=>1};
    }
    $r = $B->($r);
    unless ($r->{success}) {
      trace 2,"both  ".$in->{'pos'}."  failed 2   $N{$B}";
      return ($r->{backed})?$r:{%$r, 'pos'=>$in->{'pos'},backed=>1};
    }
    warn "we got an undef ast from $N{$B} in both" unless defined($r->{ast});
    trace 2,"both  ".$in->{'pos'}."  did  match   $N{$A}   and   $N{$B}";
    $r;
  };
  weaken($p);
  trace 6,"both creation dump: ".Dumper($A,$B,[caller(0),caller(1),caller(2)]);
  $N{$p} = "( $N{$A} $wsrule $N{$B} )";
  $p;
}

sub all {
  trace 6,"all creation dump: ".Dumper(\@_,[caller(0),caller(1),caller(2)]);
  my @p = grep $_,@_;
  return nothing if @p == 0;
  return $p[0] if @p == 1;
  my $tail = pop @p;
  my $p;
  my $tmp = $p = both(all(@p),$tail,'.');
  weaken($p);
  $N{$p} = "all( ".join(" , ",map($N{$_},@p))." , $N{$tail} )";
  $p;
}

# placing star directly in another star() causes right recursion
sub star {
    my ($p, $p_conc, $p_exec);
    
    # this is the self-reference
    my $tmp1 = $p_exec = parser { $p->(@_) };
    weaken($p_exec);
    $N{$p_exec} = "star( $N{$_[0]} )";
    
    # this is the recursion - the parser $p will call $p
    my $tmp2 = $p_conc = both($_[0], $p_exec, '.');
    weaken($p_conc);
    $N{$p_conc} = "( $N{$_[0]} . $N{$p_exec} )";
    
    my $tmp3 = $p = one($p_conc, nothing);
    weaken($p);
    $N{$p} = $N{$p_exec};
    $p;
}

sub opt {
  my $p;
  my $tmp = $p = one($_[0], nothing);
  weaken($p);
  $N{$p} = "opt( $N{$_[0]} )";
  $p;
}

sub unspace () {
    my $p;
    my $tmp = $p = all(lit("\\"),star(p6ws));
    weaken($p);
    $N{$p} = 'us';
    $p;
}

# look for a wrapped entity.  first parm is split into the wrappers.
sub w {
    my ($d,$e) = split(//,$_[0]);
    my $p;
    my $tmp = $p = all(lit($d),$_[1],lit($e));
    weaken($p);
    $N{$p} = "\"$d\" $N{$_[1]} \"$e\"";
    $p;
}

# the following 2 parsers are left-recursive

# look for an optionally wrapped entity.
sub ow {
    my ($d,$e) = split(//,$_[0]);
    my $p;
    my $tmp = $p = one(w($_[0],$_[1]),$_[1]);
    weaken($p);
    $N{$p} = "[\"$d\"] $N{$_[1]} [\"$e\"]";
    $p;
}

# nested optional wrap
sub now {
    my ($d,$e) = split(//,$_[0]);
    my ($p, $p_conc, $p_exec);
    
    # this is the self-reference
    my $tmp1 = $p_exec = parser { $p->(@_) };
    weaken($p_exec);
    $N{$p_exec} = "now( $N{$_[0]} )";
    
    # this is the recursion - the parser $p will call $p
    my $tmp2 = $p_conc = one( ow( $_[0], one( $p_exec , $_[1] ) ) );
    weaken($p_conc);
    $N{$p_conc} = "( $N{$_[0]} | now( $N{$p_exec} ) )";
    
    my $tmp3 = $p = ow( $_[0], $p_conc );
    weaken($p);
    $N{$p} = $N{$p_exec};
    $p;
}

# optws and manws always receive 1 or 2 arguments when called
# by the overloaded operators in grammars
# (so that it always gets two arguments)
#   from within hand-written parser generators, write them out
#   explicitly, or always pass them two arguments :)

sub optws {
  my ($p,$tmp);
  if ($_[1]) {
    # binary -, so optional intervening whitespace
    $tmp = $p = all($_[0], opt(p6ws), $_[1]);
    weaken($p);
    $N{$p} = "$N{$_[0]} - $N{$_[1]}";
  } else {
    # unary -, so optional leading whitespace
    $tmp = $p = both(opt(p6ws), $_[0], '.');
    weaken($p);
    $N{$p} = " -( $N{$_[0]} )";
  }
  $p;
}

sub manws {
  my ($p,$tmp);
  if (defined($_[1])) {
    # binary +, so mandatory intervening  whitespace
    $tmp = $p = all($_[0], p6ws, $_[1]);
    weaken($p);
    $N{$p} = "$N{$_[0]} + $N{$_[1]}";
  } else {
    # unary +, so mandatory leading whitespace
    $tmp = $p = both(p6ws, $_[0], '.');
    weaken($p);
    $N{$p} = "+( $N{$_[0]} )";
  }
  $p;
}

sub opttws {
  my $p;
  my $tmp = $p = both($_[0], opt(p6ws), '.');
  weaken($p);
  $N{$p} = "( $N{$_[0]} )--";
  $p;
}

sub mantws {
  my $p;
  my $tmp = $p = both($_[0], p6ws, '.');
  weaken($p);
  $N{$p} = "( $N{$_[0]} )++";
  $p;
}

sub newline () {
  my $p;
  my $tmp = $p = plus(one(panic(lit("\n#{"),"\\n#{ is illegal"),opttws(optws(lit("\n")))));
  weaken($p);
  $N{$p} = "\\n";
  $p;
}

# plus($s) = /^s+$/
sub plus {
    my $t = $_[0];
    my $p;
    my $tmp = $p = both($t,star($t),'.');
    weaken($p);
    $N{$p} = "plus( $N{$_[0]} )";
    $p;
}

sub nthru {
  my ($n,$o) = @_;
  my $p;
  my $tmp = $p = both($n, thru($o),'.');
  weaken($p);
  $N{$p} = "$N{$n} ... $N{$o}";
  $p;
}

# slurp up stuff until a stopper parser matches. basically, the {*} signifier.
# if you want to slurp up stuff *up until but stopping short of* the stopper,
# wrap the stopper in iff().  if-and-only-if, get it?
sub thru {
    my $stop = $_[0];
    my $p;
    my $tmp = $p = parser {
        my ($in) = @_;
        my $v;
        my @values;
        my $in2=$in;
        while (defined $in2) {
            $v = $stop->($in2);
            if (ref($v) eq 'HASH') {
                trace 5,"through $N{$stop} matched";
                push @values, $v;
                last;
            } else {
                trace 5,"through $N{$stop} still not matched";
                push @values, drop($in2);
                return [{expected=>$stop,found=>[undef],line=>'',file=>''}] unless defined $in2;
                next;
            }
        }
        my $len;
        $len += length($_) foreach @values;
        trace 4,"Through token matched";
        $in;
    };
    weaken($p);
    $N{$p} = "{*}.".$N{$stop};
    $p;
}

sub keyword {
    my $p;
    my $tmp = $p = lit($_[0]);
    weaken($p);
    $N{$p} = "'$_[0]'";
    $p;
}

sub keywords {
    my $p;
    my $tmp = $p = one(map(lit($_),@_));
    weaken($p);
    $N{$p} = "'".join("' | '",@_)."'";
    $p;
}

sub panic {
    my ($ins,$msg) = @_;
    my $p;
    my $tmp = $p = parser {
        my ($in) = @_;
        trace 4,"Dying if find $N{$ins}";
        my $b = deep_copy($in);
        $in = $ins->($in);
        die $msg if $in->{success};
        $b;
    };
    weaken($p);
    $N{$p} = "panic( $N{$ins} )";
    $p;
}

# build a parser object to
# arrange the results of a rule properly
sub to (&) {
    my ($h,$p) = @_;
    my $tmp = $p = parser {
        return {%{$h->($_[0])}, success=>1};
    };
    weaken($p);
    $N{$p} = "hh";
    $p;
}

sub r_amper {
    # iff(0).1 w/ identical start/endpoints
}

sub r_2amper {
    # "both" sequence point - backtracking "allowed"
}

sub r_pipe {
    # disjunction - can be tried in parallel or random order
}

sub r_2pipe {
    # firstone()
}

sub r_2tilde {
    # left item result, submatch
}

sub r_caret {
    # tight prefix, binds to start of subject
}

sub r_dollar {
    # tight postfix, binds to end of subject
}

sub r_sbrac {
    # "square brackets" w('[]')  # note - need an internals-balancing edition of w()
    # precedence override, non-capturing
}

sub r_rbrac {
    # "round brackets" w('()') - capturing group.
}

sub r_abrac {
    # "angle brackets" w('<>') - 
}

sub r_cbrac {
    # "curly brackets" w('{}') - embedded closure
}



1;
