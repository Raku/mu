use strict;
use warnings;
no warnings qw{ reserved closure recursion };

package Perl6in5::Compiler::Parser;

use Exporter;
our @EXPORT_OK = qw(hit eoi nothing debug star opt
                say all one flatten newline left ceoi
                trace %N parser check execnow $Nothing
                ch w keyword keywords panic p6ws
                unspace optws manws opttws mantws through
                plus both match unmore );
our @ISA = 'Exporter';
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

our %N;

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

sub execnow (&) { $_[0]->() }

sub say (@) { print($_,"\n") for @_ }

sub parser (&) { 
    bless(  
    #mapply( 
    $_[0]
    # ) 
    => __PACKAGE__ );
}

sub dump1 {
#  return '';
{ local $Data::Dumper::Deparse = 0;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Useqq = 1;
$Data::Dumper::Quotekeys = 0;
    "dumper from ".Dumper([caller(),caller(1),caller(2),caller(3),caller(4),caller(5),caller(6)])."\n".join("\n", map eval{$N{$_}||'none'}, grep $_,@_).Dumper(@_).join("\n",@_).Dumper(\%N).("_-" x 500) }
}

sub trace ($$) { # trace
  my $level = shift; # trace
  return unless $level <= 'tracelevel'; # trace
  my $msg = (shift) . "\n"; # trace
  my $i = 0; # trace
  $i++ while caller($i); # trace
  my $I = "-" x int($i/2-1); # trace
  warn $i.$I." ", $msg; # trace
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

sub ohr { # ordered hash ref
    tie my %h, 'Tie::IxHash', @_;
    return \%h;
}

# Randal Schwartz' deep_copy, modified
sub deep_copy {
    my $this = shift;
    my $r = ref $this;
    if (not $r || $r eq "CODE") {
        $this;
    } elsif ($r eq "ARRAY") {
        [map deep_copy($_), @$this];
    } elsif ($r eq "HASH") {
        +{map { $_ => deep_copy($this->{$_}) } keys %$this};
    } else {
        die "deep_copy failure";
    }
}

# the memo table, storing memo entries
# key depth 0. position in input string ($pos)
#           1. coderef of target rule ($q)
#     value 2. hashref of rule result ($ans)
#        OR 2. arrayref of LR object ($lr)
# Most times, there will be more rules
# (high hundreds?) than positions.
our %M;

# setter/getter for a memo table entry.
sub mmemo {
    # inputs: rule, position, $ans|$lr
    $M{$_[1]}->{$_[0]} = $_[2] if defined $_[2];
    # outputs: [ $ans, $pos ]
    $M{$_[1]}->{$_[0]};
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
    my $tmp = $p = parser {
        my ($in,$cont) = @_;
        my $pos = $in->{'pos'};
        my $m = mrecall($q,$pos);
        if (!defined $m) {
            
            # Initialize a new (lexical) @lr
            my @lr = ( undef, $q, undef );
            
            # push the newly created left-recursive
            # entry onto the left-recursive stack.
            push @L, \@lr;
            
            # Set the initial memo table entry for
            # this rule/pos to be that $lr
            mmemo($q, $pos, \@lr);
            
            # This evaluates the body of the rule,
            # which of course calls their wrapper
            # mapply() parsers prior, in turn.
            # The resulting hashref is a little
            # different from the paper's original
            # specification (just the AST or a failure code)
            my $ans = $q->($in,$cont);
            
            # Pop the $lr back off the lr stack because
            # we're done generating its lr seed.
            pop @L;
            
            # check if its evaluation created a "head"
            if (defined $lr[2]) {
                
                # store the result of $q (and its continuation(s))
                # in the left-recursive item's seed slot.
                $lr[0] = deep_copy($ans);
                
                # if the LR's rule isn't our current rule
                # must dereference the head, since they're
                # all really stored in %H.
                if (${$lr[2]}->{rule} ne $q) {
                    
                    # return the LR's seed
                    return $lr[0];
                    
                } else {
                    # there was no head created
                    
                    # commit the seed to the memo table
                    mmemo($q,$pos,$lr[0]);
                    
                    # if that branch was a failure
                    unless ($lr[0]->{success}) {
                        
                        # return that failure
                        return deep_copy($lr[0]);
                        
                    } else {
                        # the branch succeeded
                        
                        # commit the head to head storage
                        $H{$pos} = $lr[2];
                        
                        # 
                        while (1) {
                            
                            # at each iteration, the involved
                            # rules get another chance to hit.
                            delete $H{$pos}->{eSet} if exists
                                $H{$pos}->{eSet};
                            $H{$pos}->{eSet} = ohr();
                            foreach (keys %{$H{$pos}->{iSet}}) {
                                $H{$pos}->{eSet}->{$_} = 
                                    $H{$pos}->{iSet}->{$_};
                            }
                            
                            # run the rule *again*, this time
                            # the results should be different
                            $ans = $q->($in,$cont);
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
                # if there's already a head
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
                        while ( $s->[2] ne $hdr ) {
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
                return deep_copy($m);
            }
            
        }
        
    };
    $N{$p} = "_$N{$q}_";
    weaken($p);
    $p;
}

sub mrecall {
    # inputs: rule, position, input
    my $m = mmemo($_[0],$_[1]);
    return $m if (!exists $H{$_[1]} || keys(%{$H{$_[1]}}) == 0);
    unless (
        !defined $m ||
        exists $H{$_[1]}->{head}->{$_[0]} ||
        exists $H{$_[1]}->{iSet}->{$_[0]}
    ) {
        return {%{$_[2]}, success=>0, 'pos'=>$_[1]};
    }
    if (exists $H{$_[1]}->{eSet}->{$_[0]}) {
        delete $H{$_[1]}->{eSet}->{$_[0]};
        my $r = $_[0]->($_[2],mreflect());
        $m = [ ($r->{success})?$r->{ast}:'FAIL',
                $r->{'pos'} ];
    }
    $m;
}

sub mreflect {
    my $p;
    my $tmp = $p = parser {
        my ($in) = @_;
        return {%$in,success=>1};
    };
    $N{$p} = "reflect";
    weaken($p);
    $p;
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
            ast=>[[],['EOI']],
            hit=>''
        };
    };
    $N{$p} = "EOI";
    weaken($p);
    $p;
}

sub nothing {
    my $p;
    my $tmp = $p = parser {
        if (left($_[0]) ne "") { # trace
            # nothing is the only base-parser that returns the 
            # result of its continuation untouched.
            trace 3,"Nothing: Input left is ".Dumper(left($_[0]));
        } else { # trace
            trace 3,"Nothing: (At end of input)";
        } # trace
        $_[1]->($_[0]);
    };
    weaken($p);
    $N{$p} = 'Nothing';
    $p;
}

sub p6ws () {
  trace 6,"generating p6ws ".Dumper([caller()]).Dumper(\@_);
  my $p;
  my $tmp = $p = plus(one(hit(" "),hit("\n")));
  weaken($p);
  $N{$p} = ' ws ';
  $p;
}

sub unmore {
    my $q = $_[0];
    # this parser acts like a NOT gate
    my ($p);
    my $tmp = $p = parser {
        my ($in,$cont) = @_;
        my $o;
        my $tmp2 = $o = parser {
            my ($in2,$cont2) = @_;
            # the inverse of the nothing parser; this always fails.
            # so, we know if $q returns the below string, $q
            # succeeded.  Otherwise, we'll get some other error
            # hashref from q.
            return {success=>0};
        };
        weaken($o);
        $N{$o} = "fake($N{$p})";
        my $r = $q->($in,$o);
        return $r unless $r->{success};
        # send it along its merry way to the nothing parser, which
        # always succeeds, with the original input and continuation.
        $cont->($in);
    };
    weaken($p);
    $N{$p} = "fail($N{$q})";
    $p;
}

sub iff {
    my $q = shift;
    my $p;
    my $tmp = $p = parser {
        my ($in,$cont) = @_;
        my $o;
        my $tmp2 = $o = parser {
            # $q must have succeeded, so send the original
            # input to the original continuation
            $cont->($in);
        };
        weaken($o);
        $N{$o} = "to($N{$p})";
        # test $in on $q; letting it continue with the
        # original input on success.
        $q->($in,$o);
    };
    weaken($p);
    $N{$p} = "iff($N{q})";
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
        my ($in,$cont) = @_;
        $in->{want} = "RE $q";
        my ($r) = (left($in) =~ $q) or return err($in,"$N{$p}");
        my $tier = [];
        push @$tier, $r;
        $in->{pos} += length($r);
        $in->{ast} = [$tier,$in->{ast}];
        $in->{hit} = $r;
        if ($in->{hit} =~ /\n/) {
            my @lines = split "\n",$in->{hit} ;
            $in->{col} = length($lines[-1]);
            # the number of lines is 1 greater than the lines breaks
            $in->{line} += scalar(@lines) - 1;
        } else {
            $in->{col} += length($r);
        }
        $cont->($in);
    };
    weaken($p);
    $N{$p} = "match($q)";
    $p;
}

sub left {
    substr($_[0]->{inp},$_[0]->{'pos'})
}

sub hit {
    my ($want,$count) = @_;
    # $want is the desired string.
    # $count is the number of occurrences to match.
    #   Defaults to 1 of course. 0 doesn't make sense.
    $count ||= 1;
    my $p;
    my $tmp = $p = parser {
        my ($in,$cont) = @_;
        trace 4,"hit got ".Dumper(left($in));
        my $l = length($want);
        $in->{want} = $want x $count;
        return err($in,"hit() is empty in the grammar") unless $l;
        my $tier = []; # the new tier in the AST
        for my $i (1..$count) {
            unless (substr(left($_[0]),$l*($i-1),$l*$i) eq $want) {
                trace 4,"hit missed";
                return err($in,$want);
            }
            trace 4,"hit matched";
            push @$tier,$want;
        }
        $in->{'pos'} += $l * $count;
        trace 4,"advanced pos by ".($l*$count);
        $in->{ast} = [$tier,$in->{ast}];
        $in->{hit} = $in->{want};
        if ($in->{hit} =~ /\n/) {
            my @lines = split "\n",$in->{hit} ;
            $in->{col} = length($lines[$#lines] || '');
            # the number of lines is 1 greater than the lines breaks
            $in->{line} += scalar(@lines) - 1;
        } else {
            $in->{col} += $l * $count;
        }
        my $r = $cont->($in);
        $r;
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
    return parser { return () } if @p == 0;
    return $p[0]             if @p == 1;
    my $p;
    my $tmp = $p = parser {
        my ($in,$cont) = @_;
        trace 3,"Match one: $N{$p}";
        my ($q, $np) = (0, scalar @p); # trace
        my ($v,$r,$z);
        my $b = {};
        eval {
            %{$b} = %{$in};
        };
        if ($@) {
            use Carp 'confess';
            confess('argh');
        }

        for (@p) {
            $q++; # trace
            my $c = {};
            %{$c} = %{$b};
            trace 2,"Trying $q/$np: ".$N{$p[$q-1]}." on ".left($c)." and will send to ".$N{$cont};
            trace 2," b pos is ".$b->{'pos'};
            $r = $_->($c,$cont);
            trace 2," b pos is ".$b->{'pos'};
            trace 4,"one: ".$N{$p[$q-1]}." returned ".Dumper($r);
            unless ($r->{success}) {
                trace 2,"Failed $q/$np: ".$N{$p[$q-1]};
                # send back the shortest remaining input if none
                # succeed; this means the input was validly parsed
                # up till then.
                $z = $r if (!defined $z || length(left($r)) < length(left($z)));
            } else {
                trace 3,"Matched $q/$np: ".$N{$p[$q-1]};
                return $r if $r->{fated};
                $v = $r if (!defined $v || length(left($r)) < length(left($v)));
            }
        }
        if (defined $v) {
            trace 3,"Finished matching $N{$p}";
            return $v;
        }
        # we need to reset the pos on failure.  Please do
        # not ask me how many hours it took me to discover this.
        trace 3,"Failed to match any of: $N{$p}";
        trace 5,"sending back: ".Dumper({%$z, 'pos'=>$b->{'pos'}});
        return ($z->{backed})?$z:{%$r, 'pos'=>$b->{'pos'},backed=>1};
    };
    weaken($p);
    $N{$p} = "one(" . join("|", map $N{$_}, @p) . ")";
    $p;
}

my $cdepth = {};

sub both {
  my ($A, $B, $wsrule) = @_;
  $wsrule ||= '.';
  my $p;
  my $tmp = $p = parser {
    my ($in, $cont) = @_;
    my $i = 0;
    $i++ while caller($i);
    trace 4,"both $N{$p} on ".Dumper(left($in))." at depth $i";
    my $loc = $in->{'pos'}.$in->{mut};
    $loc ||= 'persnickity';
    if (# we've been in this parser before
        exists $cdepth->{$p}
        # we've seen this input for this parser last
        && exists $cdepth->{$p}->{$loc} && 
        $cdepth->{$p}->{$loc} > length(left($in))+1) {
        # we're in an infinite recursion.
        trace 4,"$N{$p} was too deep in itself: at $cdepth->{$p}->{$loc}";
        return err($in,"recursion curtailed in $N{$p}");
    } else {
        # store the coderef addresses for this
        # parser so we can detect infinite loops
        $cdepth->{$p}->{$loc}++;
        trace 5,"we're not in an infinite recursion";
        trace 5,'$cdepth->{$p}->{$loc}'." is now $cdepth->{$p}->{$loc}";
    }
    my ($r,$BC);
    my $tmp2 = $BC = parser {
      my ($newinput) = @_;
      # $BC won't get invoked by hit() unless A succeeds.
      $B->($newinput, $cont);
    };
    if ($wsrule eq '.') {
        # leave $A the way it is (this function's base case)
    } elsif ($wsrule eq '+') {
        $A = manws($A)
    } elsif ($wsrule eq '-') {
        $A = optws($A)
    }
    weaken($BC);
    $N{$BC} = $N{$B}.'.'.$N{$cont};
    trace 2,"both attempting $N{$A} then $N{$BC}";
    $r = $A->($in, $BC);
    unless ($r->{success}) {
      trace 2,"Failed to match at least one of $N{$A} and $N{$BC}";
      return ($r->{backed})?$r:{%$r, 'pos'=>$in->{'pos'},backed=>1};
    }
    trace 2,"Finished matching $N{$A} and $N{$BC}";
    return {%$r,
        ast=> [
            [ # the new "current node"
              # merges the result of $BC
              # with the most recent result
                @{head($r->{ast})},
                $r->{hit} ],
            tail($r->{ast}) ]
    };
  };
  trace 6,"both ".dump1($A,$B);
  weaken($p);
  $N{$p} = $N{$A};
  $N{$p} .= $wsrule;
  $Data::Dumper::Deparse = 1;
  print Dumper($B).Dumper([caller(5)]) unless exists $N{$B};
  $N{$p} .= $N{$B};
  $p;
}

sub all {
  my @p = grep $_,@_;
  return (scalar(@p)?$_[0]:nothing) if scalar(@p) < 2;
  my $tail = pop @p;
  my $p;
  my $tmp = $p = both(all(@p),$tail,'.');
  trace 6,"all ".dump1($tail,@p,$p);
  weaken($p);
  $N{$p} = "all(".join(",",map($N{$_},@p)).",$N{$tail})";
  $p;
}

sub star {
    my $p;
    my $tmp = $p = opt(plus($_[0]));
    weaken($p);
    $N{$p} = "($N{$_[0]})*";
    $p;
}

sub opt {
  my $p;
  my $tmp = $p = one($_[0], nothing);
  weaken($p);
  $N{$p} = "?($N{$_[0]})";
  $p;
}

sub unspace () {
    my $p;
    my $tmp = $p = all(hit("\\"),star(p6ws));
    weaken($p);
    $N{$p} = 'unspace';
    $p;
}

sub w { # look for a wrapped entity.  first parm is split into the wrappers.
    my ($d,$e) = split(//,$_[0]);
    my $p;
    my $tmp = $p = optws(optws(hit($d),$_[1]),hit($e));
    weaken($p);
    $N{$p} = "$d$N{$_[1]}$e";
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
    $N{$p} = "$N{$_[0]}-$N{$_[1]}";
  } else {
    # unary -, so optional leading whitespace
    $tmp = $p = both(opt(p6ws), $_[0], '.');
    weaken($p);
    $N{$p} = " -($N{$_[0]})";
  }
  $p;
}

sub manws {
  my ($p,$tmp);
  if (defined($_[1])) {
    # binary +, so mandatory intervening  whitespace
    $tmp = $p = all($_[0], p6ws, $_[1]);
    weaken($p);
    $N{$p} = "$N{$_[0]}+$N{$_[1]}";
  } else {
    # unary +, so mandatory leading whitespace
    $tmp = $p = both(p6ws, $_[0], '.');
    weaken($p);
    $N{$p} = " +($N{$_[0]})";
  }
  $p;
}

sub opttws {
  my $p;
  my $tmp = $p = both($_[0], opt(p6ws), '.');
  weaken($p);
  $N{$p} = "($N{$_[0]})-- ";
  $p;
}

sub mantws {
  my $p;
  my $tmp = $p = both($_[0], p6ws, '.');
  weaken($p);
  $N{$p} = "($N{$_[0]})++ ";
  $p;
}

sub newline () {
  my $p;
  my $tmp = $p = plus(one(panic(hit("\n#{"),"\\n#{ is illegal"),opttws(optws(hit("\n")))));
  weaken($p);
  $N{$p} = "\\n";
  $p;
}

# plus($s) = /^s+$/
sub plus {
    my $t = $_[0];
    my $p;
    my $tmp = $p = parser {
        my ($in,$cont) = @_;
        my $r = 0;
        my ($q,$n);
        my $tmp2 = $q = parser {
            my ($in2,$cont2) = @_;
            # if we got here once, we've succeeded.
            $in2;
        };
        $N{$q} = "fake($N{$p})";
        my $s;
        $n = $in;
        while(1) {
            # keep sending to our reflector
            $n = $t->($n,$q);
            trace 1,"plus return ".Dumper($n);
            last unless $n->{success} == 1;
        }
        return err($in,"$N{$p}") unless $n->{success};
        $cont->($n);
    };
    weaken($p);
    $N{$p} = "($N{$_[0]})+ ";
    $p;
}

sub nthru {
  my ($n,$o) = @_;
  my $p;
  my $tmp = $p = both($n, thru($o),'.');
  weaken($p);
  $N{$p} = "$N{$n}...$N{$o}";
  $p;
}

# slurp up stuff until a stopper parser matches. basically, the {*} signifier.
# if you want to slurp up stuff *up until but stopping short of* the stopper,
# wrap the stopper in iff().  if-and-only-if, get it?
sub thru {
    my $stop = $_[0];
    my $p;
    my $tmp = $p = parser {
        my ($in,$cont) = @_;
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
        $cont->(tail($in),undef,{eaten=>$len,ast=>[@values]});
    };
    weaken($p);
    $N{$p} = "{*}.".$N{$stop};
    $p;
}

sub keyword {
    my $p;
    my $tmp = $p = hit($_[0]);
    weaken($p);
    $N{$p} = "$_[0]";
    $p;
}

sub keywords {
    my $p;
    my $tmp = $p = one(map(hit($_),@_));
    weaken($p);
    $N{$p} = join("|",@_);
    $p;
}

sub panic {
    my ($ins,$msg) = @_;
    my $p;
    my $tmp = $p = parser {
        my ($in,$cont) = @_;
        trace 4,"Dying if find $N{$ins}";
        my $q;
        my $tmp2 = $q = parser {
            die $msg;
        };
        weaken($q);
        $N{$q} = "fake $N{$ins}";
        my $r = $ins->($in,$q);
        # we didn't die, so $q didn't succeed.
        # therefore send the continuation the original input
        $cont->($in);
    };
    weaken($p);
    $N{$p} = "panic($N{$ins})";
    $p;
}

1;
