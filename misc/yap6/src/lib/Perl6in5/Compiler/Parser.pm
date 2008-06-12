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

#sub normalize_parser { # memoize
    # stringify the contents of $in and the coderef # memoize
    # of the continuation. # memoize
    # left() is also memoized :) # memoize
#    left($_[0]).$_[0]->{'pos'}.(defined $_[1])?$_[1]:''; # memoize
#} # memoize

# memoize the terminal parser constructors
#use Memoize; # memoize
#map { memoize $_ } qw{ newline p6ws nothing }; # memoize
# the other benefit of this is that we don't get more than one of 
# each kind of terminal parser constructed, which means the memoization of the
# generated functions will be more efficient than otherwise.

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

# memoize each and every generated parser coderef before it's blessed.
sub parser (&) { 
 #   my $subref = $_[0]; # memoize
 #   my $memoized; # memoize
 #   my $blessed =  # memoize
    bless(  
    #    $memoized =  # memoize
    #    memoize( # memoize
        $_[0]
        #, NORMALIZER=>'normalize_parser' # memoize
    #    ) # memoize
        => __PACKAGE__ );
  #  $N{$memoized} = $N{$subref}; # memoize
  #  delete $N{$subref}; # memoize
  #  $blessed;
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
        %{$b} = %{$in};
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
