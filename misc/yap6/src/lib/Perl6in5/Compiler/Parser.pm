use strict 'refs';
use warnings;
no warnings qw{ reserved closure recursion };

package Perl6in5::Compiler::Parser;

use Exporter;
@EXPORT_OK = qw(hit eoi nothing debug star opt
                say all one flatten newline
                trace %N parser check execnow $Nothing
                ch w keyword keywords panic p6ws
                unspace optws manws opttws mantws through
                plus both match unmore );
@ISA = 'Exporter';
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

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

my $toodeep = 0;

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

sub normalize_parser { # memoize
    # stringify the contents of $in and the coderef # memoize
    # of the continuation. # memoize
    return 'noargs' unless (defined $_[0] && ref($_[0]) eq 'HASH'); # memoize
    my $msg = left($_[0]); # memoize
    $msg .= "$_[1]" if defined($_[1]); # memoize
    $msg; # memoize
} # memoize

# memoize the terminal parser constructors
use Memoize; # memoize
map { memoize $_ } qw{ newline p6ws nothing }; # memoize
# the other benefit of this is that we don't get more than one of 
# each kind of parser constructed, which means the memoization of the
# generated functions will be more efficient than otherwise.

$| = 1; # trace

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
sub parser (&) { bless( 
  #  memoize( # memoize
        $_[0]
   #     , NORMALIZER=>'normalize_parser') # memoize
        => __PACKAGE__ ) }

sub dump1 {
#  return '';
{ local $Data::Dumper::Deparse = 1;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 0;
$Data::Dumper::Useqq = 0;
$Data::Dumper::Quotekeys = 0;
    "dumper from ".Dumper([caller()])."\n".join("\n", map eval{$N{$_}||'none'}, grep $_,@_).Dumper(@_); }
}

sub trace ($$) { # trace
  my $level = shift; # trace
  return unless $level <= 'tracelevel'; # trace
  my $msg = (shift) . "\n"; # trace
  my $i = 0; # trace
  $i++ while caller($i); # trace
  $I = "-" x int($i/2-1); # trace
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
  $I = "-" x int($i/2-1); # debug
  print STDERR $i.$I." ", $msg; # debug
} # debug

sub eoi () {
  my $p;
  my $tmp = $p = parser {
      my ($in) = @_;
      trace 4,"Looking for EOI in '".Dumper(left($_[0]))."'";
      if (left($in) eq "") {
        trace 5,"Found EOI";
        return [ '' , [ 'EOI'] ]; # this is the tail of the AST
      } else {
        trace 5,"Found unparsed input: '".Dumper(left($_[0]))."'";
        return {left=>left($in),expected=>'EOI'};
      }
  };
  $N{$p} = "eoi"; # trace
  weaken($p);
  $p;
}

our $Nothing = sub {
    if (left($_[0]) eq "") { # trace
        trace 5,"Next token is '".Dumper(left($_[0]))."'";
    } else { # trace
        trace 5,"(At end of input)";
    } # trace
    # nothing is the only base-parser that returns the 
    # result of its continuation untouched. Nearly always
    # the continuation will be eoi()
    $_[1]->($_[0]);
};
# junk for eta-conversion
memoize $Nothing; # memoize
bless( $Nothing => __PACKAGE__ );
sub nothing () { $Nothing };

sub p6ws () {
  trace 6,"generating p6ws ".Dumper([caller()]).Dumper(\@_);
  my $p;
  my $tmp = $p = plus(one(hit(" "),hit("\n")));
  weaken($p);
  $N{$p} = ' ws '; # trace
  $p;
}

sub unmore {
    my $q = $_[0];
    trace 1,Dumper($q);
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
            return {expected=>'udder_end_compleat_feelure',left=>left($in2)};
        };
        weaken($o);
        $N{$o} = "fake($N{$p})"; # trace
        my $r = $q->($in,$o);
        return $r unless ref $r eq 'ARRAY';
        # send it along its merry way to the nothing parser, which
        # always succeeds, with the original input and continuation.
        $cont->($in);
    };
    weaken($p);
    $N{$p} = "fail($N{$q})"; # trace
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
        $N{$o} = "to($N{$p})"; # trace
        # test $in on $q; letting it continue with the
        # original input on success.
        $q->($in,$o);
    };
    weaken($p);
    $N{$p} = "iff($N{q})"; # trace
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
        my ($r) = (left($in) =~ $q) or return {left=>left($in),expected=>"$q"};
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
    $N{$p} = "match($q)"; # trace
    $p;
}

sub left {
    substr($_[0]->{inp},$_[0]->{pos})
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
        trace 5,"hit got '".Dumper($in)."'"
        my $l = length($want);
        $in->{want} = $want x $count;
        return err($in,"search string was empty") unless $l;
        my $tier = []; # the new tier in the AST
        for my $i (1..$count) {
            return err($in,"mismatched: '$want'") 
                unless substr(left($_[0]),$l*($i-1),$l*$i) eq $want;
            push @$tier,$want;
        }
        $in->{pos} += $l * $count;
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
        $cont->($in);
    };
    weaken($p);
    $N{$p} = $want x $count; # trace
    $p;
}

sub err{
    my $a = { %{$_[0]},err=>$_[1]};
    $a;
}

sub one {
  my @p = grep $_,@_;
  return parser { return () } if @p == 0;
  return $p[0]             if @p == 1;
  my $p;
  my $tmp = $p = parser {
    my ($in,$cont) = @_;
    my $r;
    trace 3,"Match one: $N{$p}";
    if (defined $in) { # trace
      trace 3,"Next token is ".left($in);
    } else { # trace
      trace 3,"At end of input";
    } # trace
    $in = [$in] unless ref $in;
    my ($q, $np) = (0, scalar @p); # trace
    my ($v,$w,$z);
    for (@p) {
      $q++; # trace
      trace 2,"Trying $q/$np: ".$N{$p[$q-1]};
      $w = $_->($in,$cont);
      trace 4,"one: ".$N{$p[$q-1]}." returned ".Dumper($w);
      if (ref($w) eq 'HASH') {
        trace 2,"Failed $q/$np: ".$N{$p[$q-1]};
        # send back the shortest remaining input if none of them succeed
        %{$z} = %{$w} if (defined($w->{left}) && $w->{left} !~ /^\s+$/ && length($w->{left}) > 0 && (!defined $z || (length($w->{left}) < length($z->{left}))));
      } else {
        trace 3,"Matched $q/$np: ".$N{$p[$q-1]};
        $v = $w if (!defined $v);
      }
      $w = undef;
    }
    return $v if defined $v;
    trace 3,"Failed to match any of: $N{$p}".Dumper($z);
    trace 5,"sending back: ".Dumper($z);
    $z;
  };
  trace 6,"one ".dump1(@p);
  weaken($p);
  $N{$p} = "one(" . join("|", map $N{$_}, @p) . ")"; # trace
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
    trace 5,"both $N{$p} on $in at depth $i"; # trace
    my $loc = left($in);
    $loc ||= 'persnickity';
    if (# we've been in this parser before
        exists $cdepth->{$p}
        # we've seen this input for this parser last
        && $cdepth->{$p} eq $loc) {
        # we're in an infinite recursion.
        trace 3,"$N{$p} was too deep (>$toodeep) in itself.";
        return {left=>left($in),expected=>''};
    } else { # trace
        trace 5,"we're not in an infinite recursion";
    }
    # store the coderef addresses for this
    # parser so we can detect infinite loops
    $cdepth->{$p} = $loc;
    my ($aval,$bval);
    my $BC;
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
    $N{$BC} = $N{$B}.'.'.$N{$cont}; # trace
    trace 2,"both attempting $N{$A} then $N{$BC}";
    $aval = $A->($in, $BC, $u);
    # $aval won't be an AST if $bval wasn't an AST.
    if (ref($aval) ne 'ARRAY') {
      trace 2,"Failed to match at least one of $N{$A} and $N{$BC}";
      return $aval;
    }
    trace 2,"Finished matching $N{$A} and $N{$BC}";
    return [[head($aval), head(tail($aval))], tail($aval)];
  };
  trace 5,"both ".dump1($A,$B);
  weaken($p);
  $N{$p} = $N{$A}.$wsrule.$N{$B}; # trace
  $p;
}

sub all {
  my @p = grep $_,@_;
  return nothing if @p == 0; # this should never occur
  return $_[0]  if @p == 1; # the base case for this function
  my $head = shift @p;
  my $p;
  my $tmp = $p = both($head, all(@p), '.');
  trace 5,"all ".dump1($head,@p,$p);
  weaken($p);
  $N{$p} = "all($N{$head},".join(",",map($N{$_},@p)).")"; # trace
  $p;
}

sub star {
    my ($p, $conc, $p_exec);
    $p_exec = parser { $p->(@_) };
    $N{$p_exec} = "($N{$_[0]})+"; # trace
    $conc = both($_[0], $p_exec, '.');
    $N{$conc} = "($N{$_[0]}.($N{$_[0]})*|nothing)"; # trace
    my $tmp = $p = one($conc, nothing);
    weaken($p);
    $N{$p} = "($N{$_[0]})*"; # trace
    $p;
}

sub opt {
  my $p;
  my $tmp = $p = one($_[0], nothing);
  weaken($p);
  $N{$p} = "?($N{$_[0]})"; # trace
  $p;
}

sub unspace () {
    my $p;
    my $tmp = $p = all(hit("\\"),star(p6ws));
    weaken($p);
    $N{$p} = 'unspace'; # trace
    $p;
}

sub w { # look for a wrapped entity.  first parm is split into the wrappers.
    my ($d,$e) = split(//,$_[0]);
    my $p;
    my $tmp = $p = optws(optws(hit($d),$_[1]),hit($e));
    weaken($p);
    $N{$p} = "$d$N{$_[1]}$e"; # trace
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
    $N{$p} = "$N{$_[0]}-$N{$_[1]}"; # trace
  } else {
    # unary -, so optional leading whitespace
    $tmp = $p = both(opt(p6ws), $_[0], '.');
    weaken($p);
    $N{$p} = " -($N{$_[0]})"; # trace
  }
  $p;
}

sub manws {
  my ($p,$tmp);
  if (defined($_[1])) {
    # binary +, so mandatory intervening  whitespace
    $tmp = $p = all($_[0], p6ws, $_[1]);
    weaken($p);
    $N{$p} = "$N{$_[0]}+$N{$_[1]}"; # trace
  } else {
    # unary +, so mandatory leading whitespace
    $tmp = $p = both(p6ws, $_[0], '.');
    weaken($p);
    $N{$p} = " +($N{$_[0]})"; # trace
  }
  $p;
}

sub opttws {
  my $p;
  # optional trailing whitespace
  my $tmp = $p = both($_[0], opt(p6ws), '.');
  weaken($p);
  $N{$p} = "($N{$_[0]})-- "; # trace
  $p;
}

sub mantws {
  # mandatory trailing whitespace
  my $p;
  my $tmp = $p = both($_[0], p6ws, '.');
  weaken($p);
  $N{$p} = "($N{$_[0]})++ "; # trace
  $p;
}

sub newline () {
  my $p;
  my $tmp = $p = plus(one(panic(hit("\n#{"),"\\n#{ is illegal"),opttws(optws(hit("\n")))));
  #my $tmp = $p = plus(opttws(optws(hit("\n"))));
  weaken($p);
  $N{$p} = "\\n"; # trace
  $p;
}

# plus($s) = /^s+$/
sub plus {
  my $p;
  my $tmp = $p = both($_[0], star($_[0]),'.');
  weaken($p);
  $N{$p} = "($N{$_[0]})+ "; # trace
  $p;
}

sub nthru {
  my ($n,$o) = @_;
  my $p;
  my $tmp = $p = both($n, thru($o),'.');
  weaken($p);
  $N{$p} = "$N{$n}...$N{$o}"; # trace
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
                return [{expected=>$stop,found=>[undef],line=>'',file=>''}] unless defined $in2; # trace
                next;
            }
        }
        my $len;
        $len += length($_) foreach @values;
        trace 4,"Through token matched";
        $cont->(tail($in),undef,{eaten=>$len,ast=>[@values,$u]});
    };
    weaken($p);
    $N{$p} = "{*}.".$N{$stop}; # trace
    $p;
}

sub keyword {
    my $p;
    my $tmp = $p = hit($_[0]);
    weaken($p);
    $N{$p} = "$_[0]"; # trace
    $p;
}

sub keywords {
    my $p;
    my $tmp = $p = one(map(hit($_),@_));
    weaken($p);
    $N{$p} = join("|",@_); # trace
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
    $N{$p} = "panic($N{$ins})"; # trace
    $p;
}

1;
