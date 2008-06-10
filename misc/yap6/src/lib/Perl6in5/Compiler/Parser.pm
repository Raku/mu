use strict 'refs';
use warnings;
no warnings qw{ reserved closure recursion };

package Perl6in5::Compiler::Parser;

use Exporter;
@EXPORT_OK = qw(hit eoi nothing T V error debug star opt
                display_failures say all one flatten
                trace %N parser check execnow $Nothing
                ch w keyword keywords word panic p6ws
                unspace optws manws opttws mantws through
                newline plus both match );
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

use Perl6in5::Compiler::Stream 'node', 'head', 'tail', 'promise', 'drop';

my $toodeep = 2;

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
    # stringify the contents of $input and the coderef # memoize
    # of the continuation. # memoize
    return 'noargs' unless (defined $_[0] && ref($_[0]) eq 'ARRAY'); # memoize
    #debug( Dumper([caller(3),caller(2),caller(1),caller(0)]).dump1(@_) ) unless defined $_[0]; # memoize
    my $msg = join('',(grep $_,@{$_[0]})); # memoize
    $msg .= "$_[1]" if defined($_[1]); # memoize
    $msg; # memoize
} # memoize

# memoize the terminal parser constructors
use Memoize;
map { memoize $_ } qw{ newline p6ws nothing flatten }; # memoize
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
    memoize( # memoize
        $_[0]
        , NORMALIZER=>'normalize_parser') # memoize
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
  trace 6,"generating eoi ".Dumper([caller()]).Dumper(\@_);
  my $p = parser {
  my ($input) = @_;
      trace 4,"Looking for EOI in ".Dumper($input);
      unless (defined($input) && defined($input->[0])) {
        trace 5,"Found EOI";
        return [ '' , [ 'EOI'] ]; # this is the tail of the AST
      } else {
        trace 5,"Found unparsed input: ".Dumper(flatten($input));
        return {left=>flatten($input),expected=>'EOI'};
      }
  };
  $N{$p} = "eoi"; # trace
  $p;
}

our $Nothing = sub {
    if (defined $_[0]) { # trace
        trace 5,"Next token is ".Dumper($_[0]->[0]);
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
  my $p = plus(one(ch(" "),ch("\n")));
  $N{$p} = ' _ '; # trace
  $p;
}

sub unmore {
    my $q = shift;
    # this parser acts like a NOT gate
    my ($p);
    $p = parser {
        my ($input,$cont) = @_;
        my $o = parser {
            # the inverse of the nothing parser; this always fails.
            # so, we know if $q returns the below string, $q
            # succeeded.  Otherwise, we'll get some other error
            # hashref from q.
            return 'udder_end_compleat_feelure';
        };
        $N{$o} = "fake($N{$p})"; # trace
        my $r = $q->($input,$o);
        return "fail() matched, so returning" if !ref $r;
        # send it along its merry way to the nothing parser, which
        # always succeeds, with the original input and continuation.
        nothing->($input,$cont);
    };
    $N{$p} = "fail($N{q})"; # trace
    $p;
}

sub iff {
    my $q = shift;
    my ($p);
    $p = parser {
        my ($input,$cont) = @_;
        my $o = parser {
            # $q must have succeeded, so send the original
            # input through nothing to the original continuation
            nothing->($input,$cont);
        };
        $N{$o} = "to($N{$p})"; # trace
        # test $input on $q; letting it continue with the
        # original input on success.
        $q->($input,$o);
    };
    $N{$p} = "iff($N{q})"; # trace
    $p;
}

sub match {
    my $q = shift;
    # $q is a regular expression of the form:
    # qr/^(match_pattern)/  if you want to eat input returned by the match
    # qr/match_pattern/ if you don't want it to eat input (or wrap it in iff())
    # note that you must include the ^ when eating input or you will get
    # incorrect results, b/c match() uses the length of the captured group
    # to eat the proper length of input.  Make sure your REs aren't greedy...
    my ($p);
    $p = parser {
        my ($input,$cont) = @_;
        my ($r) = (flatten($input) =~ $q) or return {left=>flatten($input),expected=>"$q"};
        my $i = $input;
        if (length($i)) { shift @$i for (1..length($r)) }
        $cont->($i);
    };
    $N{$p} = "match($q)"; # trace
    $p;
}

sub hit {
  trace 6,"generating hit ".Dumper([caller()]).Dumper(\@_);
  my ($wanted,$value) = ([@_],undef,undef);
  $value = sub { $_[0] } if !defined($value);
  $value = [$value] unless ref $value;
  $wanted = [$wanted] unless ref $wanted;
  my $p;
  $p = parser {
    my ($input,$cont) = @_;
    trace 5," after this, will run ".$N{$cont}.Dumper([caller()]).dump1($cont);
    trace 3,"Looking for token ".Dumper($wanted)." in ".Dumper(head($input));
    unless (defined $input && defined(head($input))) {
      trace 4,"Premature end of input";
      my $msg = $wanted->[0];
      $msg .= $wanted->[1] if defined $wanted->[1];
      return {left=>'EOI',expected=>$msg};
    }
    my $next = head($input);
    $next = [$next] unless ref $next;
    for my $i (0 .. $#$wanted) {
      next unless defined $wanted->[$i];
      unless ($wanted->[$i] eq $next->[$i]) {
        trace 4,"Token mismatch";
        return {left=>flatten($input),expected=>$wanted->[$i]};
      }
    }
    # probably should re-add this later for stuff
    #my $wanted_value = $value->($next, $v);
    
    # hit() and eoi() are the only terminal parsers in our
    # parsing system.
    
    # the head of the return is the successfully matched string 
    # returning so far from this branch.  Each appending
    # parser prepends their output, returns it in the head
    # and returns the new AST in the tail.
    $r = $cont->(tail($input));
    if (ref $r ne 'ARRAY') {
        trace 4,"failed";
        return $r;
    }
    [ flatten($input), [ $next->[1] , tail($r) ]];
  };
  $N{$p} = Dumper($wanted); # trace
  $p;
}

sub one {
  trace 6,"generating one ".Dumper([caller()]).dump1(@_);
  my @p = grep $_,@_;
  return parser { return () } if @p == 0;
  return $p[0]             if @p == 1;
  my $p;
  $p = parser {
    my ($input,$cont) = @_;
    my $r;
    trace 3,"Match one: $N{$p}";
    if (defined $input) { # trace
      trace 3,"Next token is ".Dumper($input->[0]);
    } else { # trace
      trace 3,"At end of input";
    } # trace
    $input = [$input] unless ref $input;
    my ($q, $np) = (0, scalar @p); # trace
    my ($w,$z);
    for (@p) {
      $q++; # trace
      trace 2,"Trying $q/$np: ".$N{$p[$q-1]};
      $w = $_->($input,$cont);
      trace 4,"one: ".$N{$p[$q-1]}." returned ".Dumper($w);
      if (ref($w) ne 'ARRAY') {
        trace 2,"Failed $q/$np: ".$N{$p[$q-1]};
        # send back the shortest remaining input if none of them succeed
        %{$z} = %{$w} if (defined($w->{left}) && $w->{left} !~ /^\s+$/ && length($w->{left}) > 0 && (!defined $z || (length($w->{left}) < length($z->{left}))));
      } else {
        trace 3,"Matched $q/$np: ".$N{$p[$q-1]};
        return $w;
      }
    }
    trace 3,"Failed to match any of: $N{$p}".Dumper($z);
    trace 5,"sending back: ".Dumper($z);
    return $z;
  };
  trace 6,"one ".dump1(@p);
  $N{$p} = "one(" . join("|", map $N{$_}, @p) . ")"; # trace
  $p;
}

my $cdepth = {};

sub both {
  trace 6,"generating both ".Dumper([caller()]).dump1($_[0],$_[1]);
  my ($A, $B, $wsrule) = @_;
  $wsrule ||= '.';
  my $p;
  $p = parser {
    my ($input, $cont) = @_;
    my $i = 0;
    $i++ while caller($i);
    trace 5,"both $p on $input at depth $i"; # trace
    my $loc = $input;
    $loc ||= 1;
    if (# we've been in this parser before
        exists $cdepth->{$p}
        # we've seen this input for this parser before
        && exists $cdepth->{$p}->{$loc}
        # our current depth is lower than before
        && $cdepth->{$p}->{$loc} > $toodeep) {
        # we're in an infinite recursion.
        trace 3,"$N{$p} was too deep (>$toodeep) in itself.";
        return {left=>flatten($input),expected=>''};
    } else { # trace
        trace 5,"we're not in an infinite recursion"; # trace
    }
    # store the coderef addresses for this
    # parser so we can detect infinite loops
    $cdepth->{$p}->{$loc}++;
    my ($aval,$bval);
    my $BC;
    $BC = parser {
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
    $N{$BC} = $N{$B}.'.'.$N{$cont}; # trace
    trace 2,"both attempting $N{$A} then $N{$BC}";
    $aval = $A->($input, $BC, $u);
    # $aval won't be an AST if $bval wasn't an AST.
    if (ref($aval) ne 'ARRAY') {
      trace 2,"Failed to match at least one of $N{$A} and $N{$BC}";
      return $aval;
    }
    trace 2,"Finished matching $N{$A} and $N{$BC}";
    return [head($aval), head(tail($aval)), tail(tail($aval))];
  };
  trace 5,"both ".dump1($A,$B);
  $N{$p} = $N{$A}.$wsrule.$N{$B}; # trace
  $p;
}

sub all {
  trace 6,'generating all '.Dumper([caller()]).dump1(@_);
  my @p = grep $_,@_;
  return nothing if @p == 0; # this should never occur
  return $_[0]  if @p == 1; # the base case for this function
  my $head = shift @p;
  my $p = both($head, all(@p), '.');
  $N{$p} = "all($N{$head},".join(",",map($N{$_},@p)).")"; # trace
  trace 5,"all ".dump1($head,@p,$p);
  $p;
}

sub star {
    trace 6,'generating star '.Dumper([caller()]).Dumper(\@_);
    my $p = shift;
    my ($p_star, $conc, $p_exec);
    $p_exec = parser { $p_star->(@_) };
    $N{$p_exec} = "($N{$p})+"; # trace
    $conc = both($p, $p_exec, '.');
    $N{$conc} = "($N{$p}.($N{$p})*|nothing)"; # trace
    $p_star = one($conc, nothing);
    $N{$p_star} = "($N{$p})*"; # trace
    $p_star;
}

sub opt {
  trace 6,'generating opt '.Dumper([caller()]).Dumper(\@_);
  my $p = shift;
  my $p_opt = one($p, nothing);
  $N{$p_opt} = "?($N{$p})"; # trace
  $p_opt;
}

sub ch { # parse for a single (normal) character.
    trace 6,'generating ch '.Dumper([caller()]).Dumper(\@_);
    # Because of the weirdness of unspace, grammars that
    # need to look for backslashes will need to use hit("C",'\\')
    my $p;
    my $char = $_[0];
    # through(opt(something)) is an interesting construct.
    # It means chew/slurp stuff through something if something
    # is the first token (series).  Since opt() succeeds with
    # nothing(), through will only ever chew 1 match.
    #$p = all(through(opt(unspace)),hit("C",$char));
    $p = hit("C",$char);
    $N{$p} = Dumper($char); # trace
    $p;
}

sub unspace () {
    trace 6,'generating unspace '.Dumper([caller()]).Dumper(\@_);
    my $p;
    $p = all(ch("\\"),star(p6ws));
    $N{$p} = 'unspace'; # trace
    $p;
}

sub w { # look for a wrapped entity.  first parm is split into the wrappers.
    trace 6,'generating w '.Dumper([caller()]).Dumper(\@_);
    my ($d,$e) = split(//,$_[0]);
    my $p;
    my $item = $_[1];
    $p = optws(optws(ch($d),$item),ch($e));
    $N{$p} = "$d$N{$item}$e"; # trace
    $p;
}

# optws and manws always receive 1 or 2 arguments when called
# by the overloaded operators in grammars
# (so that it always gets two arguments)
#   from within hand-written parser generators, write them out
#   explicitly, or always pass them two arguments :)

sub optws {
  trace 6,"generating optws ".Dumper([caller()]).Dumper(\@_);
  my $p;
  if ($_[1]) {
    # binary -, so optional intervening whitespace
    $p = all($_[0], opt(p6ws), $_[1]);
    $N{$p} = "$N{$_[0]}-$N{$_[1]}"; # trace
  } else {
    # unary -, so optional leading whitespace
    $p = both(opt(p6ws), $_[0], '.');
    $N{$p} = " -($N{$_[0]})"; # trace
  }
  $p;
}

sub manws {
  trace 6,"generating manws ".Dumper([caller()]).Dumper(\@_);
  my $p;
  my @i = @_;
  if (defined($i[1])) {
    # binary +, so mandatory intervening  whitespace
    $p = all($i[0], p6ws, $i[1]);
    $N{$p} = "$N{$i[0]}+$N{$i[1]}"; # trace
  } else {
    # unary +, so mandatory leading whitespace
    $p = both(p6ws, $i[0], '.');
    $N{$p} = " +($N{$i[0]})"; # trace
  }
  $p;
}

sub opttws {
  trace 6,"generating opttws ".Dumper([caller()]).Dumper(\@_);
  my $p;
  # optional trailing whitespace
  $p = both($_[0], opt(p6ws), '.');
  $N{$p} = "($N{$_[0]})-- "; # trace
  $p;
}

sub mantws {
  trace 6,"generating mantws ".Dumper([caller()]).Dumper(\@_);
  # mandatory trailing whitespace
  my $p = both($_[0], opt(p6ws), '.');
  $N{$p} = "($N{$_[0]})++ "; # trace
  $p;
}

sub newline () {
  trace 6,"generating newline ".Dumper([caller()]).Dumper(\@_);
  my $p;
#  $p = plus(one(panic(word("\n#{"),"\\n#{ is illegal"),opttws(optws(ch("\n")))));
  $p = plus(opttws(optws(ch("\n"))));
  $N{$p} = "\\n"; # trace
  return $p;
}

# plus($s) = /^s+$/
sub plus {
  trace 6,"generating plus ".Dumper([caller()]).Dumper(\@_);
  my $parser = shift;
  my $p = all($parser, star($parser));
  $N{$p} = "($N{$p})+ "; # trace
  $p;
}

sub nthru {
  trace 6,"generating nthru ".Dumper([caller()]).Dumper(\@_);
  my ($n,$o) = @_;
  my $p;
  $p = both($n, thru($o),'.');
  $N{$p} = "$N{$n}...$N{$o}"; # trace
  $p;
}

# slurp up stuff until a stopper parser matches. basically, the {*} signifier.
# if you want to slurp up stuff *up until but stopping short of* the stopper,
# wrap the stopper in iff().  if-and-only-if, get it?
sub thru {
  trace 6,'generating through'.Dumper([caller()]).Dumper(\@_);
    my $stop = shift;
    my $p;
    $p = parser {
        my ($input,$cont,$u) = @_;
        my $v;
        my @values;
        my $input2=$input;
        while (defined $input2) {
            $v = $stop->($input2);
            if (ref($v) eq 'HASH') {
                trace 5,"through $N{$stop} matched";
                push @values, $v;
                last;
            } else {
                trace 5,"through $N{$stop} still not matched";
                push @values, drop($input2);
                return [{expected=>$stop,found=>[undef],line=>'',file=>''}] unless defined $input2; # trace
                next;
            }
        }
        my $len;
        $len += length($_) foreach @values;
        trace 4,"Through token matched";
        $cont->(tail($input),undef,{eaten=>$len,ast=>[@values,$u]});
    };
    $N{$p} = "{*}.".$N{$stop}; # trace
    $p;
}

sub keyword {
    trace 6,'generating keyword '.Dumper([caller()]).Dumper(\@_);
    my $ins = shift;
    my $p = hit('ID',$ins);
    $N{$p} = "$ins"; # trace
    $p;
}

sub keywords {
    trace 6,'generating keywords '.Dumper([caller()]).Dumper(\@_);
    my @args = @_;
    my $p = one(map(keyword($_),@args));
    $N{$p} = join("|",@args); # trace
    $p;
}

sub panic {
    trace 6,'generating panic '.Dumper([caller()]).Dumper(\@_);
    my ($ins,$msg) = @_;
    my $p = parser {
        my $input = shift;
        trace 4,"Dying if find $N{$ins}";
        if (defined $input) { # trace
            trace 4,"Next token is ".Dumper($input->[0]);
        } else { # trace
            trace 4,"At end of input";
        } # trace
        $input = [$input] unless ref $input;
        my ($v, $newinput);
        eval { ($v, $newinput) = $ins->($input) };
        if ($@) {
            die ['panic', $input, [[], $@]]; 
        } else {
            trace 4,"Matched $N{$ins}, so dying";
            die $msg;
        }
    };
    $N{$p} = "panic($N{$ins})"; # trace
    $p;
}

sub word {
    trace 6,'generating word '.Dumper([caller()]).Dumper(\@_);
    my $word = $_[0];
    my $p = all(map(ch($_),split(//, $word)));
    $N{$p} = Dumper($word); # trace
    $p;
}

sub flatten {
    my ($s,$y,$h,$v) = ($_[0], '', undef, undef);
    return '' unless defined $s;
    while (ref($s) eq 'ARRAY' && defined ($h = head($s)) && defined ($v = $h->[1])) {
        $y .= $v;
        $s = tail($s);
    }
    $y;
}

# Only suitable for applying to concatenations
# (because their input parsers return(ed) an array)
sub T {
  my ($parser, $transform) = @_;
  my $p;
  $p = parser {
    my $input = shift;
    trace 4,"Going to transform with $N{$parser}";
    my ($value, $newinput) = $parser->($input);
    trace 4,"Transforming value produced by $N{$parser}";
    trace 4,"Input to $N{$parser}:  ". Dumper($value);
    $value = $transform->(@$value);
    trace 4,"Output from $N{$parser}: ". Dumper($value);
    return ($value, $newinput);
  };
  $N{$p} = $N{$parser}; # trace
  return $p;
}

# suitable for applying to things returning a single value
sub V {
  my ($parser, $transform) = @_;
  my $p = parser {
    my $input = shift;
    my ($value, $newinput) = $parser->($input);
    trace 4,"Vransforming value produced by $N{$parser}";
    trace 4,"Input to $N{$parser}:  ". Dumper($value);
    $value = $transform->($value); 
    trace 4,"Output from $N{$parser}: ". Dumper($value);
    return ($value, $newinput);
  };
  $N{$p} = $N{$parser}; # trace
  return $p;
}

# this generates a parser that checks the return of a
#     parser (its first argument) using its second argument,
#     which is a coderef that tests validity of a given
#     output against whatever standards.  Could be used
#     in yap6 as part of (both pos and neg) lookahead
sub check {
  my ($parser, $condition) = @_;
  my $p = parser {
    my $input = shift;
    my ($val, $newinput) = $parser->($input);
    return ($val, $newinput) if ($condition->($val));
  };
  $N{$p} = exists($N{$condition})?"check($N{$parser},$N{$condition})": # trace
    "check($N{$parser},condition)"; # trace
  $p;
}

# same as checkval, except it doesn't pass the input through
#     a specified parser first.  the testing routine can implement
#     its own parsing, of course.  
sub test {
  my $action = shift;
  my $p = return parser {
    my $input = shift;
    my $result = $action->($input);
    return $result ? (undef, $input) : ();
  };
  $N{$p} = exists($N{$action})?"test($N{$action})":"test(condition)"; # trace
  $p;
}

sub error {
  my ($try) = @_;
  my $p;
  $p = parser {
    my $input = shift;
    my @result = eval { $try->($input) };
    if ($@) {
      my $trace = 0;
      $trace = 1; # trace
      my $msg = Dumper($@);
      if ($msg =~ / at /) {
          if (!$trace) {
          $msg =~ s/ at .*//;
          $msg =~ s/^.*propagated.*$//mg;
          $msg =~ s/\n//mg; }
          print "Illegal usage: ".$msg;
      }
      display_failures($@) if ref $@;
      die $msg;
    }
    return @result;
  };
  $N{$p} = $N{$try}; # trace
  $p;
}

sub display_failures {
  my ($fail, $depth) = @_;
  my $xx = 0;
  $xx = 1; # trace
  #return unless $xx;
  $depth ||= 0;
  my $I = " " x $depth;
  unless (ref $fail) { die $fail }
  my ($type, $position, $data) = @$fail;
  my $pos_desc = "";
  while (length($pos_desc) < 80) {
    if ($position) {
      my $h = head($position);
      if (!defined $h) {
        $pos_desc .= "End of input ";
        last;
      }
      $h = [$h] unless ref $h;
      $pos_desc .= "$h->[1]";
    } else {
      $pos_desc .= "End of input ";
      last;
    }
    $position = tail($position);
  }
  chop $pos_desc;
  $pos_desc .= "..." if defined $position;
  if ($type eq 'TOKEN') {
    print $I, "Wanted [@$data] instead of ".Dumper($pos_desc);
  } elsif ($type eq 'End of input') {
    print $I, "Wanted EOI instead of ".Dumper($pos_desc);
  } elsif ($type eq 'ALT') {
    print $I, ($depth ? "Or one" : "Any"), " of the following:\n";
    for (@$data) {
      display_failures($_, $depth+1);
    }
  } elsif ($type eq 'CONC') {
    my ($succeeded, $subfailure) = @$data;
    print $I, "After ".Dumper("@$succeeded").", got ".Dumper($pos_desc)." instead of: ";
    display_failures($subfailure, $depth+1);
  } else {
    die "Unknown failure type '$type'";
  }
}

1;
