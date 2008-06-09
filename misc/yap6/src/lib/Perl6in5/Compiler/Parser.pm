use strict 'refs';
use warnings;
no warnings qw{ reserved closure recursion };

# Library based on chap09/arith25.pl

package Perl6in5::Compiler::Parser;

use Exporter;
@EXPORT_OK = qw(hit eoi nothing T V error debug star opt
                display_failures o say all any
                trace %N l parser checkval execnow
                ch w keyword keywords word panic p6ws
                unspace optws manws opttws mantws through
                newline plus both);
@ISA = 'Exporter';
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

use Perl6in5::Compiler::Trace; # set env var TRACE for trace output
# disregarding leading whitespace, lines that start with trace
# or end with trace are discarded by the source filter in 
# Perl6in5::Compiler::Trace.  This allows execution time to be
# much faster than otherwise, since the strings to be traced
# are not only not sent to STDOUT; they aren't even built.

use Perl6in5::Compiler::Stream 'node', 'head', 'tail', 'promise', 'drop';

my $toodeep = 2;

use overload
    '-'  => \&optws, # optional leading or intervening whitespace
    '+'  => \&manws, # mandatory leading or intervening whitespace
    '.'  => \&all, # intervening whitespace disallowed
    '|'  => \&any, # defaultly longest token matching
    '&'  => \&any_other, # could use this for non-eating ltm
                         # which could also be peekahead. or
                         # first match.
    '...'=> \&and_through, # both($_[0],through($_[1]))
    '>>' => \&T, # unused currently
    '>'  => \&V, # unused currently
    '/'  => \&checkval, # unused currently
    '""' => \&overload::StrVal,  # this helps stringify parser names for %N
  ;

# Here's a fun trick; memoize the parser generator functions :)
use Memoize;
map { memoize $_ unless (/^\W/ || /^trace$/) } qw{ newline p6ws nothing };

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

sub parser (&) { bless $_[0] => __PACKAGE__ }

sub l { @_ = [@_]; goto &hit }

sub o { goto &opt }

sub dump1 {
{ local $Data::Dumper::Deparse = 1;
    "dumper from ".Dumper([caller()])."\n".join("\n", map $N{$_}, @_).Dumper(@_); }
}

sub trace ($) { # trace
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
  trace $msg;
} # trace

sub debug ($) { # debug
  my $msg = (shift) . "\n"; # debug
  my $i = 0; # debug
  $i++ while caller($i); # debug
  $I = "-" x int($i/2-1); # debug
  print $i.$I." ", $msg; # debug
} # debug

sub hit {
  #trace "generating hit ".Dumper([caller()]).Dumper(\@_);
  # an arrayref of a lex category and an expectation.
  # Eventually, this may carry interesting categories such as regex.
  # Think of the "wanted" as the match pattern, and the
  # $value (sub) as the capture-obtainer.
  my ($wanted,$value,$v) = ([@_],undef,undef);
  $value = sub { $_[0] } if !defined($value);
  $value = [$value] unless ref $value;
  # I'm not sure what this is used for; it's sent as the 2nd argument
  # to the obtain-value function.  I guess it could be state storage
  # for macros.
  #my $v = shift;
  # This is necessary for the occasions when l[ookfor]('category') is 
  # passed in on its own without an expected (literal) value.
  $wanted = [$wanted] unless ref $wanted;
  my $parser;
  $parser = parser {
    my ($input,$cont,$u) = @_;
    #trace " after this, will run ".$N{$cont}.Dumper([caller()]).dump1($cont);
    #trace "Looking for token ".Dumper($wanted)." in ".Dumper(head($input));
    unless (defined $input && defined(head($input))) {
      trace "Premature end of input";
      return [{expected=>$wanted,found=>[undef],line=>'',file=>''}];
    }
    my $next = head($input);
    $next = [$next] unless ref $next;
    $wanted = [$wanted] unless ref $wanted;
    for my $i (0 .. $#$wanted) {
      trace "trying subtoken $i";
      next unless defined $wanted->[$i];
      unless ($wanted->[$i] eq $next->[$i]) {
        trace "Token mismatch";
        return [{expected=>$wanted,found=>$next,line=>'',file=>''}];
      }
    }
    my $wanted_value = $value->($next, $v);
    trace "Token matched";
    $cont->(tail($input),undef,{eaten=>length($next->[1]),ast=>[]});
  };
  $N{$parser} = Dumper($wanted); # trace
  $parser;
}

sub eoi () {
  my $p = parser {
  my ($input,$cont,$u) = @_;
      trace "Looking for EOI";
      unless (defined($input) && defined($input->[0])) {
        trace "Found EOI";
        return {eaten=>0,ast=>[]};
      } else {
        trace "Found more input: ".Dumper($input);
        return [{expected=>'EOI',found=>$input,line=>'',file=>''}];
      }
  };
  $N{$p} = "eoi";
  $p;
}

sub nothing () {
    #trace "generating nothing ".Dumper([caller()]).Dumper(\@_);
    my $p = parser {
        my ($input, $cont,$u) = @_;
        if (defined $input) { # trace
            trace "Next token is ".Dumper($input->[0]);
        } else { # trace
            trace "(At end of input)";
        } # trace
        return $cont->($input,undef,{eaten=>0,ast=>[]});
    };
    $N{$p} = "nothing";
    $p;
}

sub any {
  #trace "generating any ".Dumper([caller()]).dump1(@_);
  my @p = grep $_,@_;
  return parser { return () } if @p == 0;
  return $p[0]             if @p == 1;
  my $p;
  $p = parser {
    my ($input,$cont,$u) = @_;
    trace "Longest token match: $N{$p}";
    if (defined $input) { # trace
      trace "Next token is ".Dumper($input->[0]);
    } else { # trace
      trace "At end of input";
    } # trace
    $input = [$input] unless ref $input;
    my ($q, $np) = (0, scalar @p); # trace
    my ($v,$w);
    my @failures;
    for (@p) {
      $q++; # trace
      #trace dump1($_);
      trace "Trying $q/$np: ".$N{$p[$q-1]};
      $w = $_->($input,$cont,$u);
      if (ref($w) ne 'HASH') {
        trace "Failed $q/$np: ".$N{$p[$q-1]};
        push @failures, $w;
      } else {
        $v = $w;
        #if ($w->{eaten} > $v->{eaten}) {
        #    # this is the new longest branch.
        #    $v = $w;
        #    $v->{branch} = $q; # trace
        #}
        trace "Matched $q/$np using ".$w->{eaten}." chars: ".$N{$p[$q-1]};
      }
      $w = undef;
    }
    if ( defined $v ) {
        trace "Match $q/$np selected: [".$N{$p[$q-1]}."] with ".$v->{eaten}." chars eaten.";
        return $v;
    } else {
        trace "Failed to match any of: $N{$p}";
        return [@failures];
    }
  };
  #trace "any ".Dumper(\@p);
  $N{$p} = "any(" . join("|", map $N{$_}, @p) . ")"; # trace
  $p;
}

sub p6ws () {
  #trace "generating p6ws ".Dumper([caller()]).Dumper(\@_);
  my $p;
  $p = plus(any(ch(" "),ch("\n")));
#  $p = ch(" ");
  $N{$p} = 'ws'; # trace
  $p;
}

# slurp up stuff until a stopper parser matches. basically, the {*} signifier.
sub through {
  #trace 'generating through'.Dumper([caller()]).Dumper(\@_);
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
                trace "through $N{$stop} matched";
                push @values, $v;
                last;
            } else {
                trace "through $N{$stop} still not matched";
                push @values, drop($input2);
                return [{expected=>$stop,found=>[undef],line=>'',file=>''}] unless defined $input2; # trace
                next;
            }
        }
        my $len;
        $len += length($_) foreach @values;
        trace "Through token matched";
        $cont->(tail($input),undef,{eaten=>$len,ast=>[@values,$u]});
    };
    $N{$p} = "{*}.".$N{$stop}; # trace
    $p;
}

# optws and manws always receive 1 or 2 arguments when called
# by the overloaded operators in grammars
# (so that it always gets two arguments)
#   from within hand-written parser generators, write them out
#   explicitly, or always pass them two arguments :)

sub optws {
  #trace "generating optws ".Dumper([caller()]).Dumper(\@_);
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
  #trace "generating manws ".Dumper([caller()]).Dumper(\@_);
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
  #trace "generating opttws ".Dumper([caller()]).Dumper(\@_);
  my $p;
  my @i = @_;
  # optional trailing whitespace
  $p = both($i[0], opt(p6ws), '.');
  $N{$p} = "($N{$i[0]})-- "; # trace
  $p;
}

sub mantws {
  #trace "generating mantws ".Dumper([caller()]).Dumper(\@_);
  # mandatory trailing whitespace
  my $p = both($_[0], opt(p6ws), '.');
  $N{$p} = "($N{$_[0]})++ "; # trace
  $p;
}

sub newline () {
  #trace "generating newline ".Dumper([caller()]).Dumper(\@_);
  my $p;
#  $p = plus(any(panic(word("\n#{"),"\\n#{ is illegal"),opttws(optws(ch("\n")))));
  $p = plus(opttws(optws(ch("\n"))));
  $N{$p} = "\\n"; # trace
  return $p;
}

# plus($s) = /^s+$/
sub plus {
  #trace "generating plus ".Dumper([caller()]).Dumper(\@_);
  my $parser = shift;
  my $p = all($parser, star($parser));
  $N{$p} = "($N{$p})+ "; # trace
  $p;
}

my $cdepth = {};

sub both {
  #trace "generating both ".Dumper([caller()]).Dumper(\($_[0],$_[1]));
  my ($A, $B, $wsrule) = @_;
  my $p;
  $p = parser {
    my ($input, $cont, $u) = @_;
    my $i = 0;
    $i++ while caller($i);
    #trace "concatenate $p on $input at depth $i"; # trace
    my $loc = $input;
    $loc ||= 1; # fix '/
    if (# we've been in this parser before
        exists $cdepth->{$p}
        # we've seen this input for this parser before
        && exists $cdepth->{$p}->{$loc}
        # our current depth is lower than before
        && $cdepth->{$p}->{$loc} > $toodeep) {
        # we're in an infinite recursion.
        trace "$N{$p} was too deep (>$toodeep) in itself.";
        return {};
    } else { # trace
        #trace "we're not in an infinite recursion"; # trace
    }
    # store the coderef addresses for this
    # parser so we can detect infinite loops
    $cdepth->{$p}->{$loc}++;
    my ($aval,$bval);
    my $BC = parser {
      trace "in BC";
      # $BC won't get invoked by hit() unless A succeeds.
      $bval = $B->($_[0], $cont, $_[2]); # to $aval
    };
    if ($wsrule eq '.') {
        # leave $A the way it is (this function's base case)
    } elsif ($wsrule eq '+') {
        $A = manws($A)
    } elsif ($wsrule eq '-') {
        $A = optws($A)
    }
    $N{$BC} = $N{$B}.'.'.$N{$cont}; # trace
    trace "both attempting $N{$A} then $N{$BC}";
    $aval = $A->($input, $BC, $u);
    return [$aval,$bval];
  };
  $N{$p} = $N{$A}.$wsrule.$N{$B}; # trace
  $p;
}

sub all {
  #trace 'generating all '.Dumper([caller()]).dump1(@_);
  my @p = grep $_,@_;
  return nothing if @p == 0; # this should never occur
  return $_[0]  if @p == 1; # the base case for this function
  my $head = shift @p;
  my $p = both($head, all(@p), '.');
  $N{$p} = "all($N{$head},".join(",",map($N{$_},@p)).")"; # trace
  #trace "all ".dump1($head,@p,$p);
  $p;
}

sub star {
  #trace 'generating star '.Dumper([caller()]).Dumper(\@_);
  my ($p,$nows) = @_;
  my ($p_star, $conc);
  my $p_starexec = parser { $p_star->(@_) };
  $N{$p_starexec} = ""; # trace
  my $p_sub = all(@_);
  $p_star = opt(($conc = sub { $p_sub } )->($p, $p_starexec));
  $N{$p_star} = "$N{$p}*"; # trace
  $N{$conc} = "$N{$p}.$N{$p_star}"; # trace
  $p_star;
}

sub opt {
  my $p = shift;
  my $p_opt = any($p, nothing);
  $N{$p_opt} = "?($N{$p})"; # trace
  $p_opt;
}

# Only suitable for applying to concatenations
# (because their input parsers return(ed) an array)
sub T {
  my ($parser, $transform) = @_;
  my $p;
  $p = parser {
    my $input = shift;
    trace "Going to transform with $N{$parser}";
    my ($value, $newinput) = $parser->($input);
    trace "Transforming value produced by $N{$parser}";
    trace "Input to $N{$parser}:  ". Dumper($value);
    $value = $transform->(@$value);
    trace "Output from $N{$parser}: ". Dumper($value);
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
    trace "Vransforming value produced by $N{$parser}";
    trace "Input to $N{$parser}:  ". Dumper($value);
    $value = $transform->($value); 
    trace "Output from $N{$parser}: ". Dumper($value);
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
    print $I, ($depth ? "Or any" : "Any"), " of the following:\n";
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

sub ch { # parse for a single (normal) character.
  #trace 'generating ch '.Dumper([caller()]).Dumper(\@_);
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
  #trace 'generating unspace '.Dumper([caller()]).Dumper(\@_);
    my $p;
    $p = all(ch("\\"),star(p6ws));
    $N{$p} = 'unspace'; # trace
    $p;
}

sub w { # look for a wrapped entity.  first parm is split into the wrappers.
    #trace 'generating w '.Dumper([caller()]).Dumper(\@_);
    my ($d,$e) = split(//,$_[0]);
    my $p;
    my $item = $_[1];
    $p = optws(optws(ch($d),$item),ch($e));
    $N{$p} = "$d$N{$item}$e"; # trace
    $p;
}

sub keyword {
    my $ins = shift;
    my $p = hit('ID',$ins);
    $N{$p} = "$ins"; # trace
    $p;
}

sub keywords {
    my @args = @_;
    my $p = any(map(keyword($_),@args));
    $N{$p} = join("|",@args); # trace
    $p;
}

sub panic {
    my ($ins,$msg) = @_;
    my $p = parser {
        my $input = shift;
        trace "Dying if find $N{$ins}";
        if (defined $input) { # trace
            trace "Next token is ".Dumper($input->[0]);
        } else { # trace
            trace "At end of input";
        } # trace
        $input = [$input] unless ref $input;
        my ($v, $newinput);
        eval { ($v, $newinput) = $ins->($input) };
        if ($@) {
            die ['panic', $input, [[], $@]]; 
        } else {
            trace "Matched $N{$ins}, so dying";
            die $msg;
        }
    };
    $N{$p} = "panic($N{$ins})"; # trace
    $p;
}

sub word {
    my $word = $_[0];
    my $p = all(map(ch($_),split(//, $word)));
    $N{$p} = Dumper($word); # trace
    $p;
}

1;
