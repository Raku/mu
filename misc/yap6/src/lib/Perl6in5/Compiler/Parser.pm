#use strict;
use warnings;
no warnings qw{ reserved closure recursion };

package Perl6in5::Compiler::Parser;

use Tie::IxHash;
use Perl6in5::Compiler::Trace;
use Exporter;
our @EXPORT_OK = qw(lit eoi nothing star opt %stat
                say all one newline left ceoi worry
                %N parser check to first iff trace thru
                w keywords panic ws parser2 nthru lits
                unspace optws manws opttws mantws through
                plus both match unmore ow now warning error);
our @ISA = 'Exporter';
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

our %N;

my $tracemode = 0;
$tracemode = 1; # trace

use overload
    '-'  => \&optws, # optional leading or intervening whitespace
    '+'  => \&manws, # mandatory leading or intervening whitespace
    '.'  => \&all, # intervening whitespace disallowed
    '|'  => \&one, # longest token matching by its nature.
    '!'  => \&unmore, # continue on failure, but don't eat input
    '~'  => \&iff, # continue on success, but don't eat input
    '*'  => \&nthru, # both($_[0],through($_[1]))
    '>>' => \&T, # unused currently
    '>'  => \&V, # unused currently
    '^'  => \&first, # short-circuiting one() - alternation. (first match)
    '/'  => \&checkval, # unused currently
    '""' => \&overload::StrVal,  # this helps stringify parser names for %N
;

$| = 1; # trace

use Data::Dumper;
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;
$Data::Dumper::Useqq = 1;
$Data::Dumper::Quotekeys = 0;

sub say (@) { print($_) for @_; print "\n" }

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
  warn " $i $I ", $msg; # trace
} # trace

# ordered hash ref
sub ohr {
    tie my %h, 'Tie::IxHash', @_;
    return \%h;
}

# Randal Schwartz' deep_copy, modified
sub deep_copy {
    my $this = $_[0];
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

our %M;

our %stat;

sub parser (&) {
    my $q = $_[0];
    my $p;
    $p = bless( sub {
        my ($in) = @_;
        my $pos = $in->{'pos'};
        my $m = $M{$q}{$pos};
        if (!defined $m) {
            $stat{memo_misses}++; # trace
            $m = $M{$q}{$pos} = $q->($in);
        } else { # trace
            $stat{memo_hits}++; # trace
        }
        $m;
    } => __PACKAGE__ );
    $N{$p} = "_memoized_parser_"; # trace
    $p;
}

sub eoi {
    my $p;
    $p = parser {
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
            hit=>''
        };
    };
    $N{$p} = "EOI"; # trace
    $p;
}

sub nothing {
    my $p;
    $p = parser {
        $_[0];
    };
    $N{$p} = 'nothing'; # trace
    $p;
}

sub blank () {
    my $p;
    $p = match( qr|^(\s+)| );
    $N{$p} = 'blank'; # trace
    $p;
}

sub ws () {
    my $p;
    $p = plus(one(blank,lit("\n")));
    $N{$p} = 'ws'; # trace
    $p;
}

sub unmore {
    my $q = $_[0];
    # this parser acts like a NOT gate
    my ($p);
    $p = parser {
        my ($in) = @_;
        my $b = $in;
        $in = $q->($in);
        $b->{success} = $in->{success}?0:1;
        $b;
    };
    $N{$p} = "fail( $N{$q} )"; # trace
    $p;
}

sub iff {
    my $q = $_[0];
    my $p;
    $p = parser {
        my ($in) = @_;
        my $b = $in;
        $in = $q->($in);
        $b->{success} = $in->{success}?1:0;
        $b;
    };
    $N{$p} = "iff( $N{$q} )"; # trace
    $p;
}

sub match {
    my $q = $_[0];
    # $q is a regular expression of the form:
    # qr/^(match_pattern)/ or qr/(match_pattern)/
    # note that you must include the ^ or you will get
    # incorrect results, b/c match() uses the length of the captured group
    # to eat the proper length of input.  Probably your REs shouldn't be greedy...
    my $p;
    $p = parser {
        my ($in) = @_;
        $in->{want} = "RE $q";
        if (!(($in->{hit}) = (left($in) =~ $q))) {
            trace 3,"match failed on $N{$p}"; # trace
            return err($in,"$N{$p}"); # trace
            return err($in,"none");
        } else { # trace
            trace 3,"match matched ".$in->{hit}." on $N{$p}"; # trace
        }
        $in->{pos} += length($in->{hit});
        trace 3,"advanced pos by ".length($in->{hit});
        push @{$in->{ast}->[0]},$in->{hit} unless $in->{hit} =~ /^[\s\n]+$/;
        lineify($in);
    };
    $N{$p} = "match( $q )"; # trace
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
    substr($_[0]->{inp},$_[0]->{'pos'})
}

sub lit {
    my ($want,$count) = @_;
    # $want is the desired string.
    # $count is the number of occurrences to match.
    #   Defaults to 1 of course. 0 doesn't make sense.
    $count ||= 1;
    my $p;
    $p = parser {
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
    $N{$p} = Dumper($want x $count); # trace
    $p;
}

sub ceoi {
    left($_[0]) eq ''
}

sub err{
    trace 3,"errored ".Dumper($_[1]);
    return {%{$_[0]},success=>0,expected=>($_[1] || 'unknown')};
}

sub first {
    one( { first => 1 }, @_ );
}

sub one {
    my $opts = (ref $_[0] eq 'HASH')?shift:{};
    my @p = grep $_,@_;
    return nothing if @p == 0;
    return $p[0] if @p == 1;
    my $p;
    $p = parser {
        my ($in) = @_;
        trace 2,"one  ".$in->{'pos'}."  trying >0   ".$N{$p}." on ".Dumper(left($in)); # trace
        my ($q, $np) = (0, scalar @p); # trace
        my ($v,$r,$z);
        my $b = deep_copy($in);
        for (@p) {
            $q++; # trace
            my $c = $b;
            trace 2,"one  ".$in->{'pos'}."  trying $q/$np   ".$N{$p[$q-1]}." on ".Dumper(left($c)); # trace
            $r = $_->($c);
            trace 5,"one  ".$N{$p[$q-1]}." returned ".Dumper($r); # trace
            unless ($r->{success}) {
                trace 2,"one  ".$in->{'pos'}."  failed $q/$np    ".$N{$p[$q-1]}; # trace
                # send back the shortest remaining input if none
                # succeed; this means the input was validly parsed
                # up till then.
                $z = $r if (!defined $z || length(left($r)) < length(left($z)));
            } else {
                trace 3,"one  ".$in->{'pos'}."  matched $q/$np    ".$N{$p[$q-1]}; # trace
                #return $r if $r->{fated};
                return (($r->{backed})?$r:{%$r, 'pos'=>$b->{'pos'},backed=>1}) if $opts->{first};
                $v = $r if (!defined $v || length(left($r)) < length(left($v)));
            }
        }
        if (defined $v) {
            trace 3,"one  ".$in->{'pos'}."  matched >0   $N{$p}"; # trace
            return $v;
        }
        trace 2,"one  ".$in->{'pos'}."  failed all   $N{$p}"; # trace
        trace 5,"sending back: ".Dumper({%$z, 'pos'=>$b->{'pos'}});
        return (($z->{backed})?$z:{%$z, 'pos'=>$b->{'pos'},backed=>1});
    };

    my $joiner = ($opts->{first})?' ^ ':' | ';
    $N{$p} = "( " . join($joiner, map $N{$_}, @p) . " )"; # trace
    $p;
}

sub both {
  my ($A, $B, $wsrule) = @_;
  $wsrule ||= '.';
  my $p;
  $p = parser {
    my ($in) = @_;
    my ($r);
    if ($wsrule eq '.') {
        # leave $A the way it is (this function's base case)
    } elsif ($wsrule eq '+') {
        $A = both($A,ws)
    } elsif ($wsrule eq '-') {
        $A = both($A,first(ws,nothing))
    }
    trace 2,"both  ".$in->{'pos'}."  attempting   $N{$A}   then   $N{$B}"; # trace
    $r = $A->($in);
    unless ($r->{success}) {
      trace 2,"both  ".$in->{'pos'}."  failed 1   $N{$A}"; # trace
      return ($r->{backed})?$r:{%$r, 'pos'=>$in->{'pos'},backed=>1};
    }
    $r = $B->($r);
    unless ($r->{success}) {
      trace 2,"both  ".$in->{'pos'}."  failed 2   $N{$B}"; # trace
      return ($r->{backed})?$r:{%$r, 'pos'=>$in->{'pos'},backed=>1};
    }
    trace 2,"both  ".$in->{'pos'}."  did  match   $N{$A}   and   $N{$B}"; # trace
    $r;
  };
  trace 6,"both creation dump: ".Dumper($A,$B,[caller(0),caller(1),caller(2)]);
  $N{$p} = "( $N{$A} $wsrule $N{$B} )"; # trace
  $p;
}

sub all {
  my @q = grep $_,@_;
  return nothing if @q == 0;
  return $q[0] if @q == 1;
  my $tail = pop @q;
  my $p;
  $p = both(all(@q),$tail,'.');
  $N{$p} = "all( ".join(" , ",map($N{$_},@q))." , $N{$tail} )"; # trace
  $p;
}

# placing star directly in another star() causes left recursion
sub star {
    my ($p, $p_conc, $p_exec);
    $p_exec = parser { $p->(@_) };
    $N{$p_exec} = "star( $N{$_[0]} )"; # trace
    $p_conc = both($_[0], $p_exec, '.');
    $N{$p_conc} = "( $N{$_[0]} . $N{$p_exec} )"; # trace
    $p = opt($p_conc);
    $N{$p} = $N{$p_exec}; # trace
    $p;
}

sub opt {
  my $p;
  $p = first($_[0], nothing);
  $N{$p} = "opt( $N{$_[0]} )"; # trace
  $p;
}

sub unspace () {
    my $p;
    $p = all(lit("\\"),star(ws));
    $N{$p} = 'us'; # trace
    $p;
}

# look for a wrapped entity.  first parm is split into the wrappers.
sub w {
    my ($d,$e) = split(//,$_[0]);
    my $p;
    $p = all(lit($d),$_[1],lit($e));
    $N{$p} = "\"$d\" $N{$_[1]} \"$e\""; # trace
    $p;
}

# the following 2 parsers are left-recursive

# look for an optionally wrapped entity.
sub ow {
    my ($d,$e) = split(//,$_[0]); # trace
    my $p;
    $p = first(w($_[0],$_[1]),$_[1]);
    $N{$p} = "[\"$d\"] $N{$_[1]} [\"$e\"]"; # trace
    $p;
}

# nested optional wrap
sub now {
    my ($d,$e) = split(//,$_[0]); # trace
    my ($p, $p_conc, $p_exec);
    $p_exec = parser { $p->(@_) };
    $N{$p_exec} = "now( $N{$_[0]} )"; # trace
    $p_conc = first( ow( $_[0], first( $p_exec , $_[1] ) ) );
    $N{$p_conc} = "( $N{$_[0]} | now( $N{$p_exec} ) )"; # trace
    $p = ow( $_[0], $p_conc );
    $N{$p} = $N{$p_exec}; # trace
    $p;
}

# optws and manws always receive 1 or 2 arguments when called
# by the overloaded operators in grammars
# (so that it always gets two arguments)
#   from within hand-written parser generators, write them out
#   explicitly, or always pass them two arguments :)

sub optws {
  my $p;
  if ($_[1]) {
    # binary -, so optional intervening whitespace
    $p = all($_[0], opt(ws), $_[1]);
    $N{$p} = "$N{$_[0]} - $N{$_[1]}"; # trace
  } else {
    # unary -, so optional leading whitespace
    $p = both(opt(ws), $_[0], '.');
    $N{$p} = " -( $N{$_[0]} )"; # trace
  }
  $p;
}

sub manws {
  my ($p);
  if (defined($_[1])) {
    # binary +, so mandatory intervening  whitespace
    $p = all($_[0], ws, $_[1]);
    $N{$p} = "$N{$_[0]} + $N{$_[1]}"; # trace
  } else {
    # unary +, so mandatory leading whitespace
    $p = both(ws, $_[0], '.');
    $N{$p} = "+( $N{$_[0]} )"; # trace
  }
  $p;
}

sub opttws {
  my $p;
  $p = both($_[0], opt(ws), '.');
  $N{$p} = "( $N{$_[0]} )--"; # trace
  $p;
}

sub mantws {
  my $p;
  $p = both($_[0], ws, '.');
  $N{$p} = "( $N{$_[0]} )++"; # trace
  $p;
}

sub newline {
  my $p;
  $p = plus(first(panic(lit("\n#{"),"\\n#{ is illegal"),opttws(optws(lit("\n")))));
  $N{$p} = "\\n"; # trace
  $p;
}

sub plus {
    my $t = $_[0];
    my $p;
    $p = both($t,star($t),'.');
    $N{$p} = "plus( $N{$_[0]} )"; # trace
    $p;
}

sub nthru {
  my ($n,$o) = @_;
  my $p;
  $p = both($n, thru($o),'.');
  $N{$p} = "$N{$n} * $N{$o}"; # trace
  $p;
}

# slurp up stuff until a stopper parser matches. basically, the {*} signifier.
# if you want to slurp up stuff *up until but stopping short of* the stopper,
# wrap the stopper in iff().  if-and-only-if, get it?
sub thru {
    my $stop = $_[0];
    my $p;
    $p = parser {
        my ($in) = @_;
        my @values;
        my $v = $in;
        while (defined $v) {
            $v = $stop->($v);
            if ($v->{success}) {
                trace 5,"through $N{$stop} matched"; # trace
                push @values, $v->{hit};
                last;
            } else {
                trace 5,"through $N{$stop} still not matched"; # trace
                push @values, substr($v->{inp},$v->{'pos'},1);
                $v->{pos}++;
            }
        }
        my $len;
        $len += length($_) foreach @values;
        $in->{hit} = join('',@values);
        trace 4,"Through token matched";
        $in->{pos} += length($in->{hit});
        trace 3,"advanced pos by ".length($in->{hit});
        push @{$in->{ast}->[0]},$in->{hit} unless $in->{hit} =~ /^[\s\n]+$/;
        lineify($in);
    };
    $N{$p} = '{*} . '.$N{$stop}; # trace
    $p;
}

sub keywords {
    my $p;
    $p = both(iff(match(qr|^([A-Za-z_]\w*)|)),lits(@_));
    $N{$p} = "( '".join("' ^ '",@_)."' )"; # trace
    $p;
}

sub lits {
    my $p;
    $p = first(map(lit($_),@_));
    $N{$p} = "( '".join("' ^ '",@_)."' )"; # trace
    $p;
}

sub worry {
    my ($ins,$msg) = @_;
    my $p;
    $p = parser {
        my ($in) = @_;
        trace 4,"Worrying if find $N{$ins}"; # trace
        my $b = $in;
        $in = $ins->($in);
        warning($msg,$b) if $in->{success};
        return {%$b,success=>($in->{success}?0:1)};
    };
    $N{$p} = "worry( $N{$ins} )"; # trace
    $p;
}

sub warning {
    print STDERR "Syntax warning in ".$_[1]->{name}." at line ".$_[1]->{line}." col ".$_[1]->{col}." : ".$_[0]."\n"
}

sub error {
    print STDERR "Syntax error in ".$_[1]->{name}." at line ".$_[1]->{line}." col ".$_[1]->{col}." : ".$_[0]."\n"
}

sub panic {
    my ($ins,$msg) = @_;
    my $p;
    $p = parser {
        my ($in) = @_;
        trace 4,"Dying if find $N{$ins}"; # trace
        my $b = $in;
        $in = $ins->($in);
        error($msg,$b) if $in->{success};
        exit 255 if $in->{success};
        $b;
    };
    $N{$p} = "panic( $N{$ins} )"; # trace
    $p;
}

sub to {
    my ($h,$p) = @_;
    $p = parser {
        return {%{$h->($_[0])}, success=>1};
    };
    $N{$p} = "hh"; # trace
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
