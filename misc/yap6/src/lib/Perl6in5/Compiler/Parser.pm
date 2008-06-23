use strict;
use warnings;
no warnings qw{ reserved closure recursion };

package Perl6in5::Compiler::Parser;

use Tie::IxHash;
use Perl6in5::Compiler::Trace;
use Memoize;
use Exporter;
our @EXPORT_OK = qw(lit eoi nothing star opt %stat
                say all one newline left ceoi worry
                %N parser check to first iff trace thru
                w keywords panic ws nthru lits
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
    my $p; # trace
    $p =  # trace
    bless( sub {
        my ($in) = @_;
        my $pos = $in->{'pos'};
        my $m; 
        if (!defined $M{$q}{$pos}) {
            $stat{memo_misses}++; # trace
            $m = $M{$q}{$pos} = $q->($in);
        } else {
            $stat{memo_hits}++; # trace
            $m = $M{$q}{$pos};
        }
        $m;
    } => __PACKAGE__ );
    $p; # trace
}

memoize('parser',NORMALIZER=> sub { "@_" });

sub eoi {
    my $p; # trace
    $p =  # trace
    parser {
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
    $p; # trace
}
memoize('eoi',NORMALIZER=> sub { "@_" });

sub nothing () {
    my $p; # trace
    $p =  # trace
    parser {
        $_[0];
    };
    $N{$p} = 'nothing'; # trace
    $p; # trace
}
memoize 'nothing';

sub blank () {
    my $p; # trace
    $p =  # trace
    match( qr|^(\s+)|s );
    $N{$p} = 'blank'; # trace
    $p; # trace
}
memoize 'blank';

sub ws () {
    my $p; # trace
    $p =  # trace
    plus(one(blank,lit("\n")));
    $N{$p} = 'ws'; # trace
    $p; # trace
}
memoize 'ws';

sub unmore {
    my $q = $_[0];
    # this parser acts like a NOT gate
    my ($p); # trace
    $p =  # trace
    parser {
        my ($in) = @_;
        my $b = $in;
        $in = $q->($in);
        $b->{success} = $in->{success}?0:1;
        $b;
    };
    $N{$p} = "fail( $N{$q} )"; # trace
    $p; # trace
}
memoize('unmore',NORMALIZER=> sub { "@_" });

sub iff {
    my $q = $_[0];
    my $p; # trace
    $p =  # trace
    parser {
        my ($in) = @_;
        my $b = deep_copy($in);
        $in = $q->($in);
        $b->{success} = $in->{success}?1:0;
        $b;
    };
    $N{$p} = "iff( $N{$q} )"; # trace
    $p; # trace
}
memoize('iff',NORMALIZER=> sub { "@_" });

sub match {
    my $q = $_[0];
    # $q is a regular expression of the form: qr/^(match_pattern)/s
    # note that you must include the ^ or you will get
    # incorrect results, b/c match() uses the length of the captured group
    # to eat the proper length of input.  Probably your REs shouldn't be greedy...
    my $p; # trace
    $p =  # trace
    parser {
        my ($in) = @_;
        $in->{want} = "RE $q";
        unless (($in->{hit}) = left($in) =~ $q) {
            trace 3,"match failed on $N{$p}"; # trace
            return err($in,"$N{$p}"); # trace
            return err($in,"unknown");
        } else {
            trace 3,"match matched ".$in->{hit}." on $N{$p}"; # trace
        }
        $in->{'pos'} += length($in->{hit});
        trace 3,"advanced pos by ".length($in->{hit});
        push @{$in->{ast}->[0]},$in->{hit} unless $in->{hit} =~ /^[\s\n]+$/;
        lineify($in);
    };
    $N{$p} = "match( $q )"; # trace
    $p; # trace
}
memoize('match',NORMALIZER=> sub { "@_" });

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
memoize('left',NORMALIZER=> sub { $_[0]->{'pos'} });

sub lit {
    my ($want) = @_;
    my $p; # trace
    $p =  # trace
    match( qr/^(\Q$want\E)/s );
    $N{$p} = Dumper($want); # trace
    $p; # trace
}
memoize 'lit';

sub ceoi {
    left($_[0]) eq ''
}
memoize('ceoi',NORMALIZER=> sub { $_[0]->{'pos'} });

sub err{
    trace 3,"errored ".Dumper($_[1]);
    return {%{$_[0]},success=>0,expected=>($_[1] || 'unknown')};
}

sub first {
    one( { first => 1 }, @_ );
}
memoize('first',NORMALIZER=> sub { "@_" });

sub one {
    my $opts = (ref $_[0] eq 'HASH')?shift:{};
    my @p = grep $_,@_;
    return nothing if @p == 0;
    return $p[0] if @p == 1;
    my $p; # trace
    $p =  # trace
    parser {
        my ($in) = @_;
        trace 2,"one  ".$in->{'pos'}."  trying >0   ".$N{$p}." on ".Dumper(left($in)); # trace
        my ($q, $np) = (0, scalar @p); # trace
        my ($v,$r,$z);
        my $b = deep_copy($in);
        for (@p) {
            $q++; # trace
            my $c = deep_copy($b);
            #trace 2,"one  ".$in->{'pos'}."  trying $q/$np   ".$N{$p[$q-1]}." on ".Dumper(left($c));# XXXXXXXXXXX   AUTOVIVIFICATION WEIRDNESS  # trace
            $r = $_->($c);
            #trace 5,"one  ".$N{$p[$q-1]}." returned ".Dumper($r);# XXXXXXXXXXX   AUTOVIVIFICATION WEIRDNESS  # trace
            unless ($r->{success}) {
                trace 3,"one  ".$in->{'pos'}."  failed $q/$np    ".$N{$p[$q-1]}; # trace
                # send back the shortest remaining input if none
                # succeed; this means the input was validly parsed
                # up till then.
                $z = $r if (!defined $z || $r->{'pos'} > $z->{'pos'});
            } else {
                trace 3,"one  ".$in->{'pos'}."  matched $q/$np    ".$N{$p[$q-1]}; # trace
                #return $r if $r->{fated};
                return (($r->{backed})?$r:{%$r, 'pos'=>$in->{'pos'},backed=>1}) if $opts->{first};
                $v = $r if (!defined $v || $r->{'pos'} > $v->{'pos'});
            }
        }
        if (defined $v) {
            trace 2,"one  ".$in->{'pos'}."  matched >0   $N{$p}"; # trace
            return $v;
        }
        trace 2,"one  ".$in->{'pos'}."  failed all   $N{$p}"; # trace
        trace 5,"sending back: ".Dumper({%$z, 'pos'=>$b->{'pos'}});
        return (($z->{backed})?$z:{%$z, 'pos'=>$b->{'pos'},backed=>1});
    };
    my $joiner = ($opts->{first})?' ^ ':' | '; # trace
    $N{$p} = "( " . join($joiner, map $N{$_}, @p) . " )"; # trace
    $p; # trace
}
memoize('one',NORMALIZER=> sub { "@_" });

sub both {
  my ($A, $B, $wsrule) = @_;
  $wsrule ||= '.';
  my $p; # trace
  $p =  # trace
  parser {
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
  $N{$p} = "( $N{$A} $wsrule $N{$B} )"; # trace
  $p; # trace
}
memoize('both',NORMALIZER=> sub { "@_" });

sub all {
  my @q = grep $_,@_;
  return nothing if @q == 0;
  return $q[0] if @q == 1;
  my $tail = pop @q;
  my $p; # trace
  $p =  # trace
  both(all(@q),$tail,'.');
  $N{$p} = "all( ".join(" , ",map($N{$_},@q))." , $N{$tail} )"; # trace
  $p; # trace
}
memoize('all',NORMALIZER=> sub { "@_" });

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
memoize('star',NORMALIZER=> sub { "@_" });

sub opt {
  my $p; # trace
  $p =  # trace
  first($_[0], nothing);
  $N{$p} = "opt( $N{$_[0]} )"; # trace
  $p; # trace
}
memoize('opt',NORMALIZER=> sub { "@_" });

sub unspace () {
    my $p; # trace
    $p =  # trace
    both(lit("\\"),star(ws));
    $N{$p} = 'us'; # trace
    $p; # trace
}
memoize('unspace');

# look for a wrapped entity.  first parm is split into the wrappers.
sub w {
    my ($d,$e) = split(//,$_[0]);
    my $p; # trace
    $p =  # trace
    both(both(lit($d),$_[1]),lit($e));
    $N{$p} = "\"$d\" $N{$_[1]} \"$e\""; # trace
    $p; # trace
}
memoize('w',NORMALIZER=> sub { "@_" });

# the following 2 parsers are left-recursive

# look for an optionally wrapped entity.
sub ow {
    my ($d,$e) = split(//,$_[0]); # trace
    my $p; # trace
    $p =  # trace
    first(w($_[0],$_[1]),$_[1]);
    $N{$p} = "[\"$d\"] $N{$_[1]} [\"$e\"]"; # trace
    $p; # trace
}
memoize('ow',NORMALIZER=> sub { "@_" });

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
memoize('now',NORMALIZER=> sub { "@_" });

# optws and manws always receive 1 or 2 arguments when called
# by the overloaded operators in grammars
# (so that it always gets two arguments)
#   from within hand-written parser generators, write them out
#   explicitly, or always pass them two arguments :)

sub optws {
  my $p; # trace
  if ($_[1]) {
    # binary -, so optional intervening whitespace
    $p =  # trace
    both(both($_[0], first(ws,nothing)), $_[1]);
    $N{$p} = "$N{$_[0]} - $N{$_[1]}"; # trace
  } else {
    # unary -, so optional leading whitespace
    $p =  # trace
    both(opt(ws), $_[0], '.');
    $N{$p} = " -( $N{$_[0]} )"; # trace
  }
  $p; # trace
}
memoize('optws',NORMALIZER=> sub { "@_" });

sub manws {
  my ($p); # trace
  if (defined($_[1])) {
    # binary +, so mandatory intervening  whitespace
    $p =  # trace
    both(both($_[0], ws), $_[1]);
    $N{$p} = "$N{$_[0]} + $N{$_[1]}"; # trace
  } else {
    # unary +, so mandatory leading whitespace
    $p =  # trace
    both(ws, $_[0], '.');
    $N{$p} = "+( $N{$_[0]} )"; # trace
  }
  $p; # trace
}
memoize('manws',NORMALIZER=> sub { "@_" });

sub opttws {
  my $p; # trace
  $p =  # trace
  both($_[0], first(ws,nothing), '.');
  $N{$p} = "( $N{$_[0]} )--"; # trace
  $p; # trace
}
memoize('opttws',NORMALIZER=> sub { "@_" });

sub mantws {
  my $p; # trace
  $p =  # trace
  both($_[0], ws, '.');
  $N{$p} = "( $N{$_[0]} )++"; # trace
  $p; # trace
}
memoize('mantws',NORMALIZER=> sub { "@_" });

sub newline {
  my $p; # trace
  $p =  # trace
  plus(first(panic(lit("\n#{"),"\\n#{ is illegal"),opttws(optws(lit("\n")))));
  $N{$p} = "\\n"; # trace
  $p; # trace
}
memoize('newline',NORMALIZER=> sub { "@_" });

sub plus {
    my $t = $_[0];
    my $p; # trace
    $p =  # trace
    both($t,star($t),'.');
    $N{$p} = "plus( $N{$_[0]} )"; # trace
    $p; # trace
}
memoize('plus',NORMALIZER=> sub { "@_" });

sub nthru {
  my ($n,$o) = @_;
  my $p; # trace
  $p =  # trace
  both($n, thru($o),'.');
  $N{$p} = "$N{$n} * $N{$o}"; # trace
  $p; # trace
}
memoize('nthru',NORMALIZER=> sub { "@_" });

# slurp up stuff until a stopper parser matches. basically, the {*} signifier.
# if you want to slurp up stuff *up until but stopping short of* the stopper,
# wrap the stopper in iff().  if-and-only-if, get it?
sub thru {
    my $stop = $_[0];
    my $p; # trace
    $p =  # trace
    parser {
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
                $v->{'pos'}++;
            }
        }
        my $len;
        $len += length($_) foreach @values;
        $in->{hit} = join('',@values);
        trace 4,"Through token matched";
        $in->{'pos'} += length($in->{hit});
        trace 3,"advanced pos by ".length($in->{hit});
        push @{$in->{ast}->[0]},$in->{hit};
        lineify($in);
    };
    $N{$p} = '{*} . '.$N{$stop}; # trace
    $p; # trace
}
memoize('thru',NORMALIZER=> sub { "@_" });

sub keywords {
    my $p; # trace
    $p =  # trace
    both(iff(match(qr|^([A-Za-z_]\w*)|)),lits(@_));
    $N{$p} = "( '".join("' ^ '",@_)."' )"; # trace
    $p; # trace
}
memoize('keywords',NORMALIZER=> sub { "@_" });

sub lits {
    my $p; # trace
    $p =  # trace
    first(map(lit($_),@_));
    $N{$p} = "( '".join("' ^ '",@_)."' )"; # trace
    $p; # trace
}
memoize('lits');

sub worry {
    my ($ins,$msg) = @_;
    my $p; # trace
    $p =  # trace
    parser {
        my ($in) = @_;
        trace 4,"Worrying if find $N{$ins}"; # trace
        my $b = deep_copy($in);
        $in = $ins->($in);
        warning($msg,$b) if $in->{success};
        return {%$b,success=>($in->{success}?0:1)};
    };
    $N{$p} = "worry( $N{$ins} )"; # trace
    $p; # trace
}
memoize('worry',NORMALIZER=> sub { "@_" });

sub warning {
    print STDERR "Syntax warning in ".$_[1]->{name}." at line ".$_[1]->{line}." col ".$_[1]->{col}." : ".$_[0]."\n"
}

sub error {
    print STDERR "Syntax error in ".$_[1]->{name}." at line ".$_[1]->{line}." col ".$_[1]->{col}." : ".$_[0]."\n"
}

sub panic {
    my ($ins,$msg) = @_;
    my $p; # trace
    $p =  # trace
    parser {
        my ($in) = @_;
        trace 4,"Dying if find $N{$ins}"; # trace
        my $b = $in;
        $in = $ins->($in);
        error($msg,$b) if $in->{success};
        exit 255 if $in->{success};
        $b;
    };
    $N{$p} = "panic( $N{$ins} )"; # trace
    $p; # trace
}
memoize('panic',NORMALIZER=> sub { "@_" });

sub to {
    my ($h) = @_;
    my $p; # trace
    $p =  # trace
    parser {
        return {%{$h->($_[0])}, success=>1};
    };
    $N{$p} = "hh"; # trace
    $p; # trace
}
memoize('to',NORMALIZER=> sub { "@_" });

our %opep_table;


# operator precedence expression parser (builder)
sub opep {
    my ($t,      #  the "term" parser (what will appear in these expressions besides operators)
        $o)      #  the operator table. AroHr:  [ { ... }, {
                 #      'precedence_level_name' => {
                 #          'operator_name' => [
                 #              'literal symbol character(s)',
                 #              'prefix|postfix|infix',
                 #              'direction_of_association'
                 #              'handler_name',
                 # # the following 2 are trinary_whitespace_disallowed0_optional1_mandatory2:
                 #              'leading',
                 #              'trailing',
                 #              'boolean_chaining'
        = @_;
    # precreate all the filtered/lookup membership tables as hash keys, so that classifications
    # for "the next operator" can efficiently be tested by hash.exists().  
    
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
