# This is a regexp engine, written in p5.  It the very beginnings of a
# proof of concept.  A feasibility and performance reality check.
# Hopefully it will develop into a p6 rules engine, and then a p6
# parser, for p5.

# It's a backtracking, recursive-decent engine, generated from the
# regexp ast nodes created by Regexp::Parser.

# We're using the normal Regexp::Parser nodes, rather than it's p6
# ones, because the latter currently lack tests, so I am uncertain of
# their status.  The nodes are basically the same, so it's not a big
# deal.


# Development plan as of Mon, 13 Feb 2006 16:30 utc:

# It looks like the ("A feasibility and performance reality check")
# spike has succeeded.  The re_tests are passing.  While the spike
# "coded to the tests", and thus is far from being a complete p5
# compatible regex engine, that establishes feasiblity.  Limited
# testing suggest performance is quite plausible. Perhaps, maybe, even
# wizzy.

# So here is the current plan.  This file forks.

# One fork continues tests-are-passing incremental development.  It's
# objective is to create a clean backtracking library (basically a set
# of simple macros) to make writing this kind of application easy(er).

# As long as no-one turns up any "putter, haven't you thought about X"
# bugs, the backtracking protocol is the "solid place we keep to
# stand".

# Subs taking a single "continuation" argument, and using return value
# to report success/failure, with part of the return value space left
# over for users (likely just ref's).  undef for failure, negative
# integers also (for propagating).  Probably also a short-cutting
# success.  It's not entirely clear how to handle an extensible set of
# things which get localized at choice points.  Perhaps a small set of
# vars, plus one which if defined, is a hash which gets shallow
# copied.  Perhaps one more level.  So the cost-over-current looks
# like a single local()ly scoped variable defined() check.  Hmm, I
# wonder if we relaxed the single-argument restriction, what would be
# the performance hit.  So, there is basically an api and p5 opcode
# profiling exercise here.  But in spike mode, we may just do a simple
# api, and worry about the rest later.

# The other fork is a bootstrap to a new, simpler, ast.
# Regexp::Parser's Perl6::Rule::Parser does NOT look like it's mature
# enough to bear our weight, to be a next step. And the current
# Regexp::Parser Object ast isnt really what we want long-term.  If
# Perl6::Rule::Parser was working and robust, it would make sense to
# continue for a while on Regexp::Parser.  But it's not worth working
# to finish.  So, we bootstrap to something cleaner and simpler.

# We add rules support and capture returns a :parsetree('Pkg_stem').
# We add a <foo args> rule construct to the existing p5 engine.  Using
# a trivial regex prefilter.  <foo a> ==> (?{ return
# $Somewhere::callrule($c,'foo','a') }). Poof - Instant rules support.
# :parsetree('Foo::') means a 'a <bar> b' regexp results in a Foo::bar
# blessed object, indexed by 'bar' in the node of the hash.  The
# objective is _not_ to get this exactly right, but just to fudge it
# enought to generate an ast for p5 regexps, sufficient to shift this
# show from Regexp::Parser::Object's to the new ast.

# Once we are migrated to a new ast and a clean backtracking api, the
# question is what next.  One approach would be to do p6 nodes and
# flesh out the p5 implementation.  Which probably isn't going to
# happen first.  Because... at the same point, we just small step away
# from a real p6 parser with full adjustable syntax.  Albeit one which
# only understands rx:perl5//'s, and only a subset of them at that.
# But oh, the temptation. :)

# So, that's the current plan.  Backtracking api, clean and simple
# ast, and rules in p5 regexp.  Then spike a real p6 parser.

# Actually, the api and ast may be more spike-ish than polished, so we
# have some experience with them before polishing.


# Test suite:
#  re_tests - tests from p5 HEAD 2006-02-12
#  re_tests_perl5.t - driver from same.
#  re_tests_parrot.t - parrot's driver, same date.
#  re_tests_match.el - an old driver putter wrote, adapted to call a Match obj.
# Just  prove regexp_engine_demo.t   or somesuch.

# Test status:
# Currently at 92% pass. 3sec run on my machine.

# Re:
# - play with Regexp::Parser's Perl6::Rules to establish it's state.
#   Summarize that here.
# Summary: it looks like early phase development.  There's a "return"
# halfway down the file to cut off construction.  Oh well.

#XXX - create a p5->p6 regexp syntax converter! :)
# create methods (in_p6_syntax?) which return the p6 version of the regexp.
# We will then pour the re_test suite through it, yielding a more complete
# version of t/rules/rules.t. TASK DEFERRED.

#XXX - bugs
# Flag handling is currently an utter kludge. not even trying with m and s.
# Match objects arent being created for capture groups which are never reached.
# No lookbehind as been implemented at all, at all.
# A couple of others I don't remember at the moment but should be mentioned...

# Cleanup tasks:
#  Do look-behind.  Or not, we don't really need it for now.
# Deferred to the new ast:
#  Create a p5 to p6 converter.  This is an easy task for someone.
#    A cpan module even.
#  Generate a p6 version of re_tests.


use Regexp::Parser;
use Data::Dumper;
use Match;
use strict;
{
  # Regexp::Parser bug fix/workaround
  package Regexp::Parser::anyof_class;
  sub visual {
    my $self = shift;
    if (ref $self->{data}) {
      $self->{data}->visual;
    }
    else {
      #join "", "[", $self->{how}, ($self->{neg} ? '^' : ''),
      #     $self->{type}, $self->{how}, "]";
      join "", "[", ${$self->{how}}, ($self->{neg} ? '^' : ''),
           $self->{type}, ${$self->{how}}, "]";
    }
  }
}
{
  package Hacks;

  #XXX - surely this is part of the Regexp::Parser api... somewhere?
  sub flag_val_m { 0x1 }
  sub flag_val_s { 0x2 }
  sub flag_val_i { 0x4 }
  sub flag_val_x { 0x8 }

  my $noop;
  $noop = sub{my $c = $_[0]; return 1 if $c eq $noop; @_=$noop; goto &$c};
  sub noop {
    $noop;
  }
  sub concat {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne 'ARRAY';
    my @a = @$aref;
    return $o->noop if @a == 0;
    return $a[0]->emit if @a == 1;
    my @fs = map { $_->emit } @a;
    my $code1 = ""; my $code2 = "";
    my $code0 = "my \$f0 = \$fs[0]; ";
    for my $i (reverse(1..$#a)) {
      $code0 .= "my \$f$i = \$fs[$i]; ";
      $code1 .= "sub{\@_=";
      $code2 .= ";goto \&\$f$i}";
    }
    my $code = $code0."\n sub{my \$cn = \$_[0]; \@_=".$code1."\$cn".$code2.";goto \&\$f0}\n";
    #print $code;
    eval($code) || die "$@";
  }   
  sub mk_ignore_this_node {
    my($o)=@_;
    my $noop = $o->noop;
    sub{my $c = $_[0]; @_=$noop; goto &$c};
  }
  sub mk_eater_for_re {
    my($o,$re)=@_;
    my $noop = $o->noop;
    my $qr = qr/\G($re)/;
    sub {
      my $c = $_[0];
      my($s) = $X::str;
      pos($s) = $X::pos;
      my $x = $s =~ $qr;
      return undef if !$x;
      $X::pos += length($1);
      @_=$noop;
      goto &$c;
    };
  }

  #my %warned_about;
  sub emit {
    my $cls = ref($_[0]);
    die "$cls emit() unimplemented\n";
    #warn "$cls emit() unimplemented\n" if !defined $warned_about{$cls}++;
    #sub{return undef};
  }
  sub inf {
    1000**1000**1000 #XXX there has to be a better way. :(
  }
}
{
  package Regexp::Parser::__object__;
  @Regexp::Parser::__object__::ISA=qw(Hacks);
}  
{
  # \A ^ \B \b \G \Z \z $
  package Regexp::Parser::anchor;
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
    my $re = $o->raw();
    return sub{return undef if !($X::pos == 0);
               my $c = $_[0]; @_=$noop; goto &$c
    } if $re eq '\A';
    return sub{return undef if !($X::pos == 0 || substr($X::str,$X::pos-1,1) eq "\n");
               my $c = $_[0]; @_=$noop; goto &$c
    } if $re eq '^';
    return $o->mk_eater_for_re('\B') if $re eq '\B';
    return $o->mk_eater_for_re('\b') if $re eq '\b';
    # \G
    return $o->mk_eater_for_re('\Z') if $re eq '\Z';
    return sub{return undef if !($X::pos == (length($X::str)));
               my $c = $_[0]; @_=$noop; goto &$c
    } if $re eq '\z';
    return sub{return undef if !($X::pos == (length($X::str)) || substr($X::str,$X::pos,1) eq "\n");
               my $c = $_[0]; @_=$noop; goto &$c
    } if $re eq '$';
    die "didn't implement $re"
  }
}
{
  # . \C
  package Regexp::Parser::reg_any;
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
    my $re = $o->raw();
    return $o->mk_eater_for_re('.') if $re eq '.';
    die "didn't implement $re"
  }
}
{
  # \w \W
  package Regexp::Parser::alnum;
  sub emit {
    my($o)=@_;
    my $re = $o->raw();
    return $o->mk_eater_for_re('\w') if $re eq '\w';
    return $o->mk_eater_for_re('\W') if $re eq '\W';
    die "Regexp compilation bug";
  }
}
{
  # \s \S
  package Regexp::Parser::space;
  sub emit {
    my($o)=@_;
    my $re = $o->raw();
    return $o->mk_eater_for_re('\s') if $re eq '\s';
    return $o->mk_eater_for_re('\S') if $re eq '\S';
    die "Regexp compilation bug";
  }
}
{
  # \d \D
  package Regexp::Parser::digit;
  sub emit {
    my($o)=@_;
    my $re = $o->raw();
    return $o->mk_eater_for_re('\d') if $re eq '\d';
    return $o->mk_eater_for_re('\D') if $re eq '\D';
    die "Regexp compilation bug";
  }
}
{
  package Regexp::Parser::anyof;
  sub emit {
    my($o)=@_;
    my $re = $o->visual();
    if($o->{'flags'} & $o->flag_val_i) {
       $re = "(?i)(?:$re)";
    }
    return $o->mk_eater_for_re($re);
  }
}
{
  package Regexp::Parser::anyof_char;
}
{
  package Regexp::Parser::anyof_range;
}
{
  package Regexp::Parser::anyof_class;
}
{
  package Regexp::Parser::anyof_close;
}
{
  package Regexp::Parser::prop;
}
{
  package Regexp::Parser::clump;
}
{
  # |
  package Regexp::Parser::branch;
  sub emit {
    my($o)=@_;
    my(@fs) = map { $o->concat($_) } @{$o->data};
    my $f_last = pop(@fs);
    sub{
      my $c = $_[0];
      for my $f (@fs) {
        my $c_down = $c;
        my($str,$pos,$cap); my $v;
        { local($X::str,$X::pos,$X::cap)=($X::str,$X::pos,$X::cap);
          $v = $f->($c_down);
          ($str,$pos,$cap)=($X::str,$X::pos,$X::cap) if defined $v;
        }
        if(defined $v) {
          ($X::str,$X::pos,$X::cap)=($str,$pos,$cap);
          return $v;
        }
      }
      @_= $c;
      goto &$f_last;
    };
  }
}
{
  package Regexp::Parser::exact;
  sub emit{
    my($o)=@_;
    my $noop = $o->noop;
    my $s = join("",$o->data);
    my $len = length($s);
    if($o->{'flags'} & $o->flag_val_i) {
      my $pat = $s;
      $pat =~ s/(\W)/\\$1/g;
      $pat = "(?:(?i)(?:$pat))";
      $o->mk_eater_for_re($pat);
    }
    else {
      sub {
        my $c = $_[0];
        return undef if !(substr($X::str,$X::pos,$len) eq $s);
        $X::pos += $len;
        @_=$noop; goto &$c;
      };
    }
  }
}
{
  package Regexp::Parser::quant;
  no warnings "recursion";
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
    my($min,$max)= (@$o{'min','max'});
    $min = 0 if $min eq "";
    $max = $o->inf if $max eq "";
    $min += 0; $max += 0; 
    my $f = $o->data->emit;
    sub{
      my $c = $_[0];
      my $pos_old = -1;
      my $i = 0;
      my($fmin,$fagain,$frest);
      $fmin = sub{
        if($i >= $min) {
          goto &$fagain;
        }
        @_=$fmin; $i++; goto &$f;
      };
      $fagain = sub{
        if($pos_old >= $X::pos){
          @_=$noop;
          goto &$c;
        }
        $pos_old = $X::pos;
        goto &$frest;
      };
      $frest = sub{
        if($i >= $max) {
          @_=$noop;
          goto &$c;
        }
        $i++;
        my $c_down = $fagain;
        my($str,$pos,$cap); my $v;
        { local($X::str,$X::pos,$X::cap)=($X::str,$X::pos,$X::cap);
          $v = $f->($c_down);
          ($str,$pos,$cap)=($X::str,$X::pos,$X::cap) if defined $v;
        }
        if(defined $v) { 
          ($X::str,$X::pos,$X::cap)=($str,$pos,$cap);
          return $v;
        }
        @_=$noop;
        goto &$c;
      };
      goto &$fmin;
    };        
  }
}
{
  # *? +? {,}? - sort of: have to reach down into quant.
  package Regexp::Parser::minmod;
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
    my($min,$max)= (@{$o->data}{'min','max'});
    $min = 0 if $min eq "";
    $max = $o->inf if $max eq "";
    $min += 0; $max += 0; 
    my $f = $o->data->data->emit; # reach down into quant below.
    sub{
      my $c = $_[0];
      my $pos_old = -1;
      my $i = 0;
      my($fmin,$fagain,$frest);
      $fmin = sub{
        if($i >= $min) {
          goto &$fagain;
        }
        @_=$fmin; $i++; goto &$f;
      };
      $fagain = sub{
        if($pos_old >= $X::pos){
          @_=$noop;
          goto &$c;
        }
        $pos_old = $X::pos;
        goto &$frest;
      };
      $frest = sub{
        if($i >= $max) {
          @_=$noop;
          goto &$c;
        }
        $i++;
        my $c_down = $fagain;
        my($str,$pos,$cap); my $v;
        { local($X::str,$X::pos,$X::cap)=($X::str,$X::pos,$X::cap);
          #$v = $f->($c_down);
          $v = $c->($noop);
          ($str,$pos,$cap)=($X::str,$X::pos,$X::cap) if defined $v;
        }
        if(defined $v) { 
          ($X::str,$X::pos,$X::cap)=($str,$pos,$cap);
          return $v;
        }
        # @_=$noop; goto &$c;
        @_=$c_down; goto &$f;
      };
      goto &$fmin;
    };        
  }
}
{
  # ( non-capturing
  package Regexp::Parser::group;
  sub emit {
    my($o)=@_;
    my $f = $o->concat( $o->data );
    return $f;
  }
}
{
  # ( capturing
  package Regexp::Parser::open;
  sub emit {
    my($o)=@_;
    my $f = $o->concat( $o->data );
    my $idx = $o->{'nparen'} -1;
    sub{
      my $c = $_[0];
      my $m = MatchX->new();
      $X::cap = [@$X::cap];
      $X::cap->[$idx] = $m;
      my $from = $X::pos;
      my $close = sub {
        my $c0 = $_[0];
        my $to = $X::pos;
        $m->set(1,substr($X::str,$from,$to-$from),[],{},$from,$to);
        @_=$c;
        goto &$c0;
      };
      my $v = $f->($close);
      $m->set_as_failed if !defined($v);
      return $v;
    }
  }
}
{
  # ) closing
  package Regexp::Parser::close;
}
{
  # ) for non-captures
  package Regexp::Parser::tail;
}
{
  # \1 (backrefs)
  package Regexp::Parser::ref;
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
    my $idx = $o->{'nparen'} -1;
    sub {
      my $c = $_[0];
      return undef if $idx >= @$X::cap;
      my $m = $X::cap->[$idx];
      my $s = "$m";
      my $pat = $s;
      $pat =~ s/(\W)/\\$1/g;
      $pat = "(?:(?i)(?:$pat))" if($o->{'flags'} & $o->flag_val_i);
      my $ok = substr($X::str,$X::pos) =~ /\A($pat)/;
      return undef if !$ok;
      $X::pos += length($1);
      @_=$noop;
      goto &$c;
    };
  }
}
{
  package Regexp::Parser::assertion;
}
{
  # (?=) (?<=)
  package Regexp::Parser::ifmatch;
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
    my $f = $o->concat( $o->data );
    my $dir = $o->{'dir'};
    die "(?<=) is not yet implemented" if $dir < 0;
    sub {
      my $c = $_[0];
      my $v;
      { local($X::str,$X::pos,$X::cap)=($X::str,$X::pos,$X::cap);
        $v = $f->($noop);
        return undef if !$v;
      }
      @_=$noop;
      goto &$c;
    };
  }
}
{
  # (?!) (?<!)
  package Regexp::Parser::unlessm;
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
    my $f = $o->concat( $o->data );
    my $dir = $o->{'dir'};
    die "(?<!) is not yet implemented" if $dir < 0;
    sub {
      my $c = $_[0];
      my $v;
      { local($X::str,$X::pos,$X::cap)=($X::str,$X::pos,$X::cap);
        $v = $f->($noop);
        return undef if $v;
      }
      @_=$noop;
      goto &$c;
    };
  }
}
{
  # (?>)
  package Regexp::Parser::suspend;
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
    my $f = $o->concat( $o->data );
    sub {
      my $c = $_[0];
      my $v = $f->($noop);
      return undef if !defined $v;
      @_=$noop;
      goto &$c;
    };
  }
}
{
  # (?(n)t|f)
  package Regexp::Parser::ifthen;
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
#    die Data::Dumper::Dumper $o;

    my $f_test = $o->data->[0]->emit;
    my $f_then = $o->data->[1]->data->[0][0]->emit;
    my $crufty = $o->data->[1]->data;
    my $f_else = sub{my $c = $_[0]; @_=$noop; goto &$c};
    $f_else = $o->data->[1]->data->[1][0]->emit if @{$o->data->[1]->data} > 1;
    sub {
      my $c = $_[0];
      my $v;
      { local($X::str,$X::pos,$X::cap)=($X::str,$X::pos,$X::cap);
        $v = $f_test->($noop);
      }
      if(defined($v)) {
        @_=$c;
        goto &$f_then;
      } else {
        @_=$c;
        goto &$f_else;
      }
    };
  }
}
{
  # the N in (?(N)t|f) when N is a number
  package Regexp::Parser::groupp;
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
    my $idx = $o->{'nparen'} -1;
    sub {
      my $c = $_[0];
      return undef if $idx >= @$X::cap;
      my $m = $X::cap->[$idx];
      return undef if !$m;
      @_=$noop;
      goto &$c;
    };
  }
}
{
  # (?{ ... })
  package Regexp::Parser::eval;
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
    my $embedded_code = join("",$o->data);
    my $code = 'sub{my $__c__ = $_[0]; '.$embedded_code.'; @_=$noop; goto &$__c__}';
    eval($code) || die "Error compiling (?{$embedded_code}) :\n$@\n";
  }
}
{
  # (??{ ... })
  package Regexp::Parser::logical;
}
{
  package Regexp::Parser::flags;
  sub emit {
    my($o)=@_;
    $o->mk_ignore_this_node();
  }
}

{
package Y;
our %rules;
sub dorule {
  my($c,$name,$args)=@_;
  my $noop = Hacks->noop;

  my $ru = $rules{$name};
  if(!defined $ru) {
    warn "Unknown rule '$name'";
    return undef;
  }

  my $pos = $X::pos;
  my $cap = $X::cap;
  my $m0 = $X::current_match;
  my $m1 = MatchX->new;
  $$m1->{'RULE'} ||= $name; #EEEP
  $m1->set(1,"",[],{},$pos,undef);

  my $rest = sub{
    my $cn = $_[0];
    $$m1->{'val_array'} = $X::cap; #EEEP
    $$m1->{'to'} = $X::pos; #EEEP
    $m1->set_str(substr($X::str,$pos,$X::pos-$pos));
    local $m0->{$name} = [@{$m0->{$name}||[]}];
    push(@{$m0->{$name}},$m1); #see below
    $X::cap = $cap;
    $X::current_match = $m0;
    @_=$c; goto &$cn;
  };

  my $v;
  { local $X::current_match = $m1;
    local $X::cap = [];
    $v = $ru->($rest);
  }
  return undef if !defined $v;
  unshift(@{$m0->{$name}},$m1);# sigh,
  #  twice: once for inline code, once for the final Match tree.
  return $v;
}
}
sub def_rule {
  my($name,$re)=@_;
  my $r = ref($re) ? $re : compile($re);
  die if !defined $r;
  $Y::rules{$name} = $r;
}


my $noop = Hacks->noop;
sub compile {
  my($re)=@_;
  $re =~ s/<(\w+)([^\>]*)>/(?{\@_=(\$__c__,'$1','$2');goto \&Y::dorule;})/g;
  my $parser = Regexp::Parser->new($re);
  my $n = eval{ $parser->root };
  die "$@" if !defined $n;
  #print Dumper $n;
  my $r = Hacks->concat($n);
  return $r;
}
sub match {
  my($r,$s)=@_;
  my $len = length($s);
  for my $start (0..$len) {
    local $X::str = $s;
    local $X::pos = $start;
    local $X::cap = [];
    my $m = MatchX->new();
    local $X::current_match = $m;
    my $ok = $r->($noop);
    if(defined($ok)) {
      my $a = $X::cap;
      $m->set(1,substr($X::str,$start,$X::pos-$start),
              $a,\%{$m},$start,$X::pos);
      return $m;
    }
  }
  return MatchX->new()->set_as_failed;
}
sub match_re {
  my($re,$s)=@_;
  my $r = compile($re);
  my $m = match($r,$s);
  return $m;
}

if(@ARGV && $ARGV[0] eq '--test') {
  require './re_tests_match.pl';
  Pkg_re_tests::test(sub{my($mods,$re)=@_;
    $re = "(?$mods)(?:$re)" if $mods;
    my $r = compile($re); sub{my($s)=@_;match($r,$s)}});
  exit;
}
1;
__END__
#print Dumper match_re('a','abc');
