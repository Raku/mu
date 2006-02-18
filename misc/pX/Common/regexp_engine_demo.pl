# This is a regexp engine, written in p5.  It the very beginnings of a
# proof of concept.  A feasibility and performance reality check.
# Hopefully it will develop into a p6 rules engine, and then a p6
# parser, for p5.

# It's a backtracking, recursive-decent engine, generated from the
# regexp ast nodes created by Regexp::Parser.

# It looks like the ("A feasibility and performance reality check")
# spike has succeeded.  The most of the re_tests are passing.  While
# the spike "coded to the tests", and thus is far from being a
# complete p5 compatible regex engine, that establishes feasiblity.
# Limited testing suggest performance is quite plausible.

# Next steps:

# Create a clean backtracking library (basically a set of simple
# macros) to make writing this kind of application easy(er).

# Bootstrap to a new, simpler, ast.  Regexp::Parser's
# Perl6::Rule::Parser does NOT look like it's mature enough to bear
# our weight, to be a next step. And the current Regexp::Parser Object
# ast isnt really what we want long-term.  If Perl6::Rule::Parser was
# working and robust, it would make sense to continue for a while on
# Regexp::Parser.  But it's not worth working to finish.  So, we
# bootstrap to something cleaner and simpler.  Even though it will
# likely lack R:P's nice error messages.


# Test suite:
#  re_tests - tests from p5 HEAD 2006-02-12
#  re_tests_perl5.t - driver from same.
#  re_tests_parrot.t - parrot's driver, same date.
#  re_tests_match.el - an old driver putter wrote, adapted to call a Match obj.
# Just  prove regexp_engine_demo.t   or somesuch.

# Test status:
# Currently at 92% pass. 3sec run on my machine.

# Sidebar: State of Regexp::Parser's Perl6::Rules: it looks like early
# phase development.  There's a "return" halfway down the file to cut
# off construction.  Oh well.

#XXX - a possible task: Create a p5->p6 regexp syntax converter!  A
#cpan module even. Write methods which return the p6 version of the
#regexp.  We can then pour the re_test suite through it, yielding a
#more complete version of t/rules/rules.t.  This could either be done
#using Regexp::Parser Objects, or wait for the hypotetical new ast.

#XXX - bugs
# Flag handling is currently an utter kludge. not even trying with m and s.
# Match objects arent being created for capture groups which are never reached.
# No lookbehind as been implemented at all, at all.
# A couple of others I don't remember at the moment but should be mentioned...

package BacktrackDev;
=pod
Design notes:
 Why not call "TAILCALL" "GOTO"?  So you can easily search on both.
 Why LET but not TEMP?  LET is non-trivial, TEMP isn't.  Minimalism wins.
=cut
my @let_stack;
sub let_gen {
  my($vars)=@_;
  my $nvars = 1+($vars =~ tr/,//);
  my $tmpvars = join(",",map{"\$__tmp${_}__"}(0..($nvars-1)));
  push(@let_stack,[$vars,$tmpvars]);
  "(do{my \$__v__ ; my($tmpvars); { local($vars)=($vars); \$__v__ = do{ ";
}
sub let_end {
  my $e = shift(@let_stack) || die "LET(){ }LET pairs didn't match up";
  my($vars,$tmpvars) = @$e;
  "}; if(!FAILED(\$__v__)){ ($tmpvars)=($vars); }}; if(!FAILED(\$__v__)){ ($vars)=($tmpvars) }; \$__v__ })"
}
use Filter::Simple sub {
  s/\bLET\(([^\)]+)\)\{/let_gen($1)/eg;
  s/\}LET;/let_end().";"/eg;
  s/\bFAIL_IF_FAILED\(([^\)]+)\);/FAIL() if FAILED($1);/g;
  s/\bFAIL\(([^\)]{0,0})\)/return undef/g;
  s/\bFAILED\(([^\)]+)\)/(!defined($1))/g;
  s/\bTAILCALL\(([^,\)]+)\);/\@_=(Hacks->noop);goto $1;/g; #no
  s/\bTAILCALL\(([^,\)]+),?([^\)]*)\);/\@_=($2);goto $1;/g;
  #print STDERR $_;
  $_;
};
1;
package main;
use Regexp::Parser;
use Data::Dumper;
use Match;
BEGIN { BacktrackDev->import; };
use strict;

if($Regexp::Parser::VERSION <= 0.20) {
  eval <<'END';
    # Regexp::Parser bug fix/workaround
    package Regexp::Parser::anyof_class;
    sub visual {
      my $self = shift;
      if (ref $self->{data}) {
	$self->{data}->visual;
      }
      else {
	# The actual bug is in Handlers.pm init() - the \$how 's.
	#join "", "[", $self->{how}, ($self->{neg} ? '^' : ''),
	#     $self->{type}, $self->{how}, "]";
	join "", "[", ${$self->{how}}, ($self->{neg} ? '^' : ''),
	     $self->{type}, ${$self->{how}}, "]";
      }
    }
END
}

{
  package Hacks;

  #XXX - surely this is part of the Regexp::Parser api... somewhere?
  sub flag_val_m { 0x1 }
  sub flag_val_s { 0x2 }
  sub flag_val_i { 0x4 }
  sub flag_val_x { 0x8 }

  my $noop;
  $noop = sub{my $c = $_[0]; return 1 if $c eq $noop; TAILCALL(&$c,$noop);};
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
    sub{my $c = $_[0]; TAILCALL(&$c,$noop);};
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
      FAIL() if !$x;
      $X::pos += length($1);
      TAILCALL(&$c,$noop);
    };
  }

  sub emit {
    my $cls = ref($_[0]);
    die "$cls emit() unimplemented\n";
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
    return sub{
      FAIL() if !($X::pos == 0);
      my $c = $_[0]; TAILCALL(&$c,$noop);
    } if $re eq '\A';
    return sub{
      FAIL() if !($X::pos == 0 || substr($X::str,$X::pos-1,1) eq "\n");
      my $c = $_[0]; TAILCALL(&$c,$noop);
    } if $re eq '^';
    return $o->mk_eater_for_re('\B') if $re eq '\B';
    return $o->mk_eater_for_re('\b') if $re eq '\b';
    # \G
    return $o->mk_eater_for_re('\Z') if $re eq '\Z';
    return sub{
      FAIL() if !($X::pos == (length($X::str)));
      my $c = $_[0]; TAILCALL(&$c,$noop);
    } if $re eq '\z';
    return sub{
      FAIL() if !($X::pos == (length($X::str)) || substr($X::str,$X::pos,1) eq "\n");
      my $c = $_[0]; TAILCALL(&$c,$noop);
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
        my $v = LET($X::pos){ $f->($c) }LET;
        return $v if not FAILED($v);
      }
      TAILCALL(&$f_last,$c);
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
        FAIL() if !(substr($X::str,$X::pos,$len) eq $s);
        $X::pos += $len;
        TAILCALL(&$c,$noop);
      };
    }
  }
}
{
  package Regexp::Parser::quant;

  # Needed for regexp_try.pl, but... bad idea for development.
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
          TAILCALL(&$c,$noop);
        }
        $pos_old = $X::pos;
        goto &$frest;
      };
      $frest = sub{
        if($i >= $max) {
          TAILCALL(&$c,$noop);
        }
        $i++;
        my $v = LET($X::pos){ $f->($fagain) }LET;
        return $v if not FAILED($v);
        TAILCALL(&$c,$noop);
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
          TAILCALL(&$c,$noop);
        }
        $pos_old = $X::pos;
        goto &$frest;
      };
      $frest = sub{
        if($i >= $max) {
          TAILCALL(&$c,$noop);
        }
        $i++;
        my $v = LET($X::pos){ $c->($noop) }LET;
        return $v if not FAILED($v);
        TAILCALL(&$f,$fagain);
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
      my $from = $X::pos;
      my $close = sub {
        my $c0 = $_[0];
        my $to = $X::pos;
        $m->set(1,substr($X::str,$from,$to-$from),[],{},$from,$to);
        TAILCALL(&$c0,$c);
      };
      return LET($X::cap){
        $X::cap = [@$X::cap];
        $X::cap->[$idx] = $m;
        my $v = $f->($close);
        $m->set_as_failed if FAILED($v);
        $v;
      }LET;
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
      FAIL() if $idx >= @$X::cap;
      my $m = $X::cap->[$idx];
      my $s = "$m";
      my $pat = $s;
      $pat =~ s/(\W)/\\$1/g;
      $pat = "(?:(?i)(?:$pat))" if($o->{'flags'} & $o->flag_val_i);
      my $ok = substr($X::str,$X::pos) =~ /\A($pat)/;
      FAIL() if !$ok;
      $X::pos += length($1);
      TAILCALL(&$c,$noop);
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
    if($dir>0) {
      sub {
        my $c = $_[0];
        { local($X::pos)=($X::pos);
          my $v = $f->($noop);
          FAIL_IF_FAILED($v);
        }
        TAILCALL(&$c,$noop);
      }
    } else {
      sub {
        my $c = $_[0];
        FAIL() if not &_is_found_backwards($f);
        TAILCALL(&$c,$noop);
      }
    }
  }
  sub _is_found_backwards {
    my($f)=@_;
    my $pos = $X::pos;
    local $X::pos = $X::pos;
    my $at_pos = sub{ FAIL() if $X::pos != $pos; return 1;};
    for(my $i = $X::pos;$i>=0;$i--) {
      $X::pos = $i;
      my $v = $f->($at_pos);
      return 1 if not FAILED($v);
    }
    return 0;
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
    if($dir>0) {
      sub {
        my $c = $_[0];
        my $v;
        { local($X::pos)=($X::pos);
          $v = $f->($noop);
          FAIL() if not FAILED($v);
        }
        TAILCALL(&$c,$noop);
      };
    } else {
      sub {
        my $c = $_[0];
        FAIL() if &Regexp::Parser::ifmatch::_is_found_backwards($f);
        TAILCALL(&$c,$noop);
      };
    }
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
      FAIL_IF_FAILED($v);
      TAILCALL(&$c,$noop);
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
    my $f_else = sub{my $c = $_[0]; TAILCALL(&$c,$noop);};
    $f_else = $o->data->[1]->data->[1][0]->emit if @{$o->data->[1]->data} > 1;
    sub {
      my $c = $_[0];
      my $v;
      { local($X::pos)=($X::pos);
        $v = $f_test->($noop);
      }
      if(not FAILED($v)) {
        TAILCALL(&$f_then,$c);
      } else {
        TAILCALL(&$f_else,$c);
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
      FAIL() if $idx >= @$X::cap;
      my $m = $X::cap->[$idx];
      FAIL() if !$m;
      TAILCALL(&$c,$noop);
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
    FAIL();
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
    TAILCALL(&$cn,$c);
  };

  my $v;
  { local $X::current_match = $m1;
    local $X::cap = [];
    $v = $ru->($rest);
  }
  FAIL_IF_FAILED($v);
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

sub mk_Match_appender {
  my($f,$name)=@_;
  sub {
    my $c = $_[0];
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
      $X::cap = $cap;
      $X::current_match = $m0;
      push(@{$X::cap},$m1);
      TAILCALL(&$cn,$c);
    };

    my $v;
    { local $X::current_match = $m1;
      local $X::cap = [];
      $v = $f->($rest);
    }
    FAIL_IF_FAILED($v);
    unshift(@{$m0},$m1);# sigh, prevents mutation.
    # why twice: once for inline code, once for the final Match tree.
    return $v;
  }
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
    if(not FAILED($ok)) {
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
