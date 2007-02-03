# CONTENTS
# Regexp Engine
#  package Regexp::ModuleA::ReentrantEngine;
#    package Regexp::ModuleA::ReentrantEngine::BacktrackMacros;
#    package Regexp::ModuleA::AST::BaseClass;
#  ...
#    package Regexp::ModuleA::AST::Namespace;
#   package Regexp::ModuleA::ReentrantEngine::Match;
# AST
#  package Regexp::ModuleA::AST
#   package Regexp::ModuleA::AST::Make;
#   package Regexp::ModuleA::AST::BaseClass;
#  ...
#   package Regexp::ModuleA::AST::Namespace;
# P5 regexps
#  package Regexp::ModuleA::P5;
# P6 regexps
#  package Regexp::ModuleA::P6;
# Rx
#  package Regexp::ModuleA::Rx;
# Read-eval-print loop
#  package Regexp::ModuleA::Interactive
# Command-line and glue.

package Regexp::ModuleA;
use strict;
use warnings;
use Carp;

#======================================================================
# Regexp Engine RMARE
#
# CAUTION: Moving the Engine to later in this file causes the perlbug
# workaround to stop working in perl v5.8.8. :(

package Regexp::ModuleA::ReentrantEngine;

{
  package Regexp::ModuleA::ReentrantEngine::BacktrackMacros;
  
  my @_let_stack;
  sub _let_gen {
    my($vars)=@_;
    my $nvars = 1+($vars =~ tr/,//);
    my $tmpvars = join(",",map{"\$__tmp${_}__"}(0..($nvars-1)));
    push(@_let_stack,[$vars,$tmpvars]);
    "(do{my \$__v__ ; my($tmpvars); { local($vars)=($vars); \$__v__ = do{ ";
  }
  sub _let_end {
    my $e = shift(@_let_stack) || die "LET(){ }LET pairs didn't match up";
    my($vars,$tmpvars) = @$e;
    "}; if(!FAILED(\$__v__)){ ($tmpvars)=($vars); }}; if(!FAILED(\$__v__)){ ($vars)=($tmpvars) }; \$__v__ })"
    }
  use Filter::Simple sub {
    s/\bLET\(([^\)]+)\)\{/_let_gen($1)/eg;
    s/\}LET;/_let_end().";"/eg;
    s/\bFAIL_IF_FAILED\(([^\)]+)\);/FAIL() if FAILED($1);/g;
    s/\bFAIL\(([^\)]{0,0})\)/return undef/g;
    s/\bFAILED\(([^\)]+)\)/(!defined($1)||(!ref($1)&&($1<=0)))/g;
    s/\bTAILCALL\(([^,\)]+),?([^\)]*)\);/\@_=($2);goto $1;/g;
    #print STDERR $_;
    $_;
  };
  1;
}
BEGIN { Regexp::ModuleA::ReentrantEngine::BacktrackMacros->import; };

use strict;
use warnings;

local $Regexp::ModuleA::ReentrantEngine::Env::str;
local $Regexp::ModuleA::ReentrantEngine::Env::pos;
local $Regexp::ModuleA::ReentrantEngine::Env::current_match;
local $Regexp::ModuleA::ReentrantEngine::Env::cap;


{
  package Regexp::ModuleA::AST::BaseClass;

  use Sub::Name;
  my $sub_id = 1;

  sub RMARE_emit {
    my $cls = ref($_[0]);
    die "bug: $cls RMARE_emit() unimplemented\n";
  }

  my $noop;
  $noop = subname "<noop ".($sub_id++).">" => sub {
    my $c = $_[0];
    return 1 if !defined($c) || $c eq $noop;
    TAILCALL(&$c,$noop);
  };
  sub RMARE_noop { $noop }

  sub RMARE_eat_backref {
    my($o,$idx)=@_;
    my $noop = $o->RMARE_noop;
    subname "<eat_backref ".($sub_id++).">" => sub {
      my $c = $_[0];
      FAIL() if $idx >= @$Regexp::ModuleA::ReentrantEngine::Env::cap;
      my $m = $Regexp::ModuleA::ReentrantEngine::Env::cap->[$idx];
      FAIL() if !defined($m) || !$m->match_boolean;
      my $re = $m->match_string;
      $re =~ s/(\W)/\\$1/g;

      my($str) = $Regexp::ModuleA::ReentrantEngine::Env::str;
      pos($str) = $Regexp::ModuleA::ReentrantEngine::Env::pos;
      $str =~ /\G($re)/ or FAIL();
      $Regexp::ModuleA::ReentrantEngine::Env::pos += length($1);
      TAILCALL(&$c,$noop);
    };
  }
  { use re 'eval';
  sub RMARE_eat_regexp {
    my($o,$re)=@_;
    my $noop = $o->RMARE_noop;
# print STDERR ">$re<\n";
    my $qr = qr/\G($re)/;
    subname "<eat_regexp ".($sub_id++).">" => sub {
      my $c = $_[0];

      my($str) = $Regexp::ModuleA::ReentrantEngine::Env::str;
      pos($str) = $Regexp::ModuleA::ReentrantEngine::Env::pos;
      $str =~ $qr or FAIL();
      $Regexp::ModuleA::ReentrantEngine::Env::pos += length($1);
      TAILCALL(&$c,$noop);
    }
  }
  }
  sub RMARE_wrap_re_with_mods {
    my($o,$re)=@_;
    my $mod = "";
    $mod .= "i" if $o->{flags}{i};
    $mod .= "m" if $o->{flags}{m};
    $mod .= "s" if $o->{flags}{s};
    $mod .= "x" if $o->{flags}{x};
    return $re if $mod eq "";
    "(?$mod:$re)";
  }
  sub RMARE_alt {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne 'ARRAY';
    my @fs = @$aref;
    my $f_last = pop(@fs);
    subname "<alt ".($sub_id++).">" => sub {
      my $c = $_[0];
      for my $f (@fs) {
        my $v = LET($Regexp::ModuleA::ReentrantEngine::Env::pos){ $f->($c) }LET;
        return $v if not FAILED($v);
      }
      TAILCALL(&$f_last,$c);
    }
  }
  sub RMARE_concat {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne 'ARRAY';
    my @a = @$aref;
    return $o->RMARE_noop if @a == 0;
    return $a[0]->RMARE_emit if @a == 1;
    my @fs = map { $_->RMARE_emit } @a;
    my $code1 = ""; my $code2 = "";
    my $code0 = "my \$f0 = \$fs[0]; ";
    for my $i (reverse(1..$#a)) {
      $code0 .= "my \$f$i = \$fs[$i]; ";
      $code1 .= "sub {\@_=";
      $code2 .= ";goto \&\$f$i}";
    }
    my $code = $code0."
#line 2 \"Regexp::ModuleA::AST::BaseClass RMARE_concat\"
\n subname '<concat '.(\$sub_id++).'>' => sub {my \$cn = \$_[0]; \@_=".$code1."\$cn".$code2.";goto \&\$f0}\n";
    eval($code) || die "$@";
  }   
  sub RMARE_repeat {
    my($o,$f,$min,$max,$ng)=@_;
    my $greedy = !$ng;
    my $noop = $o->RMARE_noop;
    subname "<repeat ".($sub_id++).">" => sub {
      if(!defined $noop){die "this perlbug workaround line didn't work"}
      my $c = $_[0];
      my $previous_pos = -1;
      my $count = 0;
      my($get_minimum,$try_getting_more);
      $get_minimum = sub {
        if($count < $min) {
          $count++;
          TAILCALL(&$f,$get_minimum);
        } else {
          goto &$try_getting_more;
        }
      };
      $try_getting_more = sub {
        if( !($previous_pos < $Regexp::ModuleA::ReentrantEngine::Env::pos) ||
            !($count < $max))
        {
          TAILCALL(&$c,$noop);
        }
        $previous_pos = $Regexp::ModuleA::ReentrantEngine::Env::pos;
        $count++;

        my $v = LET($Regexp::ModuleA::ReentrantEngine::Env::pos){
          $greedy ? $f->($try_getting_more) : $c->($noop);
        }LET;
        return $v if not FAILED($v);
        if($greedy){
          TAILCALL(&$c,$noop);
        } else {
          TAILCALL(&$f,$try_getting_more);
        }        
      };
      goto &$get_minimum;
    };
  }
  sub RMARE_capture {
    my($o,$idx,$f)=@_;
    my $myid = $sub_id++;
    subname "<capture ".($myid).">" => sub {
      my $c = $_[0];
      my $m = Regexp::ModuleA::ReentrantEngine::Match->new();
      my $from = $Regexp::ModuleA::ReentrantEngine::Env::pos;
      my $close = subname '<capture-close '.($myid).">" => sub {
        my $c0 = $_[0];
        my $to = $Regexp::ModuleA::ReentrantEngine::Env::pos;
        $m->match_set(1,substr($Regexp::ModuleA::ReentrantEngine::Env::str,$from,$to-$from),[],{},$from,$to);
        TAILCALL(&$c0,$c);
      };
      return LET($Regexp::ModuleA::ReentrantEngine::Env::cap){
        $Regexp::ModuleA::ReentrantEngine::Env::cap = [@$Regexp::ModuleA::ReentrantEngine::Env::cap];
        $Regexp::ModuleA::ReentrantEngine::Env::cap->[$idx] = $m;
        my $v = $f->($close);
        $m->match_set_as_failed if FAILED($v);
        $v;
      }LET;
    };
  }
  sub RMARE_subrule {
    my($o,$fetch,$name,$args)=@_;
    my $noop = $o->RMARE_noop;
    my $f = undef;
    my $myid = $sub_id++;
    subname "<subrule ".($myid).">" => sub {
      my($c)=@_;
      $f = $fetch->(@$args) if !defined $f;

      my $pos = $Regexp::ModuleA::ReentrantEngine::Env::pos;
      my $cap = $Regexp::ModuleA::ReentrantEngine::Env::cap;
      my $m0 = $Regexp::ModuleA::ReentrantEngine::Env::current_match;
      my $m1 = Regexp::ModuleA::ReentrantEngine::Match->new;
      $$m1->{'RULE'} ||= $name; #EEEP
      $m1->match_set(1,"",[],{},$pos,undef);

      my $rest = subname "<subrule-rest ".($myid).">" => sub {
	my $cn = $_[0];
	$$m1->{'match_array'} = $Regexp::ModuleA::ReentrantEngine::Env::cap; #EEEP
	$$m1->{'match_to'} = $Regexp::ModuleA::ReentrantEngine::Env::pos; #EEEP
	$$m1->{'match_string'} = substr($Regexp::ModuleA::ReentrantEngine::Env::str,$pos,$Regexp::ModuleA::ReentrantEngine::Env::pos-$pos);
	local $$m0->{'match_hash'}{$name} = [@{$$m0->{'match_hash'}{$name}||[]}];
	push(@{$$m0->{'match_hash'}{$name}},$m1); #see below
	$Regexp::ModuleA::ReentrantEngine::Env::cap = $cap;
	$Regexp::ModuleA::ReentrantEngine::Env::current_match = $m0;
	TAILCALL(&$cn,$c);
      };

      my $v;
      { local $Regexp::ModuleA::ReentrantEngine::Env::current_match = $m1;
	local $Regexp::ModuleA::ReentrantEngine::Env::cap = [];
	$v = $f->($rest);
      }
      FAIL_IF_FAILED($v);
      unshift(@{$$m0->{'match_hash'}{$name}},$m1);# sigh,
      # why twice?: once for inline code, once for the final Match tree.
      return $v;
    };
  }
  sub RMARE_do_match {
    my($o,$f,$s,$beginat,$minlen)=@_;
    my $nparen = $o->{nparen};
    my $len = length($s);
    $beginat = 0 if !defined($beginat);
    my $noop = $o->RMARE_noop;
    my $atend = $noop;
    if(defined $minlen) {
      my $min_end = $minlen + $beginat;
      $atend = subname "<atend ".($sub_id++).">" => sub {return undef if $Regexp::ModuleA::ReentrantEngine::Env::pos < $min_end;return 1;}
    }
    for my $start ($beginat..$len) {
      local $Regexp::ModuleA::ReentrantEngine::Env::str = $s;
      local $Regexp::ModuleA::ReentrantEngine::Env::pos = $start;
      local $Regexp::ModuleA::ReentrantEngine::Env::cap = [];
      my $m = Regexp::ModuleA::ReentrantEngine::Match->new();
      local $Regexp::ModuleA::ReentrantEngine::Env::current_match = $m;
      my $ok = $f->($atend);
      if(not FAILED($ok)) {
        my $a = $Regexp::ModuleA::ReentrantEngine::Env::cap;
        if(defined($nparen) && $nparen > @$a) {
          for my $i (@$a..$nparen) {
            push(@$a,Regexp::ModuleA::ReentrantEngine::Match->new()->match_set_as_failed);
          }
        }
        for my $am (@$a) {
          $am = Regexp::ModuleA::ReentrantEngine::Match->new()->match_set_as_failed() if !defined($am);
        }
        $m->match_set(1,substr($Regexp::ModuleA::ReentrantEngine::Env::str,$start,$Regexp::ModuleA::ReentrantEngine::Env::pos-$start),
                      $a,$$m->{'match_hash'},$start,$Regexp::ModuleA::ReentrantEngine::Env::pos);
        return $m;
      }
    }
    return Regexp::ModuleA::ReentrantEngine::Match->new()->match_set_as_failed;
  }

}

{
  # any regexp
  package Regexp::ModuleA::AST::Pat5;
  sub RMARE_emit {
    my($o)=@_;
    my $re = $o->RMARE_wrap_re_with_mods($o->{pat});
    $o->RMARE_eat_regexp($re);
  }
  
  # \Qabc\E
  package Regexp::ModuleA::AST::Exact;
  sub RMARE_emit {
    my($o)=@_;
    my $re = $o->{text};
    $re =~ s/(\W)/\\$1/g;
    $re = $o->RMARE_wrap_re_with_mods($re);
    $o->RMARE_eat_regexp($re);
  }

  # (?imsx-imsx:...)
  package Regexp::ModuleA::AST::Mod_expr;
  sub RMARE_emit {
    my($o)=@_;
    $o->{expr}->RMARE_emit;
  }
  
  # (?imsx-imsx)
  package Regexp::ModuleA::AST::Mod_inline;
  sub RMARE_emit {
    my($o)=@_;
    $o->RMARE_noop;
  }

  # ? * + {n,m} ?? *? etc
  package Regexp::ModuleA::AST::Quant;
  sub RMARE_emit {
    my($o)=@_;
    my($min,$max,$nongreedy)= (@$o{'min','max','nongreedy'});
    $min = 0 if !defined $min;
    $max = 1000**1000**1000 if !defined $max; #XXX inf
    my $f = $o->{expr}->RMARE_emit;
    $o->RMARE_repeat($f,$min,$max,$nongreedy);
  }

  # a|b
  package Regexp::ModuleA::AST::Alt;
  sub RMARE_emit {
    my($o)=@_;
    $o->RMARE_alt([map{$_->RMARE_emit}@{$o->{exprs}}]);
  }
  
  # ab
  package Regexp::ModuleA::AST::Seq;
  sub RMARE_emit {
    my($o)=@_;
    $o->RMARE_concat($o->{exprs});
  }
  
  # (?:a)
  package Regexp::ModuleA::AST::Grp;
  sub RMARE_emit {
    my($o)=@_;
    $o->{expr}->RMARE_emit;
  }
  
  # (a)
  package Regexp::ModuleA::AST::Cap5;
  sub RMARE_emit {
    my($o)=@_;
    my $idx = $o->{cap5_idx};
    my $f = $o->{expr}->RMARE_emit;
    $o->RMARE_capture($idx,$f);
  }
  
  # \1
  package Regexp::ModuleA::AST::Backref;
  sub RMARE_emit {
    my($o)=@_;
    my $noop = $o->RMARE_noop;
    my $idx = $o->{'backref_n'} -1;
    $o->RMARE_eat_backref($idx);
  }
  
  # <foo>
  package Regexp::ModuleA::AST::Subrule;
  use Sub::Name;
  sub RMARE_emit {
    my($o)=@_;
    my $args = $o->{args} || [];
    my $name = $o->{name};
    my $where = $Regexp::ModuleA::AST::Namespace::_current_namespace_;
    $where = $o->{created_in_pkg} if !defined $where;
    my $fetch = subname "<subrule-fetch for $name in $where>" => sub {
      my $subname = "${where}::$name";
      no strict;
      my $f = $subname->('api0');
      use strict;
      die "assert" if !defined $f;
      $f;
    };
    $o->RMARE_subrule($fetch,$name,$args);
  }
  
  # rx/a/
  package Regexp::ModuleA::AST::ARegex;
  use Sub::Name;
  sub RMARE_emit {
    my($o)=@_;
    my $f = $o->{expr}->RMARE_emit;
    my $matcher = subname "<an aregex-matcher for $o>" => sub {
      my($s,$beginat,$minlen)=@_;
      my $m = $o->RMARE_do_match($f,$s,$beginat,$minlen);
      $m->match_enable_overload;
    };
    my $h = {};
    my $rx = subname "<an aregex for $o>" => sub {
      my($request)=@_;
      if(@_ == 0) { return $matcher }
      if($request eq 'api0') { return $f }
      if($request eq 'RMARE-tree') { return $o }
      if($request eq 'hash') { return $h }
      Carp::confess("ui assert");
      die "ui assert";
    };
    bless $rx, 'Regexp::ModuleA::Rx';
  }
  
  # regex foo /a/; rule foo /a/; token foo /a/
  package Regexp::ModuleA::AST::Bind;
  sub RMARE_emit {
    my($o)=@_;
    my $name = $o->{name};
    my $where = $Regexp::ModuleA::AST::Namespace::_current_namespace_;
    $where = $o->{created_in_pkg} if !defined $where;
    local $Regexp::ModuleA::AST::Namespace::_current_name_ = $name;
    my $f = $o->{expr}->RMARE_emit;
    eval("package $where; *$name = \$f"); die "assert" if $@;
    $f;
  }
  
  # grammar Foo::Bar { ... }
  package Regexp::ModuleA::AST::Namespace;
  sub RMARE_emit {
    my($o)=@_;
    my $pkg = $o->{created_in_pkg};
    my $name = $o->{name};
    my $where = $name =~ /\A::(.*)/ ? $1 : $name eq '' ? $pkg : "$pkg::$name";
    eval("package $where;"); die "assert" if $@;
    local $Regexp::ModuleA::AST::Namespace::_current_namespace_ = $where;
    map{$_->RMARE_emit;} @{$o->{bindings}};
    undef;
  }

  # XXX high klude factor
  # (?{ ... })
  package Regexp::ModuleA::AST::Code;
  sub RMARE_emit {
    my($o)=@_;
    my $noop = $o->RMARE_noop;
    my $code = $o->{'code'};
    $code = "''" if $code =~ /\A\s*\z/;
    my $tmp = Regexp::ModuleA::AST::CodeRx::_rewrite_matchvars($o,$code);
    my $need_match if $code ne $tmp;
    $code = $tmp;
    my $src = '
#line 2 "in Regexp::ModuleA::Code"
sub{my $__c__ = $_[0];
'.(!$need_match ? '' :
'  my $M = $Regexp::ModuleA::ReentrantEngine::Env::current_match;
  $M->match_enable_overload;').'
 '.$code.';
 @_=$noop; goto &$__c__}';
    eval($src) || die "Error compiling (?{$code}) :\n$@\n";
  }

  # XXX high klude factor
  # (??{ ... })
  package Regexp::ModuleA::AST::CodeRx;
  sub RMARE_emit {
    my($o)=@_;
    my $code = $o->{'code'};
    $code = "''" if $code =~ /\A\s*\z/;
    my $tmp = $o->_rewrite_matchvars($code);
    my $need_match if $code ne $tmp;
    $code = $tmp;
    my $src = '
#line 2 "in Regexp::ModuleA::CodeRx"
sub{my $__c__ = $_[0]; my $__rx__ = ('.$code.');
'.(!$need_match ? '' :
'  my $M = $Regexp::ModuleA::ReentrantEngine::Env::current_match;
  $M->match_enable_overload;').'
  my $__f__ = (ref($__rx__) eq "Regexp" || !ref($__rx__)) ? $o->RMARE_eat_regexp("$__rx__") : $__rx__->("api0");
  $__f__->($__c__) }';
# print STDERR $src,"\n";
    eval($src) || die "Error compiling (?{$code}) :\n$@\n";
  }
  sub _rewrite_matchvars {
    my($o_ignored,$s)=@_;
    local $_ = $s;
    s/\$([1-9])/'$M->['.($1-1).']'/eg; #XXX more...
    $_;
  }

  # (?>)
  package Regexp::ModuleA::AST::Isolate;
  sub RMARE_emit {
    my($o)=@_;
    my $noop = $o->RMARE_noop;
    my $f = $o->{expr}->RMARE_emit;
    sub {
      my $c = $_[0];
      my $v = $f->($noop);
      FAIL_IF_FAILED($v);
      TAILCALL(&$c,$noop);
    };
  }

  # (?(n)t|f)
  package Regexp::ModuleA::AST::Conditional;
  sub RMARE_emit {
    my($o)=@_;
    my $noop = $o->RMARE_noop;
    my $f_test;
    my $f_then = $o->{expr_then}->RMARE_emit;
    my $f_else = ($o->{expr_else}
                  ? $o->{expr_false}->RMARE_emit
                  : sub{my $c = $_[0]; TAILCALL(&$c,$noop);});
    if($o->{test} !~ /\A\d+\z/) {
      $f_test = $o->{test_expr}->RMARE_emit;
    } else {
      my $idx = $o->{test} +0;
      $f_test = sub {
        my $c = $_[0];
        FAIL() if $idx >= @$Regexp::ModuleA::ReentrantEngine::Env::cap;
        my $m = $Regexp::ModuleA::ReentrantEngine::Env::cap->[$idx];
        FAIL() if !$m->match_boolean;
        TAILCALL(&$c,$noop);
      };
    }
    sub {
      my $c = $_[0];
      my $v;
      { local($Regexp::ModuleA::ReentrantEngine::Env::pos)=($Regexp::ModuleA::ReentrantEngine::Env::pos);
        $v = $f_test->($noop);
      }
      if(not FAILED($v)) {
        TAILCALL(&$f_then,$c);
      } else {
        TAILCALL(&$f_else,$c);
      }
    };
  }

  # (?=) (?<=) (?!) (?<!)
  package Regexp::ModuleA::AST::Lookaround;
  sub RMARE_emit {
    my($o)=@_;
    my $noop = $o->RMARE_noop;
    my $f = $o->{expr}->RMARE_emit;
    my $is_positive = $o->{is_positive};
    my $is_forward = $o->{is_forward};
    if($is_positive) {
      if($is_forward) {
        sub {
          my $c = $_[0];
          { local($Regexp::ModuleA::ReentrantEngine::Env::pos)=($Regexp::ModuleA::ReentrantEngine::Env::pos);
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
    } else {
      if($is_forward) {
        sub {
          my $c = $_[0];
          my $v;
          { local($Regexp::ModuleA::ReentrantEngine::Env::pos)=($Regexp::ModuleA::ReentrantEngine::Env::pos);
            $v = $f->($noop);
            FAIL() if not FAILED($v);
          }
          TAILCALL(&$c,$noop);
        };
      } else {
        sub {
          my $c = $_[0];
          FAIL() if &_is_found_backwards($f);
          TAILCALL(&$c,$noop);
        };
      }
    }
  }
  sub _is_found_backwards {
    my($f)=@_;
    my $pos = $Regexp::ModuleA::ReentrantEngine::Env::pos;
    local $Regexp::ModuleA::ReentrantEngine::Env::pos = $Regexp::ModuleA::ReentrantEngine::Env::pos;
    my $at_pos = sub{ FAIL() if $Regexp::ModuleA::ReentrantEngine::Env::pos != $pos; return 1;};
    for(my $i = $Regexp::ModuleA::ReentrantEngine::Env::pos;$i>=0;$i--) {
      $Regexp::ModuleA::ReentrantEngine::Env::pos = $i;
      my $v = $f->($at_pos);
      return 1 if not FAILED($v);
    }
    return 0;
  }

}



#======================================================================
# Match
#
{
  package Regexp::ModuleA::ReentrantEngine::MatchWithOverload;
  @Regexp::ModuleA::ReentrantEngine::MatchWithOverload::ISA =
    qw(Regexp::ModuleA::ReentrantEngine::Match);

  use overload
    'bool' => 'match_boolean',
    '""'   => 'match_string',
    '@{}'  => 'match_array',
    '%{}'  => 'match_hash',
    ;

  sub match_enable_overload {my($o)=@_; $o;}

  package Regexp::ModuleA::ReentrantEngine::Match;

  sub match_enable_overload {
    my($o)=@_;
    for my $m (@{$o->match_array}) { $m->match_enable_overload }
    for my $m (map{@$_}values %{$o->match_hash}) { $m->match_enable_overload }
    bless $o, 'Regexp::ModuleA::ReentrantEngine::MatchWithOverload';
  }

  sub match_boolean {${$_[0]}->{match_boolean}}
  sub match_string  {${$_[0]}->{match_string}}
  sub match_array   {${$_[0]}->{match_array}}
  sub match_hash    {${$_[0]}->{match_hash}}

  sub from          {${$_[0]}->{match_from}}
  sub to            {${$_[0]}->{match_to}}

  sub new {
    my($cls)=@_;
    my $h = {
      match_boolean => 1,
      match_string  => "",
      match_array   => [],
      match_hash    => {},
      match_from    => undef,
      match_to      => undef
      };
    my $o = \$h;
    bless $o,$cls;
    #$o->match_set(1,"",[],{});
    return $o;
  }
  sub match_set {
    my($o,$b,$s,$a,$h,$from,$to)=@_;
    $$o->{match_boolean} = $b;
    $$o->{match_string}  = $s;
    $$o->{match_array}   = $a;
    $$o->{match_hash}    = $h;
    $$o->{match_from}    = $from;
    $$o->{match_to}      = $to;
    return $o;
  }
  sub match_set_as_failed {
    my($o)=@_;
    $o->match_set(0,"",[],{});
    return $o;
  }
  
  sub match_describe {
    my($o,$verbose_p)=@_;
    my $os = "$o";
    $os = $o->match__indent_except_top($os) if $os =~ /\n/;
    my $s = $verbose_p ? $o->match__describe_name_as : "";
    $s .= "<".($o?"1":"0").",\"$os\",[";
    for (@{$o}) { $s .= "\n".$o->match__indent($_->match_describe())."," }
    $s .= "\n " if @{$o};
    $s .= "],{";
    for my $k (keys(%{$o})) {
      my $v = $o->{$k};
      my $vs = "";
      if(ref($v) eq 'ARRAY') {
        $vs = "[\n".$o->match__indent(join(",\n",map{
          $_->match_describe
          }@$v))."\n]";
      } else {
        $vs = $v->match_describe;
      }
      $s .= "\n  $k => " .$o->match__indent_except_top($vs)."," }
    $s .= "\n " if %{$o};
    $s .= "},";
    my($from,$to)=($o->from,$o->to);
    $from = "" if !defined $from;
    $to   = "" if !defined $to;
    $s .= "$from,$to>";
    return $s;
  }
  sub match__indent {my($o,$s)=@_; $s =~ s/\A(?!\Z)/  /mg; $s}
  sub match__indent_except_top {my($o,$s)=@_; $s =~ s/\A(?<!\A)(?!\Z)/  /mg; $s}
  sub match__describe_name_as {
    my($o)=@_;
    my $s = overload::StrVal($o);
    $s;
  }
}

#======================================================================
# AST
# 
{
  package Regexp::ModuleA::AST::Make;
  BEGIN{
  require Exporter;
  @Regexp::ModuleA::AST::Make::ISA=qw(Exporter);
  @Regexp::ModuleA::AST::Make::EXPORT_OK = qw(pat5 mod_expr mod_inline exact quant quant_ng alt seq cap5 grp sr aregex bind namespace  backref  ques star plus  ques_ng star_ng plus_ng  inf  code coderx isolate conditional lookaround);
  @Regexp::ModuleA::AST::Make::EXPORT    = @Regexp::ModuleA::AST::Make::EXPORT_OK;
  }
  sub pat5 { Regexp::ModuleA::AST::Pat5->new(@_) }
  sub mod_expr { Regexp::ModuleA::AST::Mod_expr->new(@_) }
  sub mod_inline { Regexp::ModuleA::AST::Mod_inline->new(@_) }
  sub exact { Regexp::ModuleA::AST::Exact->new(@_) }
  sub quant { Regexp::ModuleA::AST::Quant->new(@_) }
  sub quant_ng { Regexp::ModuleA::AST::Quant->new(@_,'ng') }
  sub alt { Regexp::ModuleA::AST::Alt->new(@_) }
  sub seq { Regexp::ModuleA::AST::Seq->new(@_) }
  sub cap5 { Regexp::ModuleA::AST::Cap5->new(@_) }
  sub grp { Regexp::ModuleA::AST::Grp->new(@_) }
  sub sr { my($pkg)=caller; Regexp::ModuleA::AST::Subrule->new($pkg,shift,[@_]) }
  sub aregex { Regexp::ModuleA::AST::ARegex->new(@_) }
  sub bind { my($pkg)=caller; Regexp::ModuleA::AST::Bind->new($pkg,@_) }
  sub namespace { my($pkg)=caller; Regexp::ModuleA::AST::Namespace->new($pkg,@_) }

  sub backref { Regexp::ModuleA::AST::Backref->new(@_) }
  sub code { Regexp::ModuleA::AST::Code->new(@_) }
  sub coderx { Regexp::ModuleA::AST::CodeRx->new(@_) }
  sub isolate { Regexp::ModuleA::AST::Isolate->new(@_) }
  sub conditional { Regexp::ModuleA::AST::Conditional->new(@_) }
  sub lookaround { Regexp::ModuleA::AST::Lookaround->new(@_) }


  sub ques { quant(0,1,    (@_ > 1 ? seq(@_) : @_)); }
  sub star { quant(0,undef,(@_ > 1 ? seq(@_) : @_)); }
  sub plus { quant(1,undef,(@_ > 1 ? seq(@_) : @_)); }

  sub ques_ng { quant_ng(0,1,    (@_ > 1 ? seq(@_) : @_)); }
  sub star_ng { quant_ng(0,undef,(@_ > 1 ? seq(@_) : @_)); }
  sub plus_ng { quant_ng(1,undef,(@_ > 1 ? seq(@_) : @_)); }

  sub inf () { 1000**1000**1000 } #XXX There has to be a better way, no?

}

{
  # AST::BaseClass
  package Regexp::ModuleA::AST::BaseClass;
  sub RAST_children {
    my($o)=@_;
    (exists($o->{expr})
     ? [$o->{expr}]
     : exists($o->{exprs})
     ? $o->{exprs}
     : []);
  }
  sub RAST_tell_children {
    my($o,$meth,@args)=@_;
    [map { $_->$meth(@args) } @{$o->RAST_children}];
  }
  sub RAST_init { shift->RAST_tell_children('RAST_init') }
  sub RAST_pass2 { shift->RAST_tell_children('RAST_pass2') }

  # AST::Pat5
  package Regexp::ModuleA::AST::Pat5;
  @Regexp::ModuleA::AST::Pat5::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$pat)=@_; die "api assert" if @_ != 2;
    bless {pat=>$pat}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    $o->{flags} = {%$Regexp::ModuleA::AST::Env::flags};
  }

  # AST::Exact
  package Regexp::ModuleA::AST::Exact;
  @Regexp::ModuleA::AST::Exact::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$text)=@_; die "api assert" if @_ != 2;
    bless {text=>$text}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    $o->{flags} = {%$Regexp::ModuleA::AST::Env::flags};
  }

  # AST::Mod_expr
  package Regexp::ModuleA::AST::Mod_expr;
  @Regexp::ModuleA::AST::Mod_expr::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub _new_hlp {
    my($cls,$modpat)=@_;
    my %m;
    for my $mod (split(":",$modpat)) {
      next if $mod eq '';
      $mod =~ /\A(\w+)(?:[[(<](.*?)[])>])?\z/ or die "assert";
      my($k,$v) = ($1,$2);
      $v = '1' if !defined $v;
      $v = eval($v);#X
        $m{$k} = $v;
    }
    \%m;
  }
  sub new {
    my($cls,$modpat,$expr)=@_; die "api assert" if @_ != 3;
    my $m = $cls->_new_hlp($modpat);
    bless {mods=>$m,expr=>$expr}, $cls;
  }
  sub _add_mods {
    my($o)=@_;
    my $flags = {%$Regexp::ModuleA::AST::Env::flags};
    foreach my $key (keys(%{$o->{mods}})) {
      $flags->{$key} = $o->{mods}{$key};
    }
    $flags;
  }
  sub RAST_init {
    my($o)=@_;
    local $Regexp::ModuleA::AST::Env::flags = $o->_add_mods;
    $o->{expr}->RAST_init;
  }
  
  # AST::Mod_inline
  package Regexp::ModuleA::AST::Mod_inline;
  @Regexp::ModuleA::AST::Mod_inline::ISA=qw(Regexp::ModuleA::AST::Mod_expr);#
    sub new {
      my($cls,$modpat)=@_; die "api assert" if @_ != 2;
      my $m = $cls->_new_hlp($modpat);
      bless {mods=>$m}, $cls;
    }
  sub RAST_init {
    my($o)=@_;
    $Regexp::ModuleA::AST::Env::flags = $o->_add_mods;
  }

  # AST::Backref
  package Regexp::ModuleA::AST::Backref;
  @Regexp::ModuleA::AST::Backref::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$idx)=@_; die "api assert" if @_ != 2;
    bless {backref_n=>$idx}, $cls;
  }
  sub RAST_pass2 {
    my($o)=@_;
    my $n = $o->{backref_n};
    my $total = $Regexp::ModuleA::AST::Env::nparen;
    die "Backreference to nonexistent group $n of $total"
      if $total < $n;
  }

  # AST::Cap5
  package Regexp::ModuleA::AST::Cap5;
  @Regexp::ModuleA::AST::Cap5::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$expr)=@_;
    bless {expr=>$expr}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    $o->{cap5_idx} = $Regexp::ModuleA::AST::Env::nparen++;
    local $Regexp::ModuleA::AST::Env::flags = {%$Regexp::ModuleA::AST::Env::flags};
    $o->{expr}->RAST_init;
  }

  # AST::Grp
  package Regexp::ModuleA::AST::Grp;
  @Regexp::ModuleA::AST::Grp::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$expr)=@_;
    bless {expr=>$expr}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    local $Regexp::ModuleA::AST::Env::flags = {%$Regexp::ModuleA::AST::Env::flags};
    $o->{expr}->RAST_init;
  }

  # AST::Quant
  package Regexp::ModuleA::AST::Quant;
  @Regexp::ModuleA::AST::Quant::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$min,$max,$expr,$nongreedy)=@_; die "api assert" if @_ < 4||@_ > 5;
    bless {min=>$min,max=>$max,expr=>$expr,nongreedy=>$nongreedy}, $cls;
  }

  # AST::Alt
  package Regexp::ModuleA::AST::Alt;
  @Regexp::ModuleA::AST::Alt::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,@exprs)=@_;
    bless {exprs=>\@exprs}, $cls;
  }

  # AST::Seq
  package Regexp::ModuleA::AST::Seq;
  @Regexp::ModuleA::AST::Seq::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,@exprs)=@_;
    bless {exprs=>\@exprs}, $cls;
  }

  # AST::Subrule
  package Regexp::ModuleA::AST::Subrule;
  @Regexp::ModuleA::AST::Subrule::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$pkg,$name,$args)=@_; die "api assert" if @_ != 4;
    bless {created_in_pkg=>$pkg,name=>$name,args=>$args}, $cls;
  }

  # AST::ARegex
  package Regexp::ModuleA::AST::ARegex;
  @Regexp::ModuleA::AST::ARegex::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$expr)=@_; die "api assert" if @_ != 2;
    bless {expr=>$expr}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    local $Regexp::ModuleA::AST::Env::nparen = 0;
    local $Regexp::ModuleA::AST::Env::flags = {};
#   local $Regexp::ModuleA::AST::Env::regex = $o;
    $o->{expr}->RAST_init;
    $o->{nparen} = $Regexp::ModuleA::AST::Env::nparen;
    $o->RAST_pass2;
  }

  # AST::Bind
  package Regexp::ModuleA::AST::Bind;
  @Regexp::ModuleA::AST::Bind::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$pkg,$name,$expr)=@_; die "api assert" if @_ != 4;
    bless {created_in_pkg=>$pkg,name=>$name,expr=>$expr}, $cls;
  }

  # AST::Namespace
  package Regexp::ModuleA::AST::Namespace;
  @Regexp::ModuleA::AST::Namespace::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$pkg,$name,@bindings)=@_; die "api assert" if @_ < 3;
    bless {created_in_pkg=>$pkg,name=>$name,bindings=>\@bindings}, $cls;
  }
  sub RAST_children { [@{shift->{bindings}}] }

  # AST::Code
  package Regexp::ModuleA::AST::Code;
  @Regexp::ModuleA::AST::Code::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$code)=@_; die "api assert" if @_ != 2;
    bless {code=>$code}, $cls;
  }

  # AST::CodeRx
  package Regexp::ModuleA::AST::CodeRx;
  @Regexp::ModuleA::AST::CodeRx::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$code)=@_; die "api assert" if @_ != 2;
    bless {code=>$code}, $cls;
  }

  # AST::Isolate
  package Regexp::ModuleA::AST::Isolate;
  @Regexp::ModuleA::AST::Isolate::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$expr)=@_; die "api assert" if @_ != 2;
    bless {expr=>$expr}, $cls;
  }

  # AST::Conditional
  package Regexp::ModuleA::AST::Conditional;
  @Regexp::ModuleA::AST::Conditional::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$test,$expr_then,$expr_else)=@_; die "api assert" if @_ < 3 || @_ > 4;
    bless {test=>$test,expr_then=>$expr_then,expr_else=>$expr_else}, $cls;
  }
  sub RAST_children { 
    my($o)=@_;
    my @ch;
    push(@ch,$o->{test}) if $o->{test} !~ /\A\d+\z/;
    push(@ch,$o->{expr_then});
    push(@ch,$o->{expr_else}) if $o->{expr_else};
    \@ch;
  }

  # AST::Lookaround
  package Regexp::ModuleA::AST::Lookaround;
  @Regexp::ModuleA::AST::Lookaround::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new { #XXX blech api
    my($cls,$is_forward,$is_positive,$expr)=@_; die "api assert" if @_ != 4;
    bless {is_forward=>$is_forward,is_positive=>$is_positive,expr=>$expr}, $cls;
  }
}



#======================================================================
# P5 Regexps
#
package Regexp::ModuleA::P5;
BEGIN { Regexp::ModuleA::AST::Make->import; };
use Regexp::Common;
my $nonmeta = '[^[)({^$?*+\\\\\.|]';
my $perlcode = ('(?:(?>[^][(){}"\'\/]+)'
                .'|'.$RE{balanced}{-parens=>'()[]{}'}
                .'|'.$RE{delimited}{-delim=>'\'"'}
                .'|'.$RE{delimited}{-delim=>'/'}
                .')*');
                                          
namespace(""
	  ,bind('pattern',aregex(sr('_pattern')))
          ,bind('_pattern',aregex(seq(sr('_non_alt'),star(exact('|'),sr('_non_alt')))))
	  ,bind('_non_alt',aregex(star(sr('_element'))))
	  ,bind('_element',aregex(seq(sr('_non_quant'),ques(pat5('[?*+]\??|{\d+(?:,\d*)?}\??')))))
	  ,bind('_non_quant',aregex(alt(sr('_mod_inline'),sr('_mod_expr'),sr('_code'),sr('_coderx'),sr('_isolate'),sr('_conditional'),sr('_lookaround'),sr('_cap5'),sr('_grp'),sr('_charclass'),sr('_backref_or_char'),sr('_esc'),sr('_nonmeta'),sr('_passthru'))))
	  ,bind('_mod_inline',aregex(pat5('\(\?[imsx-]+\)')))
	  ,bind('_mod_expr',aregex(seq(pat5('\(\?[imsx-]+:'),sr('_pattern'),exact(')'))))
	  ,bind('_grp',aregex(seq(exact('(?:'),sr('_pattern'),exact(')'))))
	  ,bind('_cap5',aregex(seq(pat5('\((?!\?)'),sr('_pattern'),exact(')'))))
	  ,bind('_charclass',aregex(pat5('\[\^?\]?([^\]\\\\]|\\\\.)*\]')))
	  ,bind('_backref_or_char',aregex(pat5('\\\\\d+')))
	  ,bind('_esc',aregex(pat5('\\\\[^\d]')))
	  ,bind('_nonmeta',aregex(pat5("$nonmeta(?:$nonmeta+(?![?*+{]))?")))
	  ,bind('_passthru',aregex(pat5('[$^.]')))
          ,bind('_code',aregex(seq(pat5('\(\?\{'),pat5($perlcode),pat5('}\)'))))
          ,bind('_coderx',aregex(seq(pat5('\(\?\?\{'),pat5($perlcode),pat5('}\)'))))
          ,bind('_isolate',aregex(seq(exact('(?>'),sr('_pattern'),exact(')'))))
          ,bind('_conditional',aregex(seq(exact('(?('),alt(pat5('\d+'),sr('_pattern')),exact(')'),sr('_pattern'),ques(exact('|'),sr('_pattern')),exact(')'))))
          ,bind('_lookaround',aregex(seq(pat5('\(\?<?[=!]'),sr('_pattern'),exact(')'))))
	  )->RMARE_emit;


sub match_tree_to_mexpr {
  my($m)=@_;
  my $mexpr = match_tree_to_mexpr_helper($m);
  if(!defined $mexpr) {
    return undef;
  }
  "aregex($mexpr)";
}
#XXX blech:
sub match_tree_to_mexpr_helper {
  my($m)=@_;
  my $r = $$m->{RULE};
  my @v = map{match_tree_to_mexpr_helper($_)} map{@$_} values(%{$m});
  my @ret = @v;
  if(defined($r)) {
    if($r eq '_nonmeta') {
      my $pat = "$m";
      $pat =~ s/\\([\\\'])/\\\\\\$1/g;
      @ret = ("exact('$pat')");
    }
    if($r eq '_passthru') {
      my $pat = "$m";
      $pat =~ s/\\([\\\'])/\\\\\\$1/g;
      @ret = ("pat5('$pat')");
    }
    if($r eq '_element') {
      my $s = "$m";
      my($e)= @v;
      if($s =~ /{(\d+)(?:,(\d*))?}(\?)?\z/) {
        my $ng = defined $3 ? '_ng' : '';
        my $min = $1;
        my $max = !defined $2 ? $min : $2 ne "" ? $2 : 1000**1000**1000; # inf
        $e = "quant${ng}($min,$max,$e)";
      }
      elsif($s =~ /([?*+])(\?)?\z/) {
        my $ng = defined $2 ? '_ng' : '';
        $e = "ques${ng}($e)" if $1 eq '?';
        $e = "star${ng}($e)" if $1 eq '*';
        $e = "plus${ng}($e)" if $1 eq '+';
      }
      @ret = ($e);
    }
    if($r eq '_backref_or_char') {
      "$m" =~ /\A\\(\d+)\z/ or die "bug";
      my $n = $1;
      if($n !~ /\A0/ && $n < 10) {
        @ret = ("backref($n)");
      } else {
        # XXX kludge. Interpretation of \10 is much more complex.
        @ret = ("pat5('\\\\$n')");
      }
    }
    if($r eq '_esc') {
      my $pat = "$m";
      $pat =~ s/\\([\\\'])/\\\\\\$1/g;
      @ret = ("pat5('$pat')");
    }
    if($r eq '_charclass') {
      my $pat = "$m";
      $pat =~ s/\\([\\\'])/\\\\\\$1/g;
      @ret = ("pat5('$pat')");
    }
    if($r eq '_grp') {
      @ret = ("grp($v[0])");
    }
    if($r eq '_cap5') {
      @ret = ("cap5($v[0])");
    }
    if($r eq '_mod_expr') {
      "$m" =~ /\A\(\?([imsx]*)(?:-([imsx]*))?/ or die 'bug';
      my $on  = join(":",split("",$1));
      my $off = join(":",map{"${_}(0)"}split("",defined $2 ? $2 : ""));
      @ret = ("mod_expr('$on:$off',$v[0])");
    }
    if($r eq '_mod_inline') {
      "$m" =~ /\A\(\?([imsx]*)(?:-([imsx]*))?/ or die 'bug';
      my $on  = join(":",split("",$1));
      my $off = join(":",map{"${_}(0)"}split("",defined $2 ? $2 : ""));
      @ret = ("mod_inline('$on:$off')");
    }
    if($r eq '_non_alt') {
      @ret = @v != 1 ? ("seq(".join(",",@v).")") : @v;
    }
    if($r eq '_pattern') {
      @ret = @v > 1 ? ("alt(".join(",",@v).")") : @v;
    }
    if($r eq '_code' || $r eq '_coderx') {
      "$m" =~ /\A\((\?\??){(.*?)}\)\z/ or die "bug";
      my($which,$code) = ($1,$2);
      @ret = ($which eq '?' ? "code(q{$code})" : "coderx(q{$code})");
    }
    if($r eq '_isolate') {
      @ret = ("isolate($v[0])");
    }
    if($r eq '_conditional') {
      "$m" =~ /\A\(\?\((.*?)\)/ or die "bug";
      my $test_ish = $1;
      my $test;
      my $v_i = 0;
      if($test_ish =~ /\A\d+\z/) {
        $test = $test_ish;
      } else {
        $test = $v[$v_i++];
      }
      my $expr_then = $v[$v_i++];
      my $expr_else = $v[$v_i++];
      my $then_else = $expr_then.(defined $expr_else ? ",$expr_else" : "");
      @ret = ("conditional($test,$then_else)");
    }
    if($r eq '_lookaround') {
      "$m" =~ /\A\(\?(<?[=!])/ or die "bug";
      my $flavor = $1;
      my $args = {'='=>[1,1],
                  '!'=>[1,0],
                  '<='=>[0,1],
                  '<!'=>[0,0]}->{$flavor};
      my $s = join(",",@$args);
      @ret = ("lookaround($s,$v[0])");
    }
  }
  return wantarray ? @ret : $ret[0];
}

sub mk_matcher_from_re {
  my($re)=@_;
  my $match = pattern->()($re);
  if(!$match || $match->from != 0 || $match->to != length($re)) {
    my $err = "Regexp syntax error:";
    Carp::confess "$err / <-- HERE $re/" if $match->from != 0; #XX should set beginat
    my $at = $match->to+1;
    Carp::confess "$err /".substr($re,0,$at)." <-- HERE ".substr($re,$at)."/";
  }
#print $match->match_describe,"\n";
  my $m_exp = match_tree_to_mexpr($match);
print $m_exp,"\n";
  die "assert" if !defined $m_exp;
  my $ast = eval($m_exp);
  die if $@;
  $ast->RAST_init;
#use Data::Dumper;
#print Dumper $ast;
  my $matcher = $ast->RMARE_emit;
  wantarray ? ($match,$m_exp,$ast,$matcher) : $matcher;
}

#======================================================================
# P6 Regexps
#


#======================================================================
# Rx
#
package Regexp::ModuleA::Rx;

sub new {
  my($cls,$pat,$mods)=@_;
  my $h = {
    pattern => $pat,
    modifiers => $mods
    };
  my $re = $pat;
  $re = "(?$mods)$re" if $mods;
  $h->{regexp} = $re;
  my $o = eval { Regexp::ModuleA::P5::mk_matcher_from_re($re); };
  Carp::confess "compile \"$re\" failed: $@" if !defined $o;
  %{$o->('hash')} = %$h;
  bless $o, $cls;
  $o;
}

sub match {
  my($o,$str)=@_;
  $o->()($str);
}

sub match_re {
  my($pat,$mods,$str)=@_;
  ($mods,$str) = (undef,$mods) if !defined $str;
  my $o = __PACKAGE__->new($pat,$mods);
  $o->match($str);
}

#======================================================================
# Interactive
#
{
  package Regexp::ModuleA::Interactive;
  sub convert_p5_re_literal_to_p5_re {
    use re 'eval';
    my($lit5)=@_;
    $lit5 =~ s/\A\s+//; $lit5 =~ s/\s+\z//;
    
    my $modre = qr/[imsxogce]/;
    my %close = ('('=>qr/\)/,'{'=>qr/}/,'['=>qr/]/,'<'=>qr/>/);
    my $cl = sub{my $s = $_[0]; $close{$s}||qr/$s/ };
    my($op,$delim,$pat5,$delimC,$subst,$mod5);
    if($lit5 =~ /\A()(\/)(.+?)(\/)()($modre*)\z/) {
      ($op,$delim,$pat5,$delimC,$subst,$mod5)=($1,$2,$3,$4,$5,$6);
    }
    elsif($lit5 =~ /\A(qr|m)(.)(.+?)((??{$cl->($2)}))()($modre*)\z/) {
      ($op,$delim,$pat5,$delimC,$subst,$mod5)=($1,$2,$3,$4,$5,$6);
    }
    # s///ubstitution is not supported.
    #  elsif($lit5 =~ /\A(s)(.)(.+?)((??{$cl->($2)}))\2?(.+?)\4($modre*)\z/){
    #    ($op,$delim,$pat5,$delimC,$subst,$mod5)=($1,$2,$3,$4,$5,$6);
    #  }
    else { die "invalid literal: $lit5" }

    return $pat5 if $mod5 eq '';
    return "(?$mod5:$pat5)";
  }
  sub repl {
    print "Enter a Perl 5 regexp pattern or literal.\n";
    my $prompt1 = "Regex: ";
    print $prompt1;
    while(<>) {
      chomp;
      my $re = $_;
      if(/\A(\/|(m|s|qr)\W)/) {
        $re = convert_p5_re_literal_to_p5_re($re);
        print "As regexp: $re\n";
      }
      my($match,$m_exp,$ast,$matcher) = Regexp::ModuleA::P5::mk_matcher_from_re($re);
      print "As m-expr: ",$m_exp,"\n";
      print "Enter string to match against.  Blank line to stop.\nstring: ";
      while(<>) {
        chomp;
        last if /\A\z/;
        print $matcher->()($_)->match_describe(),"\n";
        print "string: ";
      }
      print $prompt1;
    }
  }
}

#======================================================================
# Command-line and glue. 
#

# Also used by t/re_tests.t.  Replace that with API, once it exists.
sub Regexp::ModuleA::test_target {
  sub {
    my($mods,$re)=@_;
    my $o = Regexp::ModuleA::Rx->new($re,$mods);
    sub{my($s)=@_;$o->match($s)}
  };
}

if($0 eq __FILE__ && @ARGV) {
  if($ARGV[0] eq '--test') {
    require './t/re_tests.pl';
    Pkg_re_tests::test(&Regexp::ModuleA::test_target);
    exit;
  }
  if($ARGV[0] eq '--repl') {
    shift;
    Regexp::ModuleA::Interactive::repl();
    exit;
  }
}


1;
__END__
#; Local Variables:
#; perl-indent-level: 2
#; perl-continued-statement-offset: 2
#; perl-continued-brace-offset: -2
#; indent-tabs-mode: nil
#; End:
#; vim: shiftwidth=2:
