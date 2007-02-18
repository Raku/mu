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
#   package Regexp::ModuleA::AST::Make0;
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
    s/\bFAIL_IF_FAILED\(([^\)]+)\);/return($1) if FAILED($1);/g;
    s/\bFAIL\(([^\)]{0,0})\)/return undef/g;
    s/\bFAIL_SEQUENCE\(([^\)]{0,0})\)/die "fail sequence\n"/g;
    s/\bFAIL_GROUP\(([^\)]{0,0})\)/die "fail group\n"/g;
    s/\bFAIL_REGEX\(([^\)]{0,0})\)/die "fail regex\n"/g;
    s/\bFAIL_MATCH\(([^\)]{0,0})\)/die "fail match\n"/g;
    s/\bFAILED\(([^\)]+)\)/(!defined($1)||(!ref($1)&&($1<=0)))/g;
    s/\bTAILCALL\(([^,\)]+),?([^\)]*)\);/\@_=($2);goto \&$1;/g;
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
local $Regexp::ModuleA::ReentrantEngine::Env::pkg;
local $Regexp::ModuleA::ReentrantEngine::Env::nested_data;

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
    TAILCALL($c,$noop);
  };
  sub RMARE_noop { $noop }

  sub RMARE_eat_backref {
    my($o,$idx,$mod5_re)=@_;
    my $noop = $o->RMARE_noop;
    subname "<eat_backref ".($sub_id++).">" => sub {
      my $c = $_[0];
      my $a = $$Regexp::ModuleA::ReentrantEngine::Env::current_match->{match_array};
      FAIL() if $idx >= @$a;
      my $m = $a->[$idx];
      FAIL() if !defined($m) || !$m->match_boolean;
      my $re = $m->match_string;
      $re =~ s/(\W)/\\$1/g;

      my($str) = $Regexp::ModuleA::ReentrantEngine::Env::str;
      pos($str) = $Regexp::ModuleA::ReentrantEngine::Env::pos;
      $str =~ /\G$mod5_re($re)/ or FAIL();
      $Regexp::ModuleA::ReentrantEngine::Env::pos += length($1);
      TAILCALL($c,$noop);
    };
  }
  { use re 'eval';
  sub RMARE_eat_regexp {
    my($o,$re)=@_;
    my $noop = $o->RMARE_noop;
    my $qr = qr/\G($re)/;
    subname "<eat_regexp ".($sub_id++).">" => sub {
      my $c = $_[0];

      my($str) = $Regexp::ModuleA::ReentrantEngine::Env::str;
      pos($str) = $Regexp::ModuleA::ReentrantEngine::Env::pos;
      $str =~ $qr or FAIL();
      $Regexp::ModuleA::ReentrantEngine::Env::pos += length($1);
      TAILCALL($c,$noop);
    }
  }
  }
  sub RMARE_imsx {
    my($o)=@_;
    my $mod = "";
    $mod .= "i" if $o->{flags}{i};
    $mod .= "m" if $o->{flags}{m};
    $mod .= "s" if $o->{flags}{s};
    $mod .= "x" if $o->{flags}{x};
    $mod;
  }
  sub RMARE_wrap_re_with_mods {
    my($o,$re)=@_;
    my $mod = $o->RMARE_imsx;
    return $re if $mod eq "";
    "(?$mod:$re)";
  }
  sub RMARE_alt {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne 'ARRAY';
    my @fs = @$aref;
    subname "<alt ".($sub_id++).">" => sub {
      my $c = $_[0];
      for my $f (@fs) {
        my $v = LET($Regexp::ModuleA::ReentrantEngine::Env::pos){
          my $v1 = eval { $f->($c) }; #try
          if($@) {
            next if $@ eq "fail sequence\n";
            die unless $@ eq "fail group\n";
            FAIL();
          }
          $v1;
        }LET;
        return $v if not FAILED($v);
      }
      FAIL();
    };
  }
  sub RMARE_group {
    my($o,$f)=@_;
    subname "<group ".($sub_id++).">" => sub {
      my $cn = $_[0];
      my $nd = $Regexp::ModuleA::ReentrantEngine::Env::nested_data;
      my $close = sub {
        my($c)=@_;
        $Regexp::ModuleA::ReentrantEngine::Env::nested_data = $nd;
        TAILCALL($cn,$c);
      };
      my $v = eval {$f->($close)}; #try
      if($@) {
        die unless $@ eq "fail group\n" || $@ eq "fail sequence\n";
        FAIL();
      }
      return $v;
    };
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
      if(!defined $noop){die "this perl v5.8.8 bug workaround line didn't work"}
      my $c = $_[0];
      my $previous_pos = -1;
      my $count = 0;
      my($get_minimum,$try_getting_more);
      $get_minimum = subname "get_minimum" => sub {
        if($count < $min) {
          $count++;
          TAILCALL($f,$get_minimum);
        } else {
          goto &$try_getting_more;
        }
      };
      $try_getting_more = subname "try_getting_more" => sub {
        if( !($previous_pos < $Regexp::ModuleA::ReentrantEngine::Env::pos) ||
            !($count < $max))
        {
          TAILCALL($c,$noop);
        }
        $previous_pos = $Regexp::ModuleA::ReentrantEngine::Env::pos;
        $count++;

        my $v = LET($Regexp::ModuleA::ReentrantEngine::Env::pos){
          $greedy ? $f->($try_getting_more) : $c->($noop);
        }LET;
        return $v if not FAILED($v);
        if($greedy){
          TAILCALL($c,$noop);
        } else {
          TAILCALL($f,$try_getting_more);
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
      my $nd = $Regexp::ModuleA::ReentrantEngine::Env::nested_data;
      my $close = subname '<capture-close '.($myid).">" => sub {
        my $c0 = $_[0];
        $Regexp::ModuleA::ReentrantEngine::Env::nested_data = $nd;
        my $to = $Regexp::ModuleA::ReentrantEngine::Env::pos;
        $m->match_set(1,substr($Regexp::ModuleA::ReentrantEngine::Env::str,$from,$to-$from),[],{},$from,$to);
        TAILCALL($c0,$c);
      };
      return LET($$Regexp::ModuleA::ReentrantEngine::Env::current_match->{match_array}){
        my $newa = [@{$$Regexp::ModuleA::ReentrantEngine::Env::current_match->{match_array}}];
        $$Regexp::ModuleA::ReentrantEngine::Env::current_match->{match_array} = $newa;
        $newa->[$idx] = $m;
        my $v = eval { $f->($close) }; #try
        if($@) {
          die unless $@ eq "fail group\n" || $@ eq "fail sequence\n";
          $m->match_set_as_failed;
          FAIL();
        }
        $m->match_set_as_failed if FAILED($v);
        $v;
      }LET;
    };
  }
  sub RMARE_subrule {
    my($o,$fetch,$pkg,$name,$args)=@_;
    my $noop = $o->RMARE_noop;
    my $myid = $sub_id++;
    subname "<subrule ".($myid)." $name>" => sub {
      my($c)=@_;
      my $f = $fetch->(@$args);

      my $pkg0 = $Regexp::ModuleA::ReentrantEngine::Env::pkg;
      my $pkg9 = $Regexp::ModuleA::ReentrantEngine::Env::pkg || $pkg;

      my $pos = $Regexp::ModuleA::ReentrantEngine::Env::pos;
      my $m0 = $Regexp::ModuleA::ReentrantEngine::Env::current_match;

      my $nd = $Regexp::ModuleA::ReentrantEngine::Env::nested_data;

      my $m1 = Regexp::ModuleA::ReentrantEngine::Match->new;
      $$m1->{'RULE'} ||= $name; #EEEP
      $m1->match_set(1,"",[],{},$pos,undef);

      my $close = subname "<subrule-close ".($myid)." $name>" => sub {
	my $cn = $_[0];
        $Regexp::ModuleA::ReentrantEngine::Env::nested_data = $nd;

	$$m1->{'match_to'} = $Regexp::ModuleA::ReentrantEngine::Env::pos; #EEEP
	$$m1->{'match_string'} = substr($Regexp::ModuleA::ReentrantEngine::Env::str,$pos,$Regexp::ModuleA::ReentrantEngine::Env::pos-$pos);

        my $post = $name."__post_action";
        if(UNIVERSAL::can($pkg9,$post)) {
          $m1->match_enable_overload;
          $pkg9->$post($m1);
        }

	$Regexp::ModuleA::ReentrantEngine::Env::current_match = $m0;
	local $Regexp::ModuleA::ReentrantEngine::Env::pkg = $pkg0;

        LET($$m0->{'match_hash'}{$name}){
          $$m0->{'match_hash'}{$name} = [@{$$m0->{'match_hash'}{$name}||[]}];
          push(@{$$m0->{'match_hash'}{$name}},$m1);
          $cn->($c);
        }LET;
      };

      my $v;
      { local $Regexp::ModuleA::ReentrantEngine::Env::current_match = $m1;
        local $Regexp::ModuleA::ReentrantEngine::Env::pkg = $pkg0;
	$v = $f->($close);
      }
      FAIL_IF_FAILED($v);
      return $v;
    };
  }
  sub RMARE_aregex {
    my($o,$f)=@_;
    my $nparen = $o->{nparen};
    subname "<aregex ".($sub_id++).">" => sub {
      my($c)=@_;

      my $m = $Regexp::ModuleA::ReentrantEngine::Env::current_match;
      my $a = [map{Regexp::ModuleA::ReentrantEngine::Match->new()->match_set_as_failed} (1..$nparen)];
      $$m->{match_array} = $a;

      my $v = eval { $f->($c) }; #try
      if($@) {
        die unless ($@ eq "fail regex\n" || $@ eq "fail group\n" ||
                    $@ eq "fail sequence\n");
        FAIL();
      }
      $v;
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
        my $m = Regexp::ModuleA::ReentrantEngine::Match->new();
        local $Regexp::ModuleA::ReentrantEngine::Env::current_match = $m;
        local $Regexp::ModuleA::ReentrantEngine::Env::nested_data = {};

        my $ok = eval { $f->($atend) }; #try
        if($@) {
          die unless ($@ eq "fail match\n" || $@ eq "fail regex\n" ||
                      $@ eq "fail group\n" || $@ eq "fail sequence\n");
          last;
        }
        if(not FAILED($ok)) {
          $m->match_set(1,substr($Regexp::ModuleA::ReentrantEngine::Env::str,$start,$Regexp::ModuleA::ReentrantEngine::Env::pos-$start),$$m->{match_array},$$m->{'match_hash'},$start,$Regexp::ModuleA::ReentrantEngine::Env::pos);
          return $m;
        }
      }
    return Regexp::ModuleA::ReentrantEngine::Match->new()->match_set_as_failed;
  }
  sub RMARE_commit_sequence {
    my($o)=@_;
    my $noop = $o->RMARE_noop;
    subname "<commit_sequence ".($sub_id++).">" => sub {
      my($c)=@_;
      my $v = $c->($noop);
      FAIL_SEQUENCE() if FAILED($v);
      return $v;
    };
  }
  sub RMARE_commit_group {
    my($o)=@_;
    my $noop = $o->RMARE_noop;
    subname "<commit_group ".($sub_id++).">" => sub {
      my($c)=@_;
      my $v = $c->($noop);
      FAIL_GROUP() if FAILED($v);
      return $v;
    };
  }
  sub RMARE_commit_regex {
    my($o)=@_;
    my $noop = $o->RMARE_noop;
    subname "<commit_regex ".($sub_id++).">" => sub {
      my($c)=@_;
      my $v = $c->($noop);
      FAIL_REGEX() if FAILED($v);
      return $v;
    };
  }
  sub RMARE_commit_match {
    my($o)=@_;
    my $noop = $o->RMARE_noop;
    subname "<commit_regex ".($sub_id++).">" => sub {
      my($c)=@_;
      my $v = $c->($noop);
      FAIL_MATCH() if FAILED($v);
      return $v;
    };
  }

  sub RMARE_independent {
    my($o,$f)=@_;
    my $noop = $o->RMARE_noop;
    subname "<independent ".($sub_id++).">" => sub {
      my $cn = $_[0];
      my $uid = "independent ".rand()."\n";
      my $nbt = sub {
        my $c = $_[0];
        my $v = $c->($cn);
        die $uid if FAILED($v);
        $v;
      };
      my $v = eval { $f->($nbt) };
      if($@) {
        die if $@ ne $uid;
        FAIL();
      }
      $v;
    };
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
    $re =~ s/([^\w\s])/\\$1/g;
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
    die "assert - Quant min <= max" if $min > $max;
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
    $o->RMARE_group($o->{expr}->RMARE_emit);
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
    $o->RMARE_eat_backref($idx,'(?'.$o->RMARE_imsx.')');
  } #XXX move imsx into eat
  
  # <foo>
  package Regexp::ModuleA::AST::Subrule;
  use Sub::Name;
  sub RMARE_emit {
    my($o)=@_;
    my $args = $o->{args} || [];
    my $pkg = $o->{pkg};
    my $name = $o->{name};
    my $fetch = subname "<subrule-fetch for $name>" => sub {
      my $pkg9 = $Regexp::ModuleA::ReentrantEngine::Env::pkg || $pkg;
      die "assert" if !defined $pkg9;
      no strict;
      my $f = $pkg9->$name($name)->(' api0');
      use strict;
      die "assert" if !defined $f;
      $f;
    };
    $o->RMARE_subrule($fetch,$pkg,$name,$args);
  }
  
  # rx/a/
  package Regexp::ModuleA::AST::ARegex;
  use Sub::Name;
  sub RMARE_emit {
    my($o)=@_;
    my $pkg = $o->{pkg};
    my $name = $o->{name};
    my $f = $o->RMARE_aregex($o->{expr}->RMARE_emit);
    # Why the extra sub?  60+% shorter re_text runtime.  sigh.
    my $matchergen = sub { subname "<an aregex-matcher for $o>" => sub {
      my($pkg9,$name1,$s,$beginat,$minlen)=@_;
      local $Regexp::ModuleA::ReentrantEngine::Env::pkg = $pkg9;
      my $m = $o->RMARE_do_match($f,$s,$beginat,$minlen);
      $m->match_enable_overload;
      $$m->{RULE} = $name1;
      if($name1) {
        my $post = $name1."__post_action";
        $pkg9->$post($m) if UNIVERSAL::can($pkg9,$post);
      }
      $m;
    } };
    Regexp::ModuleA::Rx->_new_from_ast($o,$pkg,$name,$f,$matchergen);
  }    
  
  # regex foo /a/; rule foo /a/; token foo /a/
  package Regexp::ModuleA::AST::Bind;
  use Sub::Name;
  sub RMARE_emit {
    my($o)=@_;
    my $pkg = $o->{pkg};
    my $name = $o->{name};
    my $fr = $o->{expr}->RMARE_emit;
    eval("package $pkg; *$name = \$fr"); die "assert" if $@;
    $fr;
  }
  
  # grammar Foo::Bar { ... }
  package Regexp::ModuleA::AST::Namespace;
  sub RMARE_emit {
    my($o)=@_;
    my $pkg = $o->{pkg};
    eval("package $pkg;"); die "assert" if $@;
    map{$_->RMARE_emit;} @{$o->{bindings}};
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
    my $need_match = $code ne $tmp || $code =~ /\$M\b/;
    $code = $tmp;
    my $src = '
#line 2 "in Regexp::ModuleA::Code"
sub{my $__c__ = $_[0];
'.(!$need_match ? '' :
'  my $M = $Regexp::ModuleA::ReentrantEngine::Env::current_match;
  $M->match_enable_overload;').'
 '.$code.';
 $__c__->($noop);}';
    #print STDERR $src,"\n";
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
    my $need_match = $code ne $tmp || $code =~ /\$M\b/;
    $code = $tmp;
    #XXX Really need to PPI the code.
    my $has_local = $code =~ /\blocal\b/;
    my $has_semi = $code =~ /;/;
    $code = ($has_semi && !$has_local) ? "do{$code}" : "($code)";
    warn "(??{...}) currently doesnt support code with multiple statments and local()" if $has_local && $has_semi;
    my $src = '
#line 2 "in Regexp::ModuleA::CodeRx"
sub{my $__c__ = $_[0];
'.(!$need_match ? '' :
'  my $M = $Regexp::ModuleA::ReentrantEngine::Env::current_match;
  $M->match_enable_overload;').'
  my $__rx__ = '.$code.';
  die "(??{...}) returned undef" if !defined $__rx__;
#  $__rx__ = "(?!)" if !defined $__rx__;
  my $__f__ = (ref($__rx__) eq "Regexp" || !ref($__rx__)) ? $o->RMARE_eat_regexp("$__rx__") : $__rx__->(" api0");
  $__f__->($__c__) }';
    #print STDERR $src,"\n";
    eval($src) || die "Error compiling (?{$code}) :\n$@\n";
  }
  sub _rewrite_matchvars {
    my($o_ignored,$s)=@_;
    local $_ = $s;
    s/\$([1-9])/'$M->['.($1-1).']'/eg; #XXX more...
    $_;
  }

  # (?>)
  package Regexp::ModuleA::AST::Independent;
  sub RMARE_emit {
    my($o)=@_;
    my $f = $o->{expr}->RMARE_emit;
    $o->RMARE_independent($f);
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
                  : sub{my $c = $_[0]; TAILCALL($c,$noop);});
    if($o->{test} !~ /\A\d+\z/) {
      $f_test = $o->{test_expr}->RMARE_emit;
    } else {
      my $idx = $o->{test} +0;
      $f_test = sub {
        my $c = $_[0];
        my $a = $Regexp::ModuleA::ReentrantEngine::Env::current_match->match_array;
        FAIL() if $idx >= @$a;
        my $m = $a->[$idx];
        FAIL() if !$m->match_boolean;
        TAILCALL($c,$noop);
      };
    }
    sub {
      my $c = $_[0];
      my $v;
      { local($Regexp::ModuleA::ReentrantEngine::Env::pos)=($Regexp::ModuleA::ReentrantEngine::Env::pos);
        $v = $f_test->($noop);
      }
      if(not FAILED($v)) {
        TAILCALL($f_then,$c);
      } else {
        TAILCALL($f_else,$c);
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
          TAILCALL($c,$noop);
        }
      } else {
        sub {
          my $c = $_[0];
          FAIL() if not &_is_found_backwards($f);
          TAILCALL($c,$noop);
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
          TAILCALL($c,$noop);
        };
      } else {
        sub {
          my $c = $_[0];
          FAIL() if &_is_found_backwards($f);
          TAILCALL($c,$noop);
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

  # nonexistent
  package Regexp::ModuleA::AST::CommitSequence;
  sub RMARE_emit {
    my($o)=@_;
    $o->RMARE_commit_sequence();
  }

  # ::
  package Regexp::ModuleA::AST::CommitGroup;
  sub RMARE_emit {
    my($o)=@_;
    $o->RMARE_commit_group();
  }

  # :::
  package Regexp::ModuleA::AST::CommitRegex;
  sub RMARE_emit {
    my($o)=@_;
    $o->RMARE_commit_regex();
  }

  # <commit>
  package Regexp::ModuleA::AST::CommitMatch;
  sub RMARE_emit {
    my($o)=@_;
    $o->RMARE_commit_match();
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

#  sub match_enable_overload {my($o)=@_; $o;}

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

  sub match_value   {${$_[0]}->{match_value}}

  sub new {
    my($cls)=@_;
    my $h = {
      match_boolean => 1,
      match_string  => "",
      match_array   => [],
      match_hash    => {},
      match_from    => undef,
      match_to      => undef,
      match_value   => undef
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
    $$o->{match_value}   = undef;
    return $o;
  }
  sub match_set_as_failed {
    my($o)=@_;
    $o->match_set(0,"",[],{});
    return $o;
  }
  sub match_value_set {
    my($o,$v)=@_;
    $$o->{match_value} = $v;
  }
  
  sub match_describe {
    my($o,$verbose_p)=@_;
    my $vp = $verbose_p;
    my $os = $o->match_string;
    $os = $o->match__indent_except_top($os) if $os =~ /\n/;
    my $s = $verbose_p ? $o->match__describe_name_as : "";
    $s .= "<".($o->match_boolean?"1":"0").",\"$os\",[";
    for (@{$o->match_array}) { $s .= "\n".$o->match__indent($_->match_describe($vp))."," }
    $s .= "\n " if @{$o->match_array};
    $s .= "],{";
    for my $k (keys(%{$o->match_hash})) {
      my $v = $o->match_hash->{$k};
      my $vs = "";
      if(ref($v) eq 'ARRAY') {
        $vs = "[\n".$o->match__indent(join(",\n",map{
          $_->match_describe($vp)
          }@$v))."\n]";
      } else {
        $vs = $v->match_describe($vp);
      }
      $s .= "\n  $k => " .$o->match__indent_except_top($vs)."," }
    $s .= "\n " if %{$o->match_hash};
    $s .= "},";
    my($from,$to)=($o->from,$o->to);
    $from = "" if !defined $from;
    $to   = "" if !defined $to;
    $s .= "$from,$to";
    my $val = $o->match_value;
    $s .= defined $val ? ",$val" : "";
    $s .= ">";
    return $s;
  }
  sub match__indent {my($o,$s)=@_; $s =~ s/^(?!\Z)/  /mg; $s}
  sub match__indent_except_top {my($o,$s)=@_; $s =~ s/^(?<!\A)(?!\Z)/  /mg; $s}
  sub match__describe_name_as {
    my($o)=@_;
    my $s = overload::StrVal($o);
    $s .= "{".$$o->{RULE}."}" if defined $$o->{RULE};
    $s;
  }
}

#======================================================================
# AST
# 
{
  package Regexp::ModuleA::AST::Make0;
  BEGIN{
  require Exporter;
  @Regexp::ModuleA::AST::Make0::ISA=qw(Exporter);
  @Regexp::ModuleA::AST::Make0::EXPORT_OK = qw(pat5 mod_expr mod_inline exact quant quant_ng alt seq cap5 grp sr aregex bind namespace  backref  ques star plus  ques_ng star_ng plus_ng  inf  code coderx independent conditional lookaround commit_sequence commit_group commit_regex commit_match);
  @Regexp::ModuleA::AST::Make0::EXPORT    = @Regexp::ModuleA::AST::Make0::EXPORT_OK;
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
  sub independent { Regexp::ModuleA::AST::Independent->new(@_) }
  sub conditional { Regexp::ModuleA::AST::Conditional->new(@_) }
  sub lookaround { Regexp::ModuleA::AST::Lookaround->new(@_) }
  sub commit_sequence { Regexp::ModuleA::AST::CommitSequence->new(@_) }
  sub commit_group { Regexp::ModuleA::AST::CommitGroup->new(@_) }
  sub commit_regex { Regexp::ModuleA::AST::CommitRegex->new(@_) }
  sub commit_match { Regexp::ModuleA::AST::CommitMatch->new(@_) }


  sub ques { quant(0,1,    (@_ > 1 ? seq(@_) : @_)); }
  sub star { quant(0,undef,(@_ > 1 ? seq(@_) : @_)); }
  sub plus { quant(1,undef,(@_ > 1 ? seq(@_) : @_)); }

  sub ques_ng { quant_ng(0,1,    (@_ > 1 ? seq(@_) : @_)); }
  sub star_ng { quant_ng(0,undef,(@_ > 1 ? seq(@_) : @_)); }
  sub plus_ng { quant_ng(1,undef,(@_ > 1 ? seq(@_) : @_)); }

  sub inf () { 1000**1000**1000 } #XXX There has to be a better way, no?

}

{
  local $Regexp::ModuleA::AST::Env::pkg;
  local $Regexp::ModuleA::AST::Env::name;

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
  sub RAST_pass1 {
    my($o)=@_;
    $o->{flags} = $Regexp::ModuleA::AST::Env::flags;
    shift->RAST_tell_children('RAST_pass1');
  }
  sub RAST_pass2 { shift->RAST_tell_children('RAST_pass2') }

  sub RAST_to_Make0 {
    my($o)=@_;
    my($cls) = ref($o) =~ /([^:]+)$/;
    my $name = lc $cls;
    $name.'('.$o->RAST_to_Make0_children.')';
  }
  sub RAST_to_Make0_children {
    my($o)=@_;
    my $args = $o->RAST_tell_children('RAST_to_Make0');
    join(",\n",@$args);
  }
  sub RAST_quote {
    my($o,$s)=@_;
    $s =~ s/([\\\'])/\\$1/g;
    "'$s'";
  }

  # AST::Pat5
  package Regexp::ModuleA::AST::Pat5;
  @Regexp::ModuleA::AST::Pat5::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$pat)=@_; die "api assert" if @_ != 2;
    bless {pat=>$pat}, $cls;
  }
  sub RAST_to_Make0 {
    my($o)=@_;
    'pat5('.($o->RAST_quote($o->{pat})).')';
  }

  # AST::Exact
  package Regexp::ModuleA::AST::Exact;
  @Regexp::ModuleA::AST::Exact::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$text)=@_; die "api assert" if @_ != 2;
    bless {text=>$text}, $cls;
  }
  sub RAST_to_Make0 {
    my($o)=@_;
    'exact('.($o->RAST_quote($o->{text})).')';
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
  sub RAST_pass1 {
    my($o)=@_;
    local $Regexp::ModuleA::AST::Env::flags = $o->_add_mods;
    $o->{flags} = $Regexp::ModuleA::AST::Env::flags;
    $o->{expr}->RAST_pass1;
  }
  sub _RAST_to_Make0_hlp {
    my($o)=@_;
    my $modpat = join("",map{
      my $k = $_;
      my $v = $o->{mods}{$k};
      my $vs = $v eq '1' ? "" : "<$v>";
      ":$k$vs"
      } keys(%{$o->{mods}}));
    $o->RAST_quote($modpat);
  }
  sub RAST_to_Make0 {
    my($o)=@_;
    'mod_expr('.$o->_RAST_to_Make0_hlp.",".$o->RAST_to_Make0_children.')';
  }
  
  # AST::Mod_inline
  package Regexp::ModuleA::AST::Mod_inline;
  @Regexp::ModuleA::AST::Mod_inline::ISA=qw(Regexp::ModuleA::AST::Mod_expr);#
    sub new {
      my($cls,$modpat)=@_; die "api assert" if @_ != 2;
      my $m = $cls->_new_hlp($modpat);
      bless {mods=>$m}, $cls;
    }
  sub RAST_pass1 {
    my($o)=@_;
    $Regexp::ModuleA::AST::Env::flags = $o->_add_mods;
    $o->{flags} = $Regexp::ModuleA::AST::Env::flags;
  }
  sub RAST_to_Make0 {
    my($o)=@_;
    'mod_inline('.$o->_RAST_to_Make0_hlp.')';
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
  sub RAST_to_Make0 {
    my($o)=@_;
    'backref('.$o->{backref_n}.')';
  }

  # AST::Cap5
  package Regexp::ModuleA::AST::Cap5;
  @Regexp::ModuleA::AST::Cap5::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$expr)=@_;
    bless {expr=>$expr}, $cls;
  }
  sub RAST_pass1 {
    my($o)=@_;
    $o->{cap5_idx} = $Regexp::ModuleA::AST::Env::nparen++;
    $o->{flags} = $Regexp::ModuleA::AST::Env::flags;
    local $Regexp::ModuleA::AST::Env::flags = {%$Regexp::ModuleA::AST::Env::flags};
    $o->{expr}->RAST_pass1;
  }

  # AST::Grp
  package Regexp::ModuleA::AST::Grp;
  @Regexp::ModuleA::AST::Grp::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$expr)=@_;
    bless {expr=>$expr}, $cls;
  }
  sub RAST_pass1 {
    my($o)=@_;
    $o->{flags} = $Regexp::ModuleA::AST::Env::flags;
    local $Regexp::ModuleA::AST::Env::flags = {%$Regexp::ModuleA::AST::Env::flags};
    $o->{expr}->RAST_pass1;
  }

  # AST::Quant
  package Regexp::ModuleA::AST::Quant;
  @Regexp::ModuleA::AST::Quant::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$min,$max,$expr,$nongreedy)=@_; die "api assert" if @_ < 4||@_ > 5;
    bless {min=>$min,max=>$max,expr=>$expr,nongreedy=>$nongreedy}, $cls;
  }
  sub RAST_to_Make0 {
    my($o)=@_;
    my $min = $o->{min}; $min = 'undef' if !defined $min;
    my $max = $o->{max}; $max = 'undef' if !defined $max;
    my $expr = $o->RAST_to_Make0_children;
    my $ng = $o->{nongreedy}; $ng = defined $ng ? ",'ng'" : "";
    'quant('."$min,$max,$expr$ng".')';
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
    my($cls,$inpkg,$name,$args)=@_; die "api assert" if @_ != 4;
    bless {created_in_pkg=>$inpkg,name=>$name,args=>$args}, $cls;
  }
  sub RAST_to_Make0 {
    my($o)=@_;
    my $args = $o->{args};
    my $x = defined $args ? ",".$o->RAST_quote($args) : "";
    'sr('.$o->RAST_quote($o->{name}).$x.')';
  }
  sub RAST_pass1 {
    my($o)=@_;
    $o->{pkg} = $Regexp::ModuleA::AST::Env::pkg || $o->{inpkg};
    $o->{flags} = $Regexp::ModuleA::AST::Env::flags;
    $o;
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
    $o->{pkg} = $Regexp::ModuleA::AST::Env::pkg || $o->{inpkg};
    local $Regexp::ModuleA::AST::Env::pkg = $o->{pkg};
    $o->{name} = $Regexp::ModuleA::AST::Env::name;
    local $Regexp::ModuleA::AST::Env::nparen = 0;
    local $Regexp::ModuleA::AST::Env::flags = {};
    $o->RAST_pass1;
    $o->{nparen} = $Regexp::ModuleA::AST::Env::nparen;
    $o->RAST_pass2;
    $o;
  }

  # AST::Bind
  package Regexp::ModuleA::AST::Bind;
  @Regexp::ModuleA::AST::Bind::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$inpkg,$name,$expr)=@_; die "api assert" if @_ != 4;
    die "api assert $name"  if $name =~ /::/;
    bless {created_in_pkg=>$inpkg,name=>$name,expr=>$expr}, $cls;
  }
  sub RAST_to_Make0 {
    my($o)=@_;
    'bind('.$o->RAST_quote($o->{name}).','.$o->RAST_to_Make0_children.')';
  }
  sub RAST_init {
    my($o)=@_;
    $o->{pkg} = $Regexp::ModuleA::AST::Env::pkg || $o->{inpkg};
    local $Regexp::ModuleA::AST::Env::pkg = $o->{pkg};
    local $Regexp::ModuleA::AST::Env::name = $o->{name};
    $o->{expr}->RAST_init;
    $o;
  }

  # AST::Namespace
  package Regexp::ModuleA::AST::Namespace;
  @Regexp::ModuleA::AST::Namespace::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$inpkg,$nsname,@bindings)=@_; die "api assert" if @_ < 3;
    my $pkg = ($nsname =~ /\A::(.*)/) ? $1 : $nsname eq '' ? $inpkg : "${inpkg}::$nsname";
    bless {created_in_pkg=>$inpkg,nsname=>$nsname,bindings=>\@bindings,pkg=>$pkg}, $cls;
  }
  sub RAST_children { [@{shift->{bindings}}] }
  sub RAST_to_Make0 {
    my($o)=@_;
    'namespace('.$o->RAST_quote($o->{nsname}).",\n".$o->RAST_to_Make0_children.')';
  }
  sub RAST_init {
    my($o)=@_;
    local $Regexp::ModuleA::AST::Env::pkg = $o->{pkg};
    $o->RAST_tell_children('RAST_init');
    $o;
  }

  # AST::Code
  package Regexp::ModuleA::AST::Code;
  @Regexp::ModuleA::AST::Code::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$code)=@_; die "api assert" if @_ != 2;
    bless {code=>$code}, $cls;
  }
  sub RAST_to_Make0 {
    my($o)=@_;
    'code('.$o->RAST_quote($o->{code}).')';
  }

  # AST::CodeRx
  package Regexp::ModuleA::AST::CodeRx;
  @Regexp::ModuleA::AST::CodeRx::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$code)=@_; die "api assert" if @_ != 2;
    bless {code=>$code}, $cls;
  }
  sub RAST_to_Make0 {
    my($o)=@_;
    'coderx('.$o->RAST_quote($o->{code}).')';
  }

  # AST::Independent
  package Regexp::ModuleA::AST::Independent;
  @Regexp::ModuleA::AST::Independent::ISA=qw(Regexp::ModuleA::AST::BaseClass);
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
  sub RAST_to_Make0 {
    my($o)=@_;
    my $test = $o->{test};
    my $n = ($test =~ /^\d+$/) ? "$test," : "";
    'conditional('."$n".$o->RAST_to_Make0_children.')';
  }

  # AST::Lookaround
  package Regexp::ModuleA::AST::Lookaround;
  @Regexp::ModuleA::AST::Lookaround::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new { #XXX blech api
    my($cls,$is_forward,$is_positive,$expr)=@_; die "api assert" if @_ != 4;
    bless {is_forward=>$is_forward,is_positive=>$is_positive,expr=>$expr}, $cls;
  }
  sub RAST_to_Make0 {
    my($o)=@_;
    my $a = $o->{is_forward} ? '1' : '0';
    my $b = $o->{is_positive} ? '1' : '0';
    'lookaround('."$a,$b,".$o->RAST_to_Make0_children.')';
  }

  # AST::CommitSequence
  package Regexp::ModuleA::AST::CommitSequence;
  @Regexp::ModuleA::AST::CommitSequence::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls)=@_; die "api assert" if @_ != 1;
    bless {}, $cls;
  }
  # AST::CommitGroup
  package Regexp::ModuleA::AST::CommitGroup;
  @Regexp::ModuleA::AST::CommitGroup::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls)=@_; die "api assert" if @_ != 1;
    bless {}, $cls;
  }
  # AST::CommitRegex
  package Regexp::ModuleA::AST::CommitRegex;
  @Regexp::ModuleA::AST::CommitRegex::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls)=@_; die "api assert" if @_ != 1;
    bless {}, $cls;
  }
  # AST::CommitMatch
  package Regexp::ModuleA::AST::CommitMatch;
  @Regexp::ModuleA::AST::CommitMatch::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls)=@_; die "api assert" if @_ != 1;
    bless {}, $cls;
  }


}



#======================================================================
# P5 Regexps
#
package Regexp::ModuleA::P5;
BEGIN { Regexp::ModuleA::AST::Make0->import; };
use Regexp::Common;
sub mod_helper {
  my($mod)=@_;
  my $h = {%$Regexp::ModuleA::ReentrantEngine::Env::nested_data};
  my($on,$off) = split('-',$mod);
  if($on){for my $x (unpack('c*',$on)){$h->{$x}=1}}
  if($off){for my $x (unpack('c*',$off)){$h->{$x}=0}}
  $Regexp::ModuleA::ReentrantEngine::Env::nested_data = $h;
}
sub mod_x_or_fail {
  $Regexp::ModuleA::ReentrantEngine::Env::nested_data->{x} ? qr// : qr/(?!)/;  
}
{
  my $nonmeta = '[^[)({^$?*+\\\\\.|]';
  my $perlcode = ('(?:(?>[^][(){}"\'\/]+)'
                  .'|'.$RE{balanced}{-parens=>'()[]{}'}
                  .'|'.$RE{delimited}{-delim=>'\'"'}
                  .'|'.$RE{delimited}{-delim=>'/'}
                  .')*');
  
  namespace(""
            ,bind('regex',aregex(sr('pattern')))
            ,bind('pattern',aregex(sr('_pattern')))
            ,bind('_pattern',aregex(seq(sr('_non_alt'),star(exact('|'),sr('_non_alt')))))
            ,bind('_non_alt',aregex(star(sr('_element'))))
            ,bind('_element',aregex(seq(sr('_non_quant'),ques(pat5('[?*+]\??|{\d+(?:,\d*)?}\??')))))
            ,bind('_non_quant',aregex(alt(sr('_mod_inline'),sr('_mod_expr'),sr('_code'),sr('_coderx'),sr('_independent'),sr('_conditional'),sr('_lookaround'),sr('_cap5'),sr('_grp'),sr('_charclass'),sr('_backref_or_char'),sr('_esc'),sr('_nonmeta'),sr('_passthru'),sr('_subrule'))))
            ,bind('_mod_inline',aregex(pat5('\(\?([imsx-]+)\)(?{Regexp::ModuleA::P5::mod_helper($^N)})')))
            ,bind('_mod_expr',aregex(seq(pat5('\(\?([imsx-]+):(?{Regexp::ModuleA::P5::mod_helper($^N)})'),sr('_pattern'),exact(')'))))
            ,bind('_grp',aregex(seq(exact('(?:'),sr('_pattern'),exact(')'))))
            ,bind('_cap5',aregex(seq(pat5('\((?!\?)'),sr('_pattern'),exact(')'))))
            ,bind('_charclass',aregex(pat5('\[\^?\]?([^\]\\\\]|\\\\.)*\]\]?')))#X
            ,bind('_backref_or_char',aregex(pat5('\\\\\d+')))
            ,bind('_esc',aregex(pat5('\\\\[^\d]')))
            ,bind('_nonmeta',aregex(pat5("$nonmeta(?:$nonmeta+(?![?*+{]))?")))
            ,bind('_passthru',aregex(pat5('[$^.]')))
            ,bind('_code',aregex(seq(exact('(?{'),pat5($perlcode),exact('})'))))
            ,bind('_coderx',aregex(seq(exact('(??{'),pat5($perlcode),exact('})'))))
            ,bind('_independent',aregex(seq(exact('(?>'),sr('_pattern'),exact(')'))))
            ,bind('_conditional',aregex(seq(exact('(?('),alt(pat5('\d+'),sr('_pattern')),exact(')'),sr('_pattern'),ques(exact('|'),sr('_pattern')),exact(')'))))
            ,bind('_lookaround',aregex(seq(pat5('\(\?<?[=!]'),sr('_pattern'),exact(')'))))
            ,bind('_subrule',aregex(pat5('(?!)')))
            )->RAST_init->RMARE_emit;
}
sub make0_from_match {
  my($cls,$m)=@_;
  my $r = $$m->{RULE};
  return $m if !defined $r;
  my @v = map{$cls->make0_from_match($_)} map{@$_} values(%{$m});
  if(0) {}
  elsif($r eq '_nonmeta') {
    my $pat = "$m";
    $pat =~ s/\\([\\\'])/\\\\\\$1/g;
    return "exact('$pat')";
  }
  elsif($r eq '_passthru') {
    my $pat = "$m";
    $pat =~ s/\\([\\\'])/\\\\\\$1/g;
    return "pat5('$pat')";
  }
  elsif($r eq '_element') {
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
    return $e;
  }
  elsif($r eq '_backref_or_char') {
    "$m" =~ /\A\\(\d+)\z/ or die "bug";
    my $n = $1;
    if($n !~ /\A0/ && $n < 10) {
      return "backref($n)";
    } else {
      # XXX kludge. Interpretation of \10 is much more complex.
      return "pat5('\\\\$n')";
    }
  }
  elsif($r eq '_esc') {
    my $pat = "$m";
    $pat =~ s/\\([\\\'])/\\\\\\$1/g;
    return "pat5('$pat')";
  }
  elsif($r eq '_charclass') {
    my $pat = "$m";
    $pat =~ s/\\([\\\'])/\\\\\\$1/g;
    return "pat5('$pat')";
  }
  elsif($r eq '_grp') {
    return "grp($v[0])";
  }
  elsif($r eq '_cap5') {
    return "cap5($v[0])";
  }
  elsif($r eq '_mod_expr' || $r eq '_mod_inline') {
    "$m" =~ /\A\(\?([imsx]*)(?:-([imsx]*))?/ or die 'bug';
    my $on  = join("",map{":${_}"} split("",$1));
    my $off = join("",map{":${_}<0>"} split("",defined $2 ? $2 : ""));
    return ($r eq '_mod_expr'
            ? "mod_expr('$on$off',$v[0])"
            : "mod_inline('$on$off')");
  }
  elsif($r eq '_non_alt') {
    return (@v != 1 ? ("seq(".join(",",@v).")") : $v[0]);
  }
  elsif($r eq '_pattern') {
    return (@v > 1 ? ("alt(".join(",",@v).")") : $v[0]);
  }
  elsif($r eq '_code' || $r eq '_coderx') {
    "$m" =~ /\A\((\?\??){(.*?)}\)\z/ or die "bug";
    my($which,$code) = ($1,$2);
    return ($which eq '?' ? "code(q{$code})" : "coderx(q{$code})");
  }
  elsif($r eq '_independent') {
    return "independent($v[0])";
  }
  elsif($r eq '_conditional') {
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
    return "conditional($test,$then_else)";
  }
  elsif($r eq '_lookaround') {
    "$m" =~ /\A\(\?(<?[=!])/ or die "bug";
    my $flavor = $1;
    my $args = {'='=>[1,1],
                '!'=>[1,0],
                '<='=>[0,1],
                '<!'=>[0,0]}->{$flavor};
    my $s = join(",",@$args);
    return "lookaround($s,$v[0])";
  }
  elsif($r eq 'regex') {
    return "aregex($v[0])";
  }
  else {
    return $v[0];
  }
}

sub new_rx_from_re {
  my($cls,$pat,$mods)=@_;
  my $re = $pat;
  $re = "(?$mods)$re" if $mods;
  my $verbose = 0;
  my($m,$mexpr,$ast);
  my $o = eval {
    $m = $cls->regex()->match($re);
    print STDERR $m->match_describe,"\n" if $verbose;
    if(!$m || $m->from != 0 || $m->to != length($re)) {
      my $err = "Regexp syntax error:";
      Carp::confess "$err / <== HERE $re/" if $m->from != 0; #XX should set beginat
      my $at = $m->to+1;
      Carp::confess "$err /".substr($re,0,$at)." <== HERE ".substr($re,$at)."/";
    }
    $mexpr = $cls->make0_from_match($m);
    die "assert" if !defined $mexpr;
    print STDERR $mexpr,"\n" if $verbose;
    $ast = eval("namespace('::$cls',$mexpr)");
    die if $@;
    $ast->RAST_init;
    my($rx) = $ast->RMARE_emit;
    $rx;
  };
  Carp::confess "compile \"$re\" failed: $@" if !defined $o;
  $o->_init($pat,$mods,$re,$mexpr,$ast);
}


#======================================================================
# P5 Regexps with subrules
#
package Regexp::ModuleA::P5WithSubrules;
@Regexp::ModuleA::P5WithSubrules::ISA=qw(Regexp::ModuleA::P5);
BEGIN { Regexp::ModuleA::AST::Make0->import; };

{
  my $nonmeta = '[^[)({^$?*+\\\\\.|<]';
  namespace(""
            ,bind('_subrule',aregex(seq(pat5('\<\w+'),ques(seq(pat5('\s+'),plus(sr('pattern')))),exact('>'))))
            ,bind('_nonmeta',aregex(pat5("$nonmeta(?:$nonmeta+(?![?*+{]))?")))
            ,bind('t1',aregex(pat5('\w{2}')))
            )->RAST_init->RMARE_emit;
}
sub make0_from_match {
  my($cls,$m)=@_;
  my $r = $$m->{RULE};
  return $m if !defined $r;
  my @v = map{$cls->make0_from_match($_)} map{@$_} values(%{$m});
  if(0){}
  elsif($r eq '_subrule') {
    "$m" =~ /\A<(\w+)/ or die "bug";
    my $name = $1;
    my $args = (@v ? "," : "").join(",",@v);
    return "sr('$name'$args)";
  }
  else {
    return $cls->SUPER::make0_from_match($m);
  }
}



#======================================================================
# P6 Regexps
#
package Regexp::ModuleA::P6;
@Regexp::ModuleA::P6::ISA=qw(Regexp::ModuleA::P5WithSubrules);
BEGIN { Regexp::ModuleA::AST::Make0->import; };
  
namespace(""
          ,bind('regex',aregex(seq(mod_inline(':x'),sr('pattern'))))
          );

sub make0_from_match {
  my($cls,$m)=@_;
  my $r = $$m->{RULE};
  return $m if !defined $r;
  my @v = map{$cls->make0_from_match($_)} map{@$_} values(%{$m});
  if(0){}
  elsif($r eq 'regex') {
    return "aregex(seq(mod_inline(':x'),$v[0]))";
  }
  else {
    return $cls->SUPER::make0_from_match($m);
  }
}

#======================================================================
# Rx
#
package Regexp::ModuleA::Rx;
use Sub::Name;

sub _new_from_ast {
  my($rxclass,$ast,$pkg,$name,$f,$matchergen)=@_;
  $pkg ||= "";
  my $h = {ast=>$ast,pkg=>$pkg,name=>$name,f=>$f,matchergen=>$matchergen};
  my $self;
  my $showname = $name || '*anon*';
  $self = subname "<an aregex for $ast $pkg $showname>" => sub {
    if(@_ == 0) {
      return $self;
    }
    elsif($_[0] !~ /^ /) {
      my($cls,$method)=@_; Carp::confess "api assert" if @_ > 2;
      $method ||= $name;
      if($cls eq $pkg && $method eq $name) {
        return $self;
      }
      else {
        return $rxclass->_new_from_ast($ast,$cls,$method,$f,$matchergen);
      }
    }
    else {
      my($request)=@_;
      if($request eq ' api0') { return $f }
      if($request eq ' hash') { return $h }
      if($request eq ' match') {
        shift @_;
        return $matchergen->()($pkg,$name,@_);
      }
    }
    Carp::confess("ui assert");
    die "ui assert";
  };
  bless $self, $rxclass;
}
sub _init {
  my($o,$pat,$mods,$re,$mexpr,$ast)=@_;
  my $h = $o->(' hash');
  $h->{pattern} = $pat;
  $h->{modifiers} = $mods;
  $h->{regexp} = $re;
  $h->{mexpr} = $mexpr;
  $h->{ast} = $ast;
  $o;
}

sub match {
  my($o,$str)=@_;
  $o->(' match',$str);
}

sub _mexpr {
  my($o)=@_;
  $o->(' hash')->{mexpr};
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
    print "Or 6:pattern for a Perl 6 regex.\n";
    my $prompt1 = "Regex: ";
    print $prompt1;
    while(<>) {
      chomp;
      my $re = $_;
      my $dialect;
      #$dialect = 'Regexp::ModuleA::P5';
      $dialect = 'Regexp::ModuleA::P5WithSubrules';
      if($re =~ /^6:(.*)/){
        $dialect = 'Regexp::ModuleA::P6';
        $re = $1;
      }
      if(/\A(\/|(m|s|qr)\W)/) {
        $re = convert_p5_re_literal_to_p5_re($re);
        print "As regexp: $re\n";
      }
      my $rx = $dialect->new_rx_from_re($re);
      print "As m-expr: ",$rx->_mexpr,"\n";
      print "Enter string to match against.  Blank line to stop.\nstring: ";
      while(<>) {
        chomp;
        last if /\A\z/;
        print $rx->match($_)->match_describe(),"\n";
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
    my $o = Regexp::ModuleA::P5->new_rx_from_re($re,$mods);
    sub{my($s)=@_;$o->match($s)}
  };
}
sub Regexp::ModuleA::test_target6 {
  sub {
    my($mods,$re)=@_;
    my $o = Regexp::ModuleA::P6->new_rx_from_re($re,$mods);
    sub{my($s)=@_;$o->match($s)}
  };
}

if($0 eq __FILE__ && @ARGV) {
  if($ARGV[0] eq '--test') {
    require './t/re_tests.pl';
    Pkg_re_tests::test(&Regexp::ModuleA::test_target);
    exit;
  }
  if($ARGV[0] eq '--test6') {
    require './t/rx.pl';
    Pkg_re_tests::test6(&Regexp::ModuleA::test_target6);
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
