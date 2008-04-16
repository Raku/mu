# Backtracking api changes from a Filter::Simple to a s/// method.
# Engine core becomes a prelude.
# Emitters become p6, emitting p5 source.
# IR analysis becomes p6.
# The regexp and regex parsers are temporarily retained, as some of the action logic will need to end up in IRx1_FromAST or elsewhere.
# And the unicode rules will need to be metaprogrammed in p6.
# Discarded:
#  Match - no need for painful 'use overload' games.
#  Functional regex representations - we're bootstrapping on STD.
#  Api and Filter packages - emitter has understanding and control of the code.
#  Interactive shell and command line - though may again need something like it for testing.

class EmitRegex {
  method expand_backtrack_macros ($code) {
    Needs to be turned into p6...
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
  sub filter_string {
    my($s)=@_;
    local $_ = $s;
    s/\bLET\(([^\)]+)\)\{/_let_gen($1)/eg;
    s/\}LET;/_let_end().";"/eg;
    s/\bFAIL_IF_FAILED\(([^\)]+)\);/return($1) if FAILED($1);/g;
    s/\bFAIL\(\)/return(undef)/g;
    s/\bFAIL_SEQUENCE\(\)/die("fail sequence\\n")/g;
    s/\bFAIL_GROUP\(\)/die("fail group\\n")/g;
    s/\bFAIL_REGEX\(\)/die("fail regex\\n")/g;
    s/\bFAIL_MATCH\(\)/die("fail match\\n")/g;
    s/\bFAILED\(([^\)]+)\)/(!defined($1)||(!ref($1)&&($1<=0)))/g;
    s/\bTAILCALL\(([^,\)]+),?([^\)]*)\);/\@_=($2);goto \&$1;/g;
    $_;
  }
  use Filter::Simple sub {
    $_ = filter_string($_);
    #print STDERR $_;
    $_;
  };
  1;
}

  };

  method regex_prelude {
    self.expand_backtrack_macros '
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
use strict;
use warnings;

local $Regexp::ModuleA::ReentrantEngine::Env::str;
local $Regexp::ModuleA::ReentrantEngine::Env::pos;
local $Regexp::ModuleA::ReentrantEngine::Env::current_match;
local $Regexp::ModuleA::ReentrantEngine::Env::leaf_match;
local $Regexp::ModuleA::ReentrantEngine::Env::pkg;
local $Regexp::ModuleA::ReentrantEngine::Env::nested_data;
local $Regexp::ModuleA::ReentrantEngine::Env::alias_match;
#local $Regexp::ModuleA::ReentrantEngine::Env::stop;

{
  package Regexp::ModuleA::AST::BaseClass;

  use Sub::Name;
  our $sub_id = 1;

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
  sub RMARE_is_noop {
    my($o,$c)=@_;
    return 1 if !defined($c) || $c eq $noop;
    return 0;
  }

  sub RMARE_eat_backref {
    my($o,$idx,$mod5_re)=@_;
    my $noop = $o->RMARE_noop;
    subname "<eat_backref ".($sub_id++).">" => sub {
      my $c = $_[0];
      my $a = $$Regexp::ModuleA::ReentrantEngine::Env::leaf_match->{match_array};
      FAIL() if $idx >= @$a;
      my $m = $a->[$idx];
      $m = $m->[-1] if defined($m) && ref($m) eq "ARRAY";
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
  { use re "eval";
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
    $mod .= "m" if $o->{flags}{perl5_m};
    $mod .= "s" if $o->{flags}{perl5_s};
    $mod .= "x" if $o->{flags}{perl5_x};
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
    die "bug $aref" if ref($aref) ne "ARRAY";
    my @fs = @$aref;
    subname "<alt ".($sub_id++).">" => sub {
      my $c = $_[0];
      for my $f (@fs) {
        my $v = LET($Regexp::ModuleA::ReentrantEngine::Env::pos){
          my $v1 = eval { $f->($c) }; #try
          if($@) {
            next if $@ eq "fail sequence\n";
            die $@ unless $@ eq "fail group\n";
            FAIL();
          }
          $v1;
        }LET;
        return $v if not FAILED($v);
      }
      FAIL();
    };
  }
  sub RMARE_conj {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne "ARRAY";
    my @fs = @$aref;
    my $noop = $o->RMARE_noop;
    return $noop if @fs == 0;
    return $fs[0] if @fs == 1;
    my $code1 = "()"; my $code2 = "";
    my $code0 = "my \$f0 = \$fs[0]; ";
    { my $i = $#fs;
      $code0 .= "";
      $code1 = \'sub {
  FAIL() if $__end__ != $Regexp::ModuleA::ReentrantEngine::Env::pos;
  @_=\'.$code1;
      $code2 .= ";\ngoto \&\$cn}";
    }
    for my $i (reverse(2..$#fs)) {
      $code0 .= "my \$f$i = \$fs[$i]; ";
      $code1 = \'sub {
  FAIL() if $__end__ != $Regexp::ModuleA::ReentrantEngine::Env::pos;
  $Regexp::ModuleA::ReentrantEngine::Env::pos = $__start__;
  @_=\'.$code1;
      $code2 .= ";\ngoto \&\$f$i}";
    }
    { my $i = 1;
      $code0 .= "my \$f$i = \$fs[$i]; ";
      $code1 = \'sub {
  $__end__ = $Regexp::ModuleA::ReentrantEngine::Env::pos;
  $Regexp::ModuleA::ReentrantEngine::Env::pos = $__start__;
  @_=\'.$code1;
      $code2 .= ";\ngoto \&\$f$i}";
    }
    my $code = $code0."
#line 2 \"Regexp::ModuleA::AST::BaseClass RMARE_conj\"
\n subname \'<conj \'.(\$sub_id++).">" => sub {my \$cn = \$_[0];
  my \$__start__ = \$Regexp::ModuleA::ReentrantEngine::Env::pos;
  my \$__end__ = undef;
  my \$__f__ = ".$code1.$code2.\';
    LET($Regexp::ModuleA::ReentrantEngine::Env::pos){
      $f0->($__f__);
    }LET;
  \'."}\n";
    #print STDERR $code;
    # Currently expanded in the string itself. :/
    # $code = Regexp::ModuleA::ReentrantEngine::BacktrackMacros::filter_string($code);
    eval($code) || die "$@";
  }   
  sub RMARE_concat {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne "ARRAY";
    my @fs = @$aref;
    return $o->RMARE_noop if @fs == 0;
    return $fs[0] if @fs == 1;
    my $code1 = ""; my $code2 = "";
    my $code0 = "my \$f0 = \$fs[0]; ";
    for my $i (reverse(1..$#fs)) {
      $code0 .= "my \$f$i = \$fs[$i]; ";
      $code1 .= "sub {\@_=";
      $code2 .= ";goto \&\$f$i}";
    }
    my $code = $code0."
#line 2 \"Regexp::ModuleA::AST::BaseClass RMARE_concat\"
\n subname \'<concat \'.(\$sub_id++).\'>\' => sub {my \$cn = \$_[0]; \@_=".$code1."\$cn".$code2.";goto \&\$f0}\n";
    eval($code) || die "$@";
  }   

  my $repeat_id = 1;
  our(%repeat_count,%repeat_previous_pos);
  local %repeat_count;
  local %repeat_previous_pos;
  sub RMARE_repeat {
    my($o,$f,$min,$max,$ng)=@_;
    my $greedy = !$ng ? 1 : 0;
    my $noop = $o->RMARE_noop;
    my $myid = $sub_id++;
    subname "<repeat ".($myid).">" => sub {
      if(!defined $noop){die "this perl v5.8.8 bug workaround line didnt work"}
      my $c = $_[0];
      my $rid = $repeat_id++;
      local $repeat_previous_pos{$rid} = -1;
      local $repeat_count{$rid} = 0;
      my($get_minimum,$try_getting_more);
      $get_minimum = subname "get_minimum" => sub {
        if($repeat_count{$rid} < $min) {
          local $repeat_count{$rid} = $repeat_count{$rid} +1;
          $f->($get_minimum);
        } else {
          goto &$try_getting_more;
        }
      };
      $try_getting_more = subname "try_getting_more" => sub {
        if( !($repeat_previous_pos{$rid} < $Regexp::ModuleA::ReentrantEngine::Env::pos) ||
            !($repeat_count{$rid} < $max))
        {
          TAILCALL($c,$noop);
        }
        local $repeat_previous_pos{$rid} = $Regexp::ModuleA::ReentrantEngine::Env::pos;
        local $repeat_count{$rid} = $repeat_count{$rid} +1;
        
        my $v = LET($Regexp::ModuleA::ReentrantEngine::Env::pos){
          $greedy ? $f->($try_getting_more) : $c->($noop);
        }LET;
        return $v if not FAILED($v);
        if($greedy){
          TAILCALL($c,$noop); # tailcall ok despite locals.
        } else {
          $f->($try_getting_more);
        }
      };
      $get_minimum->();
    };
  }
  sub RMARE_group {
    my($o,$f,$target_spec,$in_quant)=@_;
    my $foo = subname "<group ".($sub_id++).">" => sub {
      my $cn = $_[0];
      my $nd = $Regexp::ModuleA::ReentrantEngine::Env::nested_data;
      my $close = sub {
        my($c)=@_;
        $Regexp::ModuleA::ReentrantEngine::Env::nested_data = $nd;
        my $v = eval { $cn->($c) };
        if($@) {
          die "jump ".$@ if $@ =~ /^fail /;
          die $@;
        }
        return $v;
      };
      my $v = eval {$f->($close)}; #try
      if($@) {
        die $1 if $@ =~ /^jump (.+)/s;
        die $@ unless $@ eq "fail group\n" || $@ eq "fail sequence\n";
        FAIL();
      }
      return $v;
    };
    return $foo if !$target_spec;
    return $foo if ($target_spec->[0] =~ /^\$/) && $in_quant;
    my $cs = $o->RMARE_capture_string($foo);
    $o->RMARE_alias_wrap($cs,undef,1,0,$in_quant,$target_spec);
  }
  sub RMARE_capture_string {
    my($o,$f)=@_;
    my $myid = $sub_id++;
    subname \'<capture_string \'.($myid).">" => sub {
      my($c)=@_;

      my $m = $Regexp::ModuleA::ReentrantEngine::Env::alias_match;
      my $from = $Regexp::ModuleA::ReentrantEngine::Env::pos;

      my $close = subname \'<capture_string-close \'.($myid).">" => sub {
        my $c0 = $_[0];
        my $to = $Regexp::ModuleA::ReentrantEngine::Env::pos;
        $m->match_set(1,substr($Regexp::ModuleA::ReentrantEngine::Env::str,$from,$to-$from),$$m->{match_array},$$m->{match_hash},$from,$to);
        TAILCALL($c0,$c);
      };

      local $Regexp::ModuleA::ReentrantEngine::Env::alias_match = undef;

      $f->($close);
    };
  }
  sub RMARE_capture {
    my($o,$idx,$f,$is6,$nparen6,$in_quant,$target_spec)=@_;
    my $myid = $sub_id++;
    my $foo = subname \'<capture \'.($myid).">" => sub {
      my($c)=@_;

      my $m = $Regexp::ModuleA::ReentrantEngine::Env::alias_match;
      my $from = $Regexp::ModuleA::ReentrantEngine::Env::pos;
      my $nd = $Regexp::ModuleA::ReentrantEngine::Env::nested_data;
      my $leaf = $Regexp::ModuleA::ReentrantEngine::Env::leaf_match;

      my $close = subname \'<capture-close \'.($myid).">" => sub {
        my $c0 = $_[0];
        $Regexp::ModuleA::ReentrantEngine::Env::nested_data = $nd;
        $Regexp::ModuleA::ReentrantEngine::Env::leaf_match = $leaf if $is6;
        my $to = $Regexp::ModuleA::ReentrantEngine::Env::pos;
        $m->match_set(1,substr($Regexp::ModuleA::ReentrantEngine::Env::str,$from,$to-$from),$$m->{match_array},$$m->{match_hash},$from,$to);
        my $v = eval { $c0->($c) };
        if($@) {
          die "jump ".$@ if $@ =~ /^fail /;
          die $@;
        }
        return $v;
      };

      local $Regexp::ModuleA::ReentrantEngine::Env::alias_match;
      local $Regexp::ModuleA::ReentrantEngine::Env::leaf_match = $is6 ? $m : $leaf;

      my $v = eval { $f->($close) }; #try
      if($@) {
        die $1 if $@ =~ /^jump (.+)/s;
        die $@ unless $@ eq "fail group\n" || $@ eq "fail sequence\n";
        $m->match_set_as_failed;
        FAIL();
      }
      $m->match_set_as_failed if FAILED($v);
      $v;
    };
    $o->RMARE_alias_wrap($foo,$idx,$is6,$nparen6,$in_quant,$target_spec);
  }
  sub RMARE_alias_wrap {
    my($o,$f,$idx,$is6,$nparen6,$in_quant,$target_spec)=@_;
    my $myid = $sub_id++;
    my $spec = $target_spec ? [@$target_spec] : [\'$/\',\'[\'=>$idx];
    my $root = shift(@$spec);
    my $top = \'$$Regexp::ModuleA::ReentrantEngine::Env::leaf_match\';
    my($copy,$access);
    my $localize = $top;
    for(my $i=0;$i<@$spec;$i+=2){
      my($flag,$key)=($spec->[$i],$spec->[$i+1]);
      my $is_final = $i == (@$spec - 2);
      if($flag eq \'[\'){
        $localize .= \'->{match_array}\';
        $localize .= "[$key]" if !$is_final;
        if($is_final){
          $copy = \'[@{\'.$localize.\'}]\';
          $access = "[$key]";
        }
      } elsif($flag eq \'{\'){
        $localize .= \'->{match_hash}\';
        $localize .= "{$key}" if !$is_final;
        if($is_final){
          $copy = \'{%{\'.$localize.\'}}\';
          $access = "{$key}";
        }
      } else { die "bug" };
    }
    my $array_alias = $root =~ /^\@/;
    my $code = \'
sub {
  my($c)=@_;
  my $m = $Regexp::ModuleA::ReentrantEngine::Env::alias_match;
  if(1 || !defined($m)){#XXXXX
    $m = Regexp::ModuleA::ReentrantEngine::Match0->new_failed();
    if($is6) {
      my $a = [map{Regexp::ModuleA::ReentrantEngine::Match0->new_failed()} (1..$nparen6)];
      $$m->{match_array} = $a;
    }
  }
  return LET(\'.$localize.\'){
    my $newa = \'.$copy.\';
    \'.$localize.\' = $newa;
    if(\'.($is6 && $in_quant ? 1 : 0).\') {
      my $onto = $newa->\'.$access.\';
      $onto = [] if ref($onto) ne \'ARRAY\';
      $onto = [@$onto,($array_alias ? @{$$m->{match_array}} : $m)];
      $newa->\'.$access.\' = $onto;
    } else {
      $newa->\'.$access.\' = (\'.($array_alias?1:0).\' ? [$m] : $m);
    }
    local $Regexp::ModuleA::ReentrantEngine::Env::alias_match = $m;
    $f->($c);
  }LET;
}\';
#print STDERR $code;
    my $capf = subname "<alias_wrap ".($myid).">" => eval($code);
    die "bug $@" if $@;
    $capf;
  }
  sub RMARE_subrule {
    my($o,$fetch,$pkg,$pkg_override,$name,$args,$neg,$nocap,$in_quant,$target_spec)=@_;
    my $noop = $o->RMARE_noop;
    my $myid = $sub_id++;
    my $f1 = subname "<subrule ".($myid)." $name>" => sub {
      my($c)=@_;
      my $f = $fetch->(@$args);

      my $pkg0 = $Regexp::ModuleA::ReentrantEngine::Env::pkg;
      my $pkg2 = $pkg_override || $pkg0;
      my $pkg9 = $pkg_override || $Regexp::ModuleA::ReentrantEngine::Env::pkg || $pkg;

      my $pos = $Regexp::ModuleA::ReentrantEngine::Env::pos;
      my $m0 = $Regexp::ModuleA::ReentrantEngine::Env::current_match;
      my $m0b = $Regexp::ModuleA::ReentrantEngine::Env::leaf_match;

      my $nd = $Regexp::ModuleA::ReentrantEngine::Env::nested_data;

      my $m1 = $Regexp::ModuleA::ReentrantEngine::Env::alias_match;
      if(defined($m1)) {
      } else {
        $m1 = Regexp::ModuleA::ReentrantEngine::Match0->new_failed;
      }
      $m1->match_set(1,"",[],{},$pos,undef);
      $$m1->{\'RULE\'} ||= $name; #EEEP

      my $close = subname "<subrule-close ".($myid)." $name>" => sub {
	my $cn = $_[0];

        $Regexp::ModuleA::ReentrantEngine::Env::nested_data = $nd;

	$$m1->{\'match_to\'} = $Regexp::ModuleA::ReentrantEngine::Env::pos; #EEEP
	$$m1->{\'match_string\'} = substr($Regexp::ModuleA::ReentrantEngine::Env::str,$pos,$Regexp::ModuleA::ReentrantEngine::Env::pos-$pos);

        my $post = $name."__post_action";
        if(UNIVERSAL::can($pkg9,$post)) {
          $m1->_match_enable_overload1;
          $pkg9->$post($m1);
        }

	$Regexp::ModuleA::ReentrantEngine::Env::current_match = $m0;
	$Regexp::ModuleA::ReentrantEngine::Env::leaf_match = $m0b;
	local $Regexp::ModuleA::ReentrantEngine::Env::pkg = $pkg0;

# =pod
#         if(!$nocap) {
#           LET($$m0->{\'match_hash\'}{$name}){
#             if($in_quant) {
#               $$m0->{\'match_hash\'}{$name} = [@{$$m0->{\'match_hash\'}{$name}||[]}];
#               push(@{$$m0->{\'match_hash\'}{$name}},$m1);
#             } else {
#               $$m0->{\'match_hash\'}{$name} = $m1;
#             }
#             $neg ? 1 : $cn->($c);
#           }LET;
#         } else {
#             $neg ? 1 : $cn->($c);
#         }
# =cut
            $neg ? 1 : $cn->($c);
      };

      my $v;
      { local $Regexp::ModuleA::ReentrantEngine::Env::current_match = $m1;
        local $Regexp::ModuleA::ReentrantEngine::Env::leaf_match = $m1;
        local $Regexp::ModuleA::ReentrantEngine::Env::pkg = $pkg2;
        local $Regexp::ModuleA::ReentrantEngine::Env::nested_data->{args} = $args;
	$v = eval { $f->($close) };
        if($@) {
          die $@ unless $@ eq "fail regex\n";
          FAIL() if !$neg;
          $v = undef; # FAILED #X
        }
      }
      if($neg) {
        if(FAILED($v)) {
          $$m1->{\'match_to\'} = $$m1->{\'match_from\'};
          $$m1->{\'match_string\'} = "";

# =pod
#           LET($$m0->{\'match_hash\'}{$name}){
#             $$m0->{\'match_hash\'}{$name} = [@{$$m0->{\'match_hash\'}{$name}||[]}];
#             push(@{$$m0->{\'match_hash\'}{$name}},$m1);
#             $c->($noop);
#           }LET;
# =cut
            $c->($noop);

        } else {
          FAIL();
        }
      } else {
        FAIL_IF_FAILED($v);
        return $v;
      }
    };
    return $f1 if $nocap;
    $target_spec ||= [\'$/\',\'{\'=>$name];
    $o->RMARE_alias_wrap($f1,undef,1,0,$in_quant,$target_spec);
  }
  sub RMARE_aregex {
    my($o,$f)=@_;
    my $nparenx = $o->{flags}{p5} ? $o->{nparen} : $o->{nparen6};
    $nparenx = 0 if !defined $nparenx; #XXX arguments to subrules.  aregex not seeing an init.
    subname "<aregex ".($sub_id++).">" => sub {
      my($c)=@_;

      my $m = $Regexp::ModuleA::ReentrantEngine::Env::leaf_match;
      my $a = [map{Regexp::ModuleA::ReentrantEngine::Match0->new_failed()} (1..$nparenx)];
      $$m->{match_array} = $a;

      my $v = eval { $f->($c) }; #try
      if($@) {
        die $@ unless ($@ eq "fail group\n" ||
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
      my $m = Regexp::ModuleA::ReentrantEngine::Match0->new_failed();
      local $Regexp::ModuleA::ReentrantEngine::Env::current_match = $m;
      local $Regexp::ModuleA::ReentrantEngine::Env::leaf_match = $m;
      local $Regexp::ModuleA::ReentrantEngine::Env::nested_data = {};
      local $Regexp::ModuleA::ReentrantEngine::Env::alias_match;
      $Regexp::ModuleA::ReentrantEngine::Env::nested_data->{args} = [];
      
      my $ok = eval { $f->($atend) }; #try
      if($@) {
        die $@ unless ($@ eq "fail match\n" || $@ eq "fail regex\n" ||
                       $@ eq "fail group\n" || $@ eq "fail sequence\n");
        last;
      }
      if(not FAILED($ok)) {
        $m->match_set(1,substr($Regexp::ModuleA::ReentrantEngine::Env::str,$start,$Regexp::ModuleA::ReentrantEngine::Env::pos-$start),$$m->{match_array},$$m->{\'match_hash\'},$start,$Regexp::ModuleA::ReentrantEngine::Env::pos);
        return $m;
      }
    }
    return Regexp::ModuleA::ReentrantEngine::Match0->new_failed();
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
'
  };

  #======================================================================
  # Emitters
  # 

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
    my $f1 = $o->RMARE_repeat($f,$min,$max,$nongreedy);
    if($o->{flags}{ratchet}) {
      $o->RMARE_concat([$f1,$o->RMARE_commit_sequence()]);
    } else {
      $f1;
    }
  }

  # a|b
  package Regexp::ModuleA::AST::Alt;
  sub RMARE_emit {
    my($o)=@_;
    my $f1 = $o->RMARE_alt([map{$_->RMARE_emit}@{$o->{exprs}}]);
    if($o->{flags}{ratchet}) {
      $o->RMARE_concat([$f1,$o->RMARE_commit_sequence()]);
    } else {
      $f1;
    }
  }
  
  # a&b
  package Regexp::ModuleA::AST::Conj;
  sub RMARE_emit {
    my($o)=@_;
    $o->RMARE_conj([map{$_->RMARE_emit}@{$o->{exprs}}]);
  }
  
  # ab
  package Regexp::ModuleA::AST::Seq;
  sub RMARE_emit {
    my($o)=@_;
    $o->RMARE_concat([map{$_->RMARE_emit}@{$o->{exprs}}]);
  }
  
  # .. := ...
  package Regexp::ModuleA::AST::Alias;
  sub RMARE_emit {
    my($o)=@_;
    my $target_spec = $o->{target_spec};
    my $construct_kind = $o->{construct_kind};
    my $construct_in_quant = $o->{construct_in_quant};
    my $f = $o->{expr}->RMARE_emit;
    if($construct_kind eq 'group'
       && $construct_in_quant
       && $target_spec->[0] =~ /^\$/)
    {
      my $cs = $o->RMARE_capture_string($f);
      $o->RMARE_alias_wrap($cs,undef,1,0,0,$target_spec);
    }
    else {
      $f;
    }
  }

  # (?:a)
  package Regexp::ModuleA::AST::Grp;
  sub RMARE_emit {
    my($o)=@_;
    my $target_spec = $o->{target_spec};
    my $in_quant = $o->{in_quant};
    $o->RMARE_group($o->{expr}->RMARE_emit,$target_spec,$in_quant);
  }
  
  # (a)
  package Regexp::ModuleA::AST::Cap;
  sub RMARE_emit {
    my($o)=@_;
    my $in_quant = $o->{in_quant} ? 1 : 0;
    my $target_spec = $o->{target_spec};
    my $is6 = !$o->{flags}{'p5'};
    my $idx = ($is6
               ? $o->{cap6_idx}
               : $o->{cap5_idx});
    my $f = $o->{expr}->RMARE_emit;
    $o->RMARE_capture($idx,$f,$is6,$o->{nparen6},
                      $in_quant,$target_spec);
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
    my $exprs = $o->{exprs};
    my $pkg = $o->{pkg};
    my $name = $o->{name};
    my $neg = $o->{neg};
    my $nocap = $o->{nocap};
    my $in_quant = $o->{in_quant} ? 1 : 0;
    my $pkg_override = undef;
    if($name =~ /^([\w\:\.]+)\.(\w+)$/){
      $name = $2;
      $pkg_override = $1;
    }
    my $fetch = subname "<subrule-fetch for $name>" => sub {
      my $pkg9 = $pkg_override || $Regexp::ModuleA::ReentrantEngine::Env::pkg || $pkg;
      die "assert" if !defined $pkg9;
      no strict;
      my $f;
      eval { $f = $pkg9->$name($name)->(' api0'); };
      Carp::confess $@ if $@;
      die if $@;
      use strict;
      die "assert" if !defined $f;
      $f;
    };
    my $target_spec = $o->{target_spec};
    $o->RMARE_subrule($fetch,$pkg,$pkg_override,$name,[map{$_->RMARE_emit} @$exprs],$neg,$nocap,$in_quant,$target_spec);
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
      $m->_match_enable_overload2;
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
  package Regexp::ModuleA::AST::Biind;
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
  $M->_match_enable_overload1;').'
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
  $M->_match_enable_overload1;').'
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
                  ? $o->{expr_else}->RMARE_emit
                  : sub{my $c = $_[0]; TAILCALL($c,$noop);});
    if($o->{test} !~ /\A\d+\z/) {
      $f_test = $o->{test}->RMARE_emit;
    } else {
      my $idx = $o->{test} +0;
      $f_test = sub {
        my $c = $_[0];
        my $a = $Regexp::ModuleA::ReentrantEngine::Env::current_match->match_array;
        FAIL() if $idx > @$a;
        my $m = $a->[$idx-1];
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
    my $is_forward = $o->{is_forward};
    my $is_positive = $o->{is_positive};
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

};


#======================================================================
# IR Analysis
# 
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
  sub RAST_pass10 { # flags, mods, and pkg
    my($o)=@_;
    $o->{flags} = {%$Regexp::ModuleA::AST::Env::flags};
    shift->RAST_tell_children('RAST_pass10');
  }
  sub RAST_pass15 { # nparen, target_spec (req: RAST_pass14)
    shift->RAST_tell_children('RAST_pass15');
  }
  sub RAST_pass14 { # in_quant, subrules_seen, alias_construct
    my($o)=@_;
    my $q = $Regexp::ModuleA::AST::Env::in_quant;
    $q = 1 if $q; # remove 'directly'
    local $Regexp::ModuleA::AST::Env::in_quant = $q;
    $o->RAST_tell_children('RAST_pass14');
  }
  sub RAST_pass30 { shift->RAST_tell_children('RAST_pass30') }

  sub RAST_to_make0 {
    my($o)=@_;
    my($cls) = ref($o) =~ /([^:]+)$/;
    my $name = lc $cls;
    $name.'('.$o->RAST_to_make0_children.')';
  }
  sub RAST_to_make0_children {
    my($o)=@_;
    my $args = $o->RAST_tell_children('RAST_to_make0');
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
  sub RAST_to_make0 {my($o)=@_; 'pat5('.($o->RAST_quote($o->{pat})).')';}

  # AST::Exact
  package Regexp::ModuleA::AST::Exact;
  @Regexp::ModuleA::AST::Exact::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$text)=@_; die "api assert" if @_ != 2;
    bless {text=>$text}, $cls;
  }
  sub RAST_to_make0 {my($o)=@_; 'exact('.($o->RAST_quote($o->{text})).')';}

  # AST::MixinMod
  package Regexp::ModuleA::AST::MixinMod;
  sub mods_from_modpat {
    my($cls,$modpat)=@_;
    my %normalize = (
                     perl5_i => 'i',
                     ignorecase => 'i',
                     s => 'sigspace'
                     );
    my %m;
    for my $mod (split(":",$modpat)) {
      next if $mod eq '';
      $mod =~ /\A(\w+)(?:[[(<](.*?)[])>])?\z/ or die "assert";
      my($k,$v) = ($1,$2);
      $v = '1' if !defined $v;
      $v = eval($v);#X
      $k = $normalize{$k} || $k;
      $m{$k} = $v;
    }
    \%m;
  }
  sub _add_mods {
    my($o)=@_;
    my $flags = {%$Regexp::ModuleA::AST::Env::flags};
    foreach my $key (keys(%{$o->{mods}})) {
      $flags->{$key} = $o->{mods}{$key};
    }
    $flags;
  }
  sub _RAST_to_make0_hlp {
    my($o)=@_;
    my $modpat = join("",map{
      my $k = $_;
      my $v = $o->{mods}{$k};
      my $vs = $v eq '1' ? "" : "<$v>";
      ":$k$vs"
      } keys(%{$o->{mods}}));
    $o->RAST_quote($modpat);
  }

  # AST::Mod_expr
  package Regexp::ModuleA::AST::Mod_expr;
  @Regexp::ModuleA::AST::Mod_expr::ISA=qw(Regexp::ModuleA::AST::MixinMod Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$modpat,$expr)=@_; die "api assert" if @_ != 3;
    my $mods = $cls->mods_from_modpat($modpat);
    bless {mods=>$mods,expr=>$expr}, $cls;
  }
  sub RAST_pass10 {
    my($o)=@_;
    local $Regexp::ModuleA::AST::Env::flags = $o->_add_mods;
    $o->SUPER::RAST_pass10;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    'mod_expr('.$o->_RAST_to_make0_hlp.",".$o->RAST_to_make0_children.')';
  }
  
  # AST::Mod_inline
  package Regexp::ModuleA::AST::Mod_inline;
  @Regexp::ModuleA::AST::Mod_inline::ISA=qw(Regexp::ModuleA::AST::MixinMod Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$modpat)=@_; die "api assert" if @_ != 2;
    my $mods = $cls->mods_from_modpat($modpat);
    bless {mods=>$mods}, $cls;
  }
  sub RAST_pass10 { # Not the same as Mod_expr's.
    my($o)=@_;
    $Regexp::ModuleA::AST::Env::flags = $o->_add_mods;
    $o->SUPER::RAST_pass10;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    'mod_inline('.$o->_RAST_to_make0_hlp.')';
  }

  # AST::Backref
  package Regexp::ModuleA::AST::Backref;
  @Regexp::ModuleA::AST::Backref::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$idx)=@_; die "api assert" if @_ != 2;
    bless {backref_n=>$idx}, $cls;
  }
  sub RAST_pass30 {
    my($o)=@_;
    my $n = $o->{backref_n};
    my $total = $Regexp::ModuleA::AST::Env::nparen;
    die "Backreference to nonexistent group $n of $total"
      if $total < $n;
  }
  sub RAST_to_make0 {my($o)=@_; 'backref('.$o->{backref_n}.')';}

  # AST::Cap
  package Regexp::ModuleA::AST::Cap;
  @Regexp::ModuleA::AST::Cap::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$expr)=@_;
    bless {expr=>$expr}, $cls;
  }
  sub RAST_pass10 {
    my($o)=@_;
    $o->{flags} = {%$Regexp::ModuleA::AST::Env::flags};
    local $Regexp::ModuleA::AST::Env::flags = {%$Regexp::ModuleA::AST::Env::flags};
    $o->SUPER::RAST_pass10;
  }
  sub RAST_pass15 {
    my($o)=@_;
    $o->{cap6_idx} = $Regexp::ModuleA::AST::Env::nparen6_idx++;
    $o->{cap5_idx} = $Regexp::ModuleA::AST::Env::nparen++;
    $Regexp::ModuleA::AST::Env::nparen6 = $Regexp::ModuleA::AST::Env::nparen6_idx
      if $Regexp::ModuleA::AST::Env::nparen6 < $Regexp::ModuleA::AST::Env::nparen6_idx;
    $o->{target_spec} = $Regexp::ModuleA::AST::Env::target_spec;

    local $Regexp::ModuleA::AST::Env::nparen6 = 0;
    local $Regexp::ModuleA::AST::Env::nparen6_idx = 0;
    local $Regexp::ModuleA::AST::Env::target_spec = undef;
    $o->{expr}->RAST_pass15;
    $o->{nparen6} = $Regexp::ModuleA::AST::Env::nparen6;
  }
  sub RAST_pass14 {
    my($o)=@_;
    $o->{in_quant} = $Regexp::ModuleA::AST::Env::in_quant;
    local $Regexp::ModuleA::AST::Env::in_quant = 0;
    local $Regexp::ModuleA::AST::Env::subrules_seen = {};
    $o->SUPER::RAST_pass14;
    $Regexp::ModuleA::AST::Env::alias_construct = $o;
  }

  # AST::Grp
  package Regexp::ModuleA::AST::Grp;
  @Regexp::ModuleA::AST::Grp::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$expr)=@_;
    bless {expr=>$expr}, $cls;
  }
  sub RAST_pass10 {
    my($o)=@_;
    $o->{flags} = {%$Regexp::ModuleA::AST::Env::flags};
    local $Regexp::ModuleA::AST::Env::flags = {%$Regexp::ModuleA::AST::Env::flags};
    $o->SUPER::RAST_pass10;
  }
  sub RAST_pass15 {
    my($o)=@_;
    $o->{target_spec} = $Regexp::ModuleA::AST::Env::target_spec;
    local $Regexp::ModuleA::AST::Env::target_spec = undef;
    $o->{expr}->RAST_pass15;
  }
  sub RAST_pass14 {
    my($o)=@_;
    $o->{in_quant} = $Regexp::ModuleA::AST::Env::in_quant;
    $o->SUPER::RAST_pass14;
    $Regexp::ModuleA::AST::Env::alias_construct = $o;
  }

  # AST::Alias
  package Regexp::ModuleA::AST::Alias;
  @Regexp::ModuleA::AST::Alias::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$target,$expr)=@_;
    $target =~ /^([\$\@\%](?:[[:alpha:]_][\w:]+|\/)?)(.*)$/ or die "bug";
    my($root,$rest)=($1,$2);
    my @parts;
    if($rest =~ /^(\d+)(.*)/) {
      push(@parts,'['=>$1);
      $rest = $2;
    }
    while($rest ne ""){
      $rest =~ /^((<)(\w+)>|(\[)(\d+)\])(.*)/ or die "bug";
      my $key = $3 || $5;
      my $kind = $2 || $4;
      $kind = {'['=>'[','{'=>'{','<'=>'{'}->{$kind};
      push(@parts,$kind=>$key);
      $rest = $6;
    }
    $root .= '/' if length($root) == 1;
    unshift(@parts,$root);
    my $target_spec = \@parts;
    bless {target=>$target,target_spec=>$target_spec,expr=>$expr}, $cls;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    'alias('.$o->RAST_quote($o->{target}).','.$o->RAST_to_make0_children.')';
  }
  sub RAST_pass15 {
    my($o)=@_;
    $o->{first_alias} = !defined($Regexp::ModuleA::AST::Env::target_spec);
    my $spec = $o->{target_spec};
    my $idx = (@$spec == 3 && $spec->[1] eq '[') ? $spec->[2] : undef;
    if(defined($idx)) {
      my $construct_kind = $o->{construct_kind};
      my $construct_in_quant = $o->{construct_in_quant};
      my $next_idx = $idx;
      $next_idx++ if $construct_kind eq 'group';
      $Regexp::ModuleA::AST::Env::nparen6_idx = $next_idx;
      $Regexp::ModuleA::AST::Env::nparen6 = $next_idx if
        $Regexp::ModuleA::AST::Env::nparen6 < $next_idx;
    }
    local $Regexp::ModuleA::AST::Env::target_spec = $spec;
    $o->{expr}->RAST_pass15;
  }
  sub RAST_pass14 {
    my($o)=@_;
    $o->RAST_tell_children('RAST_pass14');
    my $construct = $Regexp::ModuleA::AST::Env::alias_construct;
    my $kind = {'Regexp::ModuleA::AST::Grp'=>'group',
                'Regexp::ModuleA::AST::Cap'=>'capture',
                'Regexp::ModuleA::AST::Subrule'=>'subrule'}->{ref($construct)};
    my $in_quant = $construct->{in_quant};
    $o->{construct_kind} = $kind;
    $o->{construct_in_quant} = $in_quant;
  }

  # AST::Quant
  package Regexp::ModuleA::AST::Quant;
  @Regexp::ModuleA::AST::Quant::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$min,$max,$expr,$nongreedy)=@_; die "api assert" if @_ < 4||@_ > 5;
    bless {min=>$min,max=>$max,expr=>$expr,nongreedy=>$nongreedy}, $cls;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    my $min = $o->{min}; $min = 'undef' if !defined $min;
    my $max = $o->{max}; $max = 'undef' if !defined $max;
    my $expr = $o->RAST_to_make0_children;
    my $ng = $o->{nongreedy}; $ng = defined $ng ? ",'ng'" : "";
    'quant('."$min,$max,$expr$ng".')';
  }
  sub RAST_pass14 {
    my($o)=@_;
    local $Regexp::ModuleA::AST::Env::in_quant = 'directly';
    $o->RAST_tell_children('RAST_pass14');
  }

  # AST::Alt
  package Regexp::ModuleA::AST::Alt;
  @Regexp::ModuleA::AST::Alt::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,@exprs)=@_;
    bless {exprs=>\@exprs}, $cls;
  }
  sub RAST_pass15 {
    my($o)=@_;
    my $start = $Regexp::ModuleA::AST::Env::nparen6_idx;
    my $max = $start;
    my $x = [map{
      local $Regexp::ModuleA::AST::Env::nparen6_idx = $start;
      my $x1 = $_->RAST_pass15;
      my $np = $Regexp::ModuleA::AST::Env::nparen6_idx;
      $max = $np if $max < $np;
      $x1;
    } @{$o->{exprs}}];
    $o->{cap6_idx_start} = $start;
    $o->{nparen6} = $max - $start;
    $Regexp::ModuleA::AST::Env::nparen6_idx = $max;
    $x;
  }

  # AST::Conj
  package Regexp::ModuleA::AST::Conj;
  @Regexp::ModuleA::AST::Conj::ISA=qw(Regexp::ModuleA::AST::BaseClass);
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
  sub RAST_pass14 {
    my($o)=@_;
    if(@{$o->{exprs}} == 1) {
      # Single item sequence doesn't affect in_quant directness.
      $o->RAST_tell_children('RAST_pass14');
    } else {
      $o->SUPER::RAST_pass14;
    }
  }

  # AST::ASpace
  package Regexp::ModuleA::AST::ASpace;
  @Regexp::ModuleA::AST::ASpace::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$inpkg,$text)=@_; die "api assert" if @_ != 3;
    bless {aspace_inpkg=>$inpkg,text=>$text}, $cls;
  }
  sub RAST_to_make0 {my($o)=@_; 'aspace('.($o->RAST_quote($o->{text})).')';}
  sub RAST_pass10 {
    my($o)=@_;
    my $flags = {%$Regexp::ModuleA::AST::Env::flags};
    if($flags->{sigspace}) {
      my $sr = Regexp::ModuleA::AST::Subrule->new($o->{aspace_inpkg},'?ws',[]);
      %$o = %$sr;
      bless $o,'Regexp::ModuleA::AST::Subrule';
    } else {
      bless $o,'Regexp::ModuleA::AST::Exact';
    }
    $o->RAST_pass10;
  }

  # AST::Subrule
  package Regexp::ModuleA::AST::Subrule;
  @Regexp::ModuleA::AST::Subrule::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$inpkg,$fullname,$exprs)=@_; die "api assert" if @_ != 4;
    $fullname =~ /^([?!]*)(.*)$/ or die;
    my($prefix,$name) = ($1,$2);
    my $neg = $prefix =~ /\!/;
    my $nocap = $prefix =~ /\?/;
    bless {created_in_pkg=>$inpkg,name=>$name,exprs=>($exprs||[]),neg=>$neg,nocap=>$nocap}, $cls;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    my $exprs = $o->{exprs};
    my $x = defined $exprs ? ",".$o->RAST_quote($exprs) : "";
    'sr('.$o->RAST_quote($o->{name}).$x.')';
  }
  sub RAST_pass10 {
    my($o)=@_;
    $o->{pkg} = $Regexp::ModuleA::AST::Env::pkg || $o->{inpkg};
    $o->SUPER::RAST_pass10;
  }
  sub RAST_pass15 {
    my($o)=@_;
    $o->{target_spec} = $Regexp::ModuleA::AST::Env::target_spec;
  }
  sub RAST_pass14 {
    my($o)=@_;
    $o->{in_quant} = $Regexp::ModuleA::AST::Env::in_quant;
    if(!$o->{nocap}) {
      my $name = $o->{name};
      my $seen = $Regexp::ModuleA::AST::Env::subrules_seen->{$name};
      if($seen) {
        $seen->() if ref($seen);
        $Regexp::ModuleA::AST::Env::subrules_seen->{$name} = 1;
        $o->{in_quant} ||= 1;
      } else {
        $Regexp::ModuleA::AST::Env::subrules_seen->{$name} = sub {
          $o->{in_quant} ||= 1;
        };
      }
    }
    $o->SUPER::RAST_pass14;
    $Regexp::ModuleA::AST::Env::alias_construct = $o;
  }

  # AST::ARegex
  package Regexp::ModuleA::AST::ARegex;
  @Regexp::ModuleA::AST::ARegex::ISA=qw(Regexp::ModuleA::AST::MixinMod Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$modpat,$expr)=@_; die "api assert" if @_ != 3;
    my $mods = $cls->mods_from_modpat($modpat);
    bless {modpat=>$modpat,mods=>$mods,expr=>$expr}, $cls;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    'aregexm('.$o->RAST_quote($o->{modpat}).','.$o->RAST_to_make0_children.')';
  }
  sub RAST_init {
    my($o)=@_;
    $o->{pkg} = $Regexp::ModuleA::AST::Env::pkg || $o->{inpkg};
    $o->{name} = $Regexp::ModuleA::AST::Env::name;
    local $Regexp::ModuleA::AST::Env::pkg = $o->{pkg};
    local $Regexp::ModuleA::AST::Env::flags = {%{$o->{mods}}};
    $o->RAST_pass10;
    local $Regexp::ModuleA::AST::Env::in_quant = 0;
    local $Regexp::ModuleA::AST::Env::subrules_seen = {};
    local $Regexp::ModuleA::AST::Env::alias_construct = undef;
    $o->RAST_pass14;
    local $Regexp::ModuleA::AST::Env::nparen = 0;
    local $Regexp::ModuleA::AST::Env::nparen6 = 0;
    local $Regexp::ModuleA::AST::Env::nparen6_idx = 0;
    local $Regexp::ModuleA::AST::Env::target_spec = undef;
    $o->RAST_pass15;
    $o->{nparen} = $Regexp::ModuleA::AST::Env::nparen;
    $o->{nparen6} = $Regexp::ModuleA::AST::Env::nparen6;
    $o->RAST_pass30;
    $o;
  }

  # AST::Biind
  package Regexp::ModuleA::AST::Biind;
  @Regexp::ModuleA::AST::Biind::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new {
    my($cls,$inpkg,$name,$expr)=@_; die "api assert" if @_ != 4;
    die "api assert $name"  if $name =~ /::/;
    bless {created_in_pkg=>$inpkg,name=>$name,expr=>$expr}, $cls;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    'biind('.$o->RAST_quote($o->{name}).','.$o->RAST_to_make0_children.')';
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
  sub RAST_to_make0 {
    my($o)=@_;
    'namespace('.$o->RAST_quote($o->{nsname}).",\n".$o->RAST_to_make0_children.')';
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
  sub RAST_to_make0 {
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
  sub RAST_to_make0 {
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
  sub RAST_to_make0 {my($o)=@_; 'independent('.$o->RAST_to_make0_children.')';}

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
  sub RAST_to_make0 {
    my($o)=@_;
    my $test = $o->{test};
    my $n = ($test =~ /^\d+$/) ? "$test," : "";
    'conditional('."$n".$o->RAST_to_make0_children.')';
  }

  # AST::Lookaround
  package Regexp::ModuleA::AST::Lookaround;
  @Regexp::ModuleA::AST::Lookaround::ISA=qw(Regexp::ModuleA::AST::BaseClass);
  sub new { #XXX blech api
    my($cls,$is_forward,$is_positive,$expr)=@_; die "api assert" if @_ != 4;
    bless {is_forward=>$is_forward,is_positive=>$is_positive,expr=>$expr}, $cls;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    my $a = $o->{is_forward} ? '1' : '0';
    my $b = $o->{is_positive} ? '1' : '0';
    'lookaround('."$a,$b,".$o->RAST_to_make0_children.')';
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

### Some of the action logic needs to end up in IRx1_FromAST...
#======================================================================
# P5 Regexps
#
package Regexp::ModuleA::P5;
Regexp::ModuleA::AST::Make0->import;
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
# a defining characteristic: ws is fudged, so comments are unsupported.
  namespace(""
            ,biind('regex',aregexm(':p5',sr('pattern')))
            ,biind('pattern',aregex(sr('regex_ordered_disjunction')))
            ,biind('regex_ordered_disjunction',aregex(seq(sr('regex_sequence'),star(exact('|'),sr('regex_sequence')))))
            ,biind('regex_sequence',aregex(star(sr('regex_quantified_atom'))))
            ,biind('regex_quantified_atom',aregex(seq(sr('regex_atom'),ques(pat5('[?*+]\??|{\d+(?:,\d*)?}\??')))))
            ,biind('regex_atom',aregex(alt(sr('_mod_inline'),sr('_mod_expr'),sr('_code'),sr('_coderx'),sr('_independent'),sr('_conditional'),sr('_lookaround'),sr('_cap'),sr('_grp'),sr('_charclass'),sr('_backref_or_char'),sr('_esc'),sr('_nonmeta'),sr('_passthru'),sr('_subrule'))))
            ,biind('_mod_inline',aregex(pat5('\(\?([imsx-]+)\)(?{Regexp::ModuleA::P5::mod_helper($^N)})')))
            ,biind('_mod_expr',aregex(seq(pat5('\(\?([imsx-]+):(?{Regexp::ModuleA::P5::mod_helper($^N)})'),sr('pattern'),exact(')'))))
            ,biind('_grp',aregex(seq(exact('(?:'),sr('pattern'),exact(')'))))
            ,biind('_cap',aregex(seq(pat5('\((?!\?)'),sr('pattern'),exact(')'))))
#            ,biind('_charclass',aregex(pat5('\[\^?\]?([^\]\\\\]|\\\\.)*\]\]?')))#X
            ,biind('_charclass',aregex(seq(pat5('\[\^?[\]\-]?'),sr('_charset_def'),pat5('\-?\]'))))
            ,biind('_charset_def',aregex(pat5('(?>\[:\^?\w+:\]|[^\]\\\\]+|\\\\(?s:.))*')))
            ,biind('_backref_or_char',aregex(pat5('\\\\\d+')))
            ,biind('_esc',aregex(pat5('\\\\[^\d]')))
            ,biind('_nonmeta',aregex(pat5("$nonmeta(?:$nonmeta+(?![?*+{]))?")))
            ,biind('_passthru',aregex(pat5('[$^.]')))
            ,biind('_code',aregex(seq(exact('(?{'),pat5($perlcode),exact('})'))))
            ,biind('_coderx',aregex(seq(exact('(??{'),pat5($perlcode),exact('})'))))
            ,biind('_independent',aregex(seq(exact('(?>'),sr('pattern'),exact(')'))))
            ,biind('_conditional',aregex(seq(pat5('\(\?(?=\()'),alt(pat5('\(\d+\)'),sr('_lookaround')),sr('regex_sequence'),ques(exact('|'),sr('regex_sequence')),exact(')'))))
            ,biind('_lookaround',aregex(seq(pat5('\(\?<?[=!]'),sr('pattern'),exact(')'))))
            ,biind('_subrule',aregex(pat5('(?!)')))
            )->RAST_init->RMARE_emit;
}
Regexp::ModuleA::Api::GatherMethodsA->import('gather_methods');
sub make0_from_match {
  my($cls,$m)=@_;
  my $map = $cls->gather_methods(filter=>qr/^make0_from_node__(.+)$/);
  my $map_code = {map{($_,UNIVERSAL::can($cls,$map->{$_}))} keys %$map};
  local $Regexp::ModuleA::Scratch::make0_from_match::map_code = $map_code;
  $cls->make0_from_node($m);
}
use Carp;
sub make0_from_node {
  my($cls,$m)=@_;
  Carp::confess "make0_from_node called with match: undef" if !defined $m;
  my $r = $$m->{RULE};
  my $map_code = $Regexp::ModuleA::Scratch::make0_from_match::map_code;
  my $meth = $map_code->{$r} || $map_code->{DEFAULT};
  if($meth) {
    my $z = $meth->($cls,$m);
    $z;
  } else {
    die "api assert";
  }
}
sub make0_from_children {
  my($cls,$m)=@_;
  $m->match_x_process_children(sub{$cls->make0_from_node($_[0])});
}
sub make0_from_node__DEFAULT {
  my($cls,$m)=@_;
  my($a,$h) = $cls->make0_from_children($m);
#  my $m1 = $m->match_copy();
#  $$m1->{match_array} = $a;
#  $$m1->{match_hash} = $h;
#  $m1;
  my @v = values(%{$h});
  $v[0];
}

sub make0_from_node___nonmeta {
  my($cls,$m)=@_;
  my $pat = "$m";
  $pat =~ s/\\([\\\'])/\\\\\\$1/g;
  return "exact('$pat')";
}
sub make0_from_node___passthru {
  my($cls,$m)=@_;
  my $pat = "$m";
  $pat =~ s/\\([\\\'])/\\\\\\$1/g;
  return "pat5('$pat')";
}
sub make0_from_node__regex_quantified_atom {
  my($cls,$m)=@_;
  my $s = "$m";
  my $e = $cls->make0_from_node($m->{regex_atom});
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
sub make0_from_node___backref_or_char {
  my($cls,$m)=@_;
  "$m" =~ /\A\\(\d+)\z/ or die "bug";
  my $n = $1;
  if($n !~ /\A0/ && $n < 10) {
    return "backref($n)";
  } else {
    # XXX kludge. Interpretation of \10 is much more complex.
    return "pat5('\\\\$n')";
  }
}
sub make0_from_node___esc {
  my($cls,$m)=@_;
  my $pat = "$m";
  $pat =~ s/\\([\\\'])/\\\\\\$1/g;
  return "pat5('$pat')";
}
sub make0_from_node___charclass {
  my($cls,$m)=@_;
  my $pat = "$m";
  $pat =~ s/\\([\\\'])/\\\\\\$1/g;
  return "pat5('$pat')";
}
sub make0_from_node___grp {
  my($cls,$m)=@_;
  my $e = $cls->make0_from_node($m->{pattern});
  return "grp($e)";
}
sub make0_from_node___cap {
  my($cls,$m)=@_;
  my $e = $cls->make0_from_node($m->{pattern});
  return "cap($e)";
}
sub make0_from_node___mod_expr {
  my($cls,$m)=@_;
  my $e = $cls->make0_from_node($m->{pattern});
  "$m" =~ /\A\(\?([imsx]*)(?:-([imsx]*))?/ or die 'bug';
  my $on  = join("",map{":perl5_${_}"} split("",$1));
  my $off = join("",map{":perl5_${_}<0>"} split("",defined $2 ? $2 : ""));
  return "mod_expr('$on$off',$e)";
}
sub make0_from_node___mod_inline {
  my($cls,$m)=@_;
  "$m" =~ /\A\(\?([imsx]*)(?:-([imsx]*))?/ or die 'bug';
  my $on  = join("",map{":perl5_${_}"} split("",$1));
  my $off = join("",map{":perl5_${_}<0>"} split("",defined $2 ? $2 : ""));
  return "mod_inline('$on$off')";
}
sub make0_from_node__regex_sequence {
  my($cls,$m)=@_;
  my @v = map{$cls->make0_from_node($_)} @{$m->{regex_quantified_atom}};
  return (@v != 1 ? ("seq(".join(",",@v).")") : $v[0]);
}
sub make0_from_node__regex_ordered_disjunction {
  my($cls,$m)=@_;
  my @v = map{$cls->make0_from_node($_)} @{$m->{regex_sequence}};
  return (@v > 1 ? ("alt(".join(",",@v).")") : $v[0]);
}
sub make0_from_node___coderx {
  my($cls,$m)=@_;
  "$m" =~ /\A\((\?\??){(.*?)}\)\z/ or die "bug";
  my($which,$code) = ($1,$2);
  return "coderx(q{$code})";
}
sub make0_from_node___code {
  my($cls,$m)=@_;
  "$m" =~ /\A\((\?\??){(.*?)}\)\z/ or die "bug";
  my($which,$code) = ($1,$2);
  return "code(q{$code})";
}
sub make0_from_node___independent {
  my($cls,$m)=@_;
  my $e = $cls->make0_from_node($m->{pattern});
  return "independent($e)";
}
sub make0_from_node___conditional {
  my($cls,$m)=@_;
  my($a,$h) = $cls->make0_from_children($m);
  "$m" =~ /\A\(\?\((.*?)\)/ or die "bug";
  my $test_ish = $1;
  my $test;
  if($test_ish =~ /\A\d+\z/) {
    $test = $test_ish;
  } else {
    $test = $h->{_lookaround};
  }
  my $expr_then = $h->{regex_sequence}[0];
  my $expr_else = $h->{regex_sequence}[1];
  my $then_else = $expr_then.(defined $expr_else ? ",$expr_else" : "");
  return "conditional($test,$then_else)";
}
sub make0_from_node___lookaround {
  my($cls,$m)=@_;
  my $e = $cls->make0_from_node($m->{pattern});
  "$m" =~ /\A\(\?(<?[=!])/ or die "bug";
  my $flavor = $1;
  my $args = {'='=>[1,1],
              '!'=>[1,0],
              '<='=>[0,1],
              '<!'=>[0,0]}->{$flavor};
  my $s = join(",",@$args);
  return "lookaround($s,$e)";
}
sub make0_from_node__regex {
  my($cls,$m)=@_;
  my $e = $cls->make0_from_node($m->{pattern});
  return "aregexm(':p5',$e)";
}


sub new_rx_from_re {
  my($cls,$inpkg,$pat,$mods)=@_;
  my $re = $pat;
  if($mods){
    if($mods =~ /:/){
      $re = $mods."::$re";
    } else {
      $re = "(?$mods)$re";
    }
  }
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
    $ast = eval("namespace('::$inpkg',$mexpr)");
    die if $@;
    $ast->RAST_init;
#    use Data::Dumper; print STDERR Dumper $ast;##
    my($rx) = $ast->RMARE_emit;
    $rx;
  };
  Carp::confess "compile \"$re\" failed: $@" if !defined $o;
  $o->_init($pat,$mods,$re,$mexpr,$ast);
}
sub biind_rx {
  my($cls,$pkg,$name,$rx)=@_;die "api assert" if @_ != 4;
  eval("package $pkg; *$name = \$rx"); die $@ if $@;
  $rx;
}

#======================================================================
# P5 Regexps with subrules
#
package Regexp::ModuleA::P5WithSubrules;
@Regexp::ModuleA::P5WithSubrules::ISA=qw(Regexp::ModuleA::P5);
Regexp::ModuleA::AST::Make0->import;

{
  my $nonmeta = '[^[)({^$?*+\\\\\.|<]';
  namespace(""
            ,biind('_subrule',aregex(seq(pat5('\<[?!]*\w+'),ques(seq(pat5('\s+'),plus(sr('pattern')))),exact('>'))))
            ,biind('_nonmeta',aregex(pat5("$nonmeta(?:$nonmeta+(?![?*+{]))?")))
            ,biind('test1',aregex(pat5('\w{2}')))
            )->RAST_init->RMARE_emit;
}

sub make0_from_node___subrule {
  my($cls,$m)=@_;
  my @v = map{$cls->make0_from_node($_)} @{$m->{pattern}};
  "$m" =~ /\A<([?!]*(\w+))/ or die "bug";
  my $name = $1;
  my $args = (@v ? "," : "").join(",",map{"aregex($_)"}@v);
  return "sr('$name'$args)";
}


### The unicode stuff has to be metaprogrammed.  Either in p6 or p5.
#======================================================================
# P6 Regexps
#
#======================================================================
package Regexp::ModuleA::Api::PreludeA;
require Exporter;
@Regexp::ModuleA::Api::PreludeA::ISA=qw(Exporter);
@Regexp::ModuleA::Api::PreludeA::EXPORT =
@Regexp::ModuleA::Api::PreludeA::EXPORT_OK =
  qw(regex_api0 regex_ast_maker_api0
     alpha alnum ascii blank cntrl digit
     graph lower print punct space upper
     word xdigit
     commit null before after sp lt gt dot ident wb ws fail
     name _nofat
     );
# also exports the unicode classes defined below.

sub regex_api0 {'Regexp::ModuleA::Api::RegexApi0'}
sub regex_ast_maker_api0 {'Regexp::ModuleA::AST::Make1'}

Regexp::ModuleA::AST::Make0->import;
sub nrx {my($name,$v)=@_; biind($name,aregex($v));}
namespace(""
          ,(map{nrx($_,pat5("[[:$_:]]"))}
            qw(alpha alnum ascii blank cntrl digit
               graph lower print punct space upper
               word xdigit))
          ,nrx('commit',commit_match())
          ,nrx('null',pat5(''))
          ,nrx('before',lookaround(1,1,coderx(q{$Regexp::ModuleA::ReentrantEngine::Env::nested_data->{args}[0]||qr/(?!)/})))
          ,nrx('after',lookaround(0,1,coderx(q{$Regexp::ModuleA::ReentrantEngine::Env::nested_data->{args}[0]||qr/(?!)/})))
          ,nrx('sp',pat5('[ ]'))
          ,nrx('lt',pat5('<'))
          ,nrx('gt',pat5('>'))
          ,nrx('dot',pat5('\.'))
          ,nrx('ident',pat5('(?:_|[[:alpha:]])\w*'))
          ,nrx('wb',pat5('\b'))
          ,nrx('ws',alt(pat5('(?!\s)(?!\w)'),
                        pat5('(?!\s)(?<!\w)'),
                        plus(pat5('\s'))))
          ,nrx('fail',pat5('(?!)'))
          # These are required by rx_*.t, but are they really Prelude?
          ,nrx('name',alt(seq(sr('ident'),sr('_nofat'),star(exact('::'),sr('ident'))),
                          plus(exact('::'),sr('ident'))))
          ,nrx('_nofat',pat5('')) # <!before \h* <?unsp>? =\> >
          )->RAST_init->RMARE_emit;

my @unicode_classes = (
  #perl-5.9.4/pod/perlunicode.pod
  #=item General Category
  qw(L  Letter LC CasedLetter Lu UppercaseLetter Ll LowercaseLetter Lt TitlecaseLetter Lm ModifierLetter Lo OtherLetter M  Mark Mn NonspacingMark Mc SpacingMark Me EnclosingMark N  Number Nd DecimalNumber Nl LetterNumber No OtherNumber P  Punctuation Pc ConnectorPunctuation Pd DashPunctuation Ps OpenPunctuation Pe ClosePunctuation Pi InitialPunctuation Pf FinalPunctuation Po OtherPunctuation S  Symbol Sm MathSymbol Sc CurrencySymbol Sk ModifierSymbol So OtherSymbol Z  Separator Zs SpaceSeparator Zl LineSeparator Zp ParagraphSeparator C  Other Cc Control Cf Format Cs Surrogate Co PrivateUse Cn),
  #=item Bidirectional Character Types
  # separate
  #=item Scripts
  qw(Arabic Armenian Bengali Bopomofo Buhid CanadianAboriginal Cherokee Cyrillic Deseret Devanagari Ethiopic Georgian Gothic Greek Gujarati Gurmukhi Han Hangul Hanunoo Hebrew Hiragana Inherited Kannada Katakana Khmer Lao Latin Malayalam Mongolian Myanmar Ogham OldItalic Oriya Runic Sinhala Syriac Tagalog Tagbanwa Tamil Telugu Thaana Thai Tibetan Yi),
  #=item Extended property classes
  qw(ASCIIHexDigit BidiControl Dash Deprecated Diacritic Extender GraphemeLink HexDigit Hyphen Ideographic IDSBinaryOperator IDSTrinaryOperator JoinControl LogicalOrderException NoncharacterCodePoint OtherAlphabetic OtherDefaultIgnorableCodePoint OtherGraphemeExtend OtherLowercase OtherMath OtherUppercase QuotationMark Radical SoftDotted TerminalPunctuation UnifiedIdeograph WhiteSpace),
  # and there are further derived properties:
  qw(Alphabetic Lowercase Uppercase Math ID_Start ID_Continue Any Assigned Common),
  #=item Blocks
  qw(InAlphabeticPresentationForms InArabic InArabicPresentationFormsA InArabicPresentationFormsB InArmenian InArrows InBasicLatin InBengali InBlockElements InBopomofo InBopomofoExtended InBoxDrawing InBraillePatterns InBuhid InByzantineMusicalSymbols InCJKCompatibility InCJKCompatibilityForms InCJKCompatibilityIdeographs InCJKCompatibilityIdeographsSupplement InCJKRadicalsSupplement InCJKSymbolsAndPunctuation InCJKUnifiedIdeographs InCJKUnifiedIdeographsExtensionA InCJKUnifiedIdeographsExtensionB InCherokee InCombiningDiacriticalMarks InCombiningDiacriticalMarksforSymbols InCombiningHalfMarks InControlPictures InCurrencySymbols InCyrillic InCyrillicSupplementary InDeseret InDevanagari InDingbats InEnclosedAlphanumerics InEnclosedCJKLettersAndMonths InEthiopic InGeneralPunctuation InGeometricShapes InGeorgian InGothic InGreekExtended InGreekAndCoptic InGujarati InGurmukhi InHalfwidthAndFullwidthForms InHangulCompatibilityJamo InHangulJamo InHangulSyllables InHanunoo InHebrew InHighPrivateUseSurrogates InHighSurrogates InHiragana InIPAExtensions InIdeographicDescriptionCharacters InKanbun InKangxiRadicals InKannada InKatakana InKatakanaPhoneticExtensions InKhmer InLao InLatin1Supplement InLatinExtendedA InLatinExtendedAdditional InLatinExtendedB InLetterlikeSymbols InLowSurrogates InMalayalam InMathematicalAlphanumericSymbols InMathematicalOperators InMiscellaneousMathematicalSymbolsA InMiscellaneousMathematicalSymbolsB InMiscellaneousSymbols InMiscellaneousTechnical InMongolian InMusicalSymbols InMyanmar InNumberForms InOgham InOldItalic InOpticalCharacterRecognition InOriya InPrivateUseArea InRunic InSinhala InSmallFormVariants InSpacingModifierLetters InSpecials InSuperscriptsAndSubscripts InSupplementalArrowsA InSupplementalArrowsB InSupplementalMathematicalOperators InSupplementaryPrivateUseAreaA InSupplementaryPrivateUseAreaB InSyriac InTagalog InTagbanwa InTags InTamil InTelugu InThaana InThai InTibetan InUnifiedCanadianAboriginalSyllabics InVariationSelectors InYiRadicals InYiSyllables));
my @unicode_bidi_classes = (
  qw(L LRE LRO R AL RLE RLO PDF EN ES ET AN CS NSM BN B S WS ON));
for my $class (@unicode_classes) {
  my $name = "is$class";
  namespace("",nrx($name,pat5("\\p{$class}")))->RAST_init->RMARE_emit;
  push(@Regexp::ModuleA::Api::PreludeA::EXPORT,$name);
  push(@Regexp::ModuleA::Api::PreludeA::EXPORT_OK,$name);
}
for my $class (@unicode_bidi_classes) {
  my $name = "isBidi$class";
  namespace("",nrx($name,pat5("\\p{BidiClass:$class}")))->RAST_init->RMARE_emit;
  push(@Regexp::ModuleA::Api::PreludeA::EXPORT,$name);
  push(@Regexp::ModuleA::Api::PreludeA::EXPORT_OK,$name);
}
#XXX Lr - it's defined in propcharset.t, but its not in perlunicode.
namespace("",nrx('isLr',pat5("\\p{Ll}|\\p{Lu}|\\p{Lt}")))->RAST_init->RMARE_emit;
push(@Regexp::ModuleA::Api::PreludeA::EXPORT,'isLr');
push(@Regexp::ModuleA::Api::PreludeA::EXPORT_OK,'isLr');

1;
#======================================================================
package Regexp::ModuleA::P6;
@Regexp::ModuleA::P6::ISA=qw(Regexp::ModuleA::P5WithSubrules);
Regexp::ModuleA::AST::Make0->import;
Regexp::ModuleA::Api::PreludeA->import;
  
sub nrx {my($name,$v)=@_; biind($name,aregex($v));}
sub unction {
  my($name,$op,$subname)=@_;
  nrx($name,seq(sr($subname),star($op,sr($subname))));
}
sub unction1 {
  my($name,$op,$subname)=@_;
  nrx($name,seq(ques($op),sr($subname),star($op,sr($subname))));
}

{
  my $nonmeta = '(?:(?!\s)[^][)(><}{&|^$?*+\\\\\.:\#])';
  namespace(""
            ,biind('regex',aregex(seq(mod_inline(':perl5_x'),sr('pattern'))))
            ,nrx('pattern',sr('regex_ordered_disjunction'))
            ,unction1('regex_ordered_disjunction',
                      pat5('\|\|'),'regex_ordered_conjunction')
            ,unction('regex_ordered_conjunction',
                     pat5('\&\&'),'regex_unordered_disjunction')
            ,unction1('regex_unordered_disjunction',
                      pat5('\|(?!\|)'),'regex_unordered_conjunction')
            ,unction('regex_unordered_conjunction',
                     pat5('\&(?!\&)'),'regex_sequence')
            ,nrx('regex_sequence',plus(sr('_regex_sequence_thing')))
            ,nrx('_regex_sequence_thing',alt(seq(sr('_alias'),commit_group()),
                                             sr('regex_quantified_atom')))
            ,nrx('regex_quantified_atom',
                 seq(sr('regex_atom'),ques(sr('regex_quantifier'))))
            ,nrx('regex_quantifier',
                 alt(seq(pat5('\*\*'),#<?ws>
                         sr('block'),sr('quantmod')),
                     seq(pat5('[\*\+\?](?!\*)'),sr('quantmod'))))
            ,nrx('quantmod',ques(pat5('\? | \! | \: | \+')))
            ,nrx('block',pat5('\{[\d\.]+\}'))
            ,biind('regex_atom',aregex(alt(sr('_mod_inline'),sr('_inline5'),sr('_mod_expr'),sr('_code'),sr('_coderx'),sr('_independent'),sr('_conditional'),sr('_lookaround'),sr('_cap'),sr('_grp'),sr('_charclass'),sr('_backref'),sr('_esc'),sr('_nonmeta'),sr('_space'),sr('_dot'),sr('_beosl'),sr('_subrule'),
                                          sr('_commit'),sr('_esc_code'),sr('_word_boundary'),sr('_literal'))))
            ,biind('_nonmeta',aregex(pat5("$nonmeta(?:$nonmeta+(?![?*+:!]))?")))
            ,nrx('_space',pat5('(?>\s+|\#.*\n?)+'))
            ,biind('_grp',aregex(seq(exact('['),sr('pattern'),exact(']'))))
            ,nrx('_cap',seq(pat5('\('),sr('pattern'),exact(')')))
            ,nrx('_commit',pat5(':+'))
            ,biind('_esc',aregex(pat5('\\\\[^\doOxX]')))
            ,nrx('_esc_code',pat5('\\\\[oO][0-7]+|\\\\[xX][0-9a-fA-F]+'))
            ,biind('_backref',aregex(pat5('\$\d+')))
            ,biind('_backref_or_char',aregex(pat5('(?!)')))
            ,nrx('_charclass',seq(pat5('<(?=[-+\[]|\w+[-+])'),plus(sr('_charset')),pat5('>')))
            ,nrx('_charset',seq(pat5('[-+]?'),alt(pat5('\[(?:[^\]\\\\]|\\\\.)*\]'),sr('ident'))))
            ,nrx('_mod_inline',pat5('(?<![\?\*\+\!]):(?!(?i:p5|perl5))\w+(?:<[^>]*>|\([^)]+\))?'))
            ,nrx('_inline5',seq(pat5('(?<![\?\*\+\!]):(?i:p5|perl5)(?:<[^>]*>|\([^)]+\))?'),
                                sr('Regexp::ModuleA::P5.pattern')))
            ,nrx('_dot',pat5('\\.'))
            ,nrx('_beosl',pat5('[\^\$]{1,2}'))

            ,nrx('_alias',seq(sr('_alias_target'),pat5('\s*:=\s*'),sr('_construct')))
            ,nrx('_alias_target',seq(alt(seq(pat5('[\$\@\%]<'),sr('name'),pat5('>')),
                                         pat5('[\$\@\%]\d+'),
                                         pat5('[\$\@\%]\/'),
                                         seq(pat5('[\$\@\%]'),sr('name'))),
                                     star(sr('_alias_tail'))))
            ,nrx('_alias_tail',alt(seq(pat5('<'),sr('name'),pat5('>')),
                                   pat5('\[\d+\]')))
            ,nrx('_construct',alt(sr('_alias'),
                                  sr('_quantified_non_alias_construct')))
            ,nrx('_quantified_non_alias_construct',
                 seq(sr('_non_alias_construct'),ques(sr('regex_quantifier'))))
            ,nrx('_non_alias_construct',alt(sr('_subrule'),sr('_cap'),sr('_grp')))

            ,nrx('_word_boundary',pat5('<<|>>|\x{abd}|\x{bbd}'))
            ,nrx('_literal',pat5('<\'(?:[^\'\\\\]|\\\\.)*\'>'))

            )->RAST_init->RMARE_emit;
}


sub make0_from_node__regex {
  my($cls,$m)=@_;
  my $e = $cls->make0_from_node($m->{pattern});
  return "aregex(seq(mod_inline(':perl5_x'),$e))";
}
sub make0_from_node___commit {
  my($cls,$m)=@_;
  my $pat = "$m";
  my $what = {':'=>'sequence',
              '::'=>'group',
              ':::'=>'regex'}->{$pat};
  die "assert" if !$what;
  return "commit_${what}()";
}
sub make0_from_node__regex_sequence {
  my($cls,$m)=@_;
  my @v = map{$cls->make0_from_node($_)} @{$m->{_regex_sequence_thing}};
  return (@v != 1 ? ("seq(".join(",",@v).")") : $v[0]);
}
sub make0_from_node___alias {
  my($cls,$m)=@_;
  my $a = $m->{_alias_target}."";
  my $e = $cls->make0_from_node($m->{_construct});
  return "alias('$a',$e)";
}
sub make0_from_node___quantified_non_alias_construct {
  shift->make0_from_node__regex_quantified_atom(@_);
}
sub make0_from_node__regex_quantified_atom {
  my($cls,$m)=@_;
  my $e = $cls->make0_from_node($m->{regex_atom} || $m->{_non_alias_construct});
  return $e if !$m->{regex_quantifier};
  my $q = $m->{regex_quantifier}[0]."";
  if($q =~ /^\*\*{(\d+)(?:,(\d*))?}(\?)?\z/) {
    my $ng = defined $3 ? '_ng' : '';
    my $min = $1;
    my $max = !defined $2 ? $min : $2 ne "" ? $2 : 1000**1000**1000; # inf
    $e = "quant${ng}($min,$max,$e)";
  }
  elsif($q =~ /^([?*+])(\?)?\s*\z/) {
    my $ng = defined $2 ? '_ng' : '';
    $e = "ques${ng}($e)" if $1 eq '?';
    $e = "star${ng}($e)" if $1 eq '*';
    $e = "plus${ng}($e)" if $1 eq '+';
  }
  else { die "bug >>$q<<" }
  return $e;
}
sub make0_from_node__regex_ordered_disjunction {
  my($cls,$m)=@_;
  my @v = map{$cls->make0_from_node($_)} @{$m->{regex_ordered_conjunction}};
  return (@v > 1 ? ("alt(".join(",",@v).")") : $v[0]);
}
sub make0_from_node__regex_ordered_conjunction {
  my($cls,$m)=@_;
  my @v = map{$cls->make0_from_node($_)} @{$m->{regex_unordered_disjunction}};
  return $v[0] if @v == 1;
  return "conj(".join(",",@v).")";
}
sub make0_from_node__regex_unordered_disjunction {
  my($cls,$m)=@_;
  my @v = map{$cls->make0_from_node($_)} @{$m->{regex_unordered_conjunction}};
  return (@v > 1 ? ("alt(".join(",",@v).")") : $v[0]);
}
sub make0_from_node__regex_unordered_conjunction {
  my($cls,$m)=@_;
  my @v = map{$cls->make0_from_node($_)} @{$m->{regex_sequence}};
  return $v[0] if @v == 1;
  return "conj(".join(",",@v).")";
}
sub make0_from_node___backref {
  my($cls,$m)=@_;
  "$m" =~ /\A\$(\d+)\z/ or die "bug";
  my $n = $1 +1;
  return "backref($n)";
}
sub make0_from_node___esc {
  my($cls,$m)=@_;
  my $pat = "$m";
  $pat =~ /^\\(.)$/ or die "bug";
  my $ch = $1;
  my $nl = '\x0d\x0a?|(?<!\x0d)\x0a|\x2028\x2029';
  my $h = '\x{0009}\x{0020}\x{00a0}\x{1680}\x{180e}\x{2000}\x{2001}\x{2002}\x{2003}\x{2004}\x{2005}\x{2006}\x{2007}\x{2008}\x{2008}\x{2009}\x{200a}\x{202f}\x{205f}\x{3000}';
  my $v = '\x{000a}\x{000b}\x{000c}\x{000d}\x{0085}';
  my $pat1 = {
    T => '[^\t]',
    n => $nl,
    N => "(?!$nl)(?s:.)",
    R => '[^\r]',
    F => '[^\f]',
    E => '[^\e]',
    v => "[$v]",
    V => "[^$v]",
    h => "[$h]",
    H => "[^$h]",
    Q => 'Q', L => 'L', U => 'U',
    z => 'z', Z => 'Z', A => 'A',
    p => 'p', P => 'P', G => 'G',
    b => 'b', B => 'B'
    }->{$ch};
  if(!defined($pat1)){
    $pat1 = $pat;
    $pat1 =~ s/\\([\\\'])/\\\\\\$1/g;
  }
  return "pat5('$pat1')";
}
sub make0_from_node___esc_code {
  my($cls,$m)=@_;
  my $pat = "$m";
  $pat =~ /^\\([oOxX])(.+)$/ or die "bug";
  my $neg = ($1 eq 'O' || $1 eq 'X') ? '^' : '';
  my $code = $2;
  $code =~ s/^0+//;
  my $ox = lc $1;
  if($ox eq 'o') {
  } else {
    $code = 'x'.$code;
  }
  return "pat5('[$neg\\$code]')";
}
sub make0_from_node___charclass {
  my($cls,$m)=@_;
  my @v = map{$cls->make0_from_node($_)} @{$m->{_charset}};
  my(@inc,@not);
  for my $opset (@v) {
    $opset =~ /^([-+]?)(.+)/s or die "bug";
    push(@{$1 eq '-' ? \@not : \@inc},$2);
  }
  my $maybe_alt = sub {
    my(@a)=@_; @a == 1 ? $a[0] : "alt(".join(",",@a).")";
  };
  my $code = "";
  $code .= "sr('!?before',".&$maybe_alt(map{"aregex($_)"}@not).")," if @not;
  $code .= @inc ? &$maybe_alt(@inc) : "pat5('(?s:.)')";
  $code = "seq(".$code.")" if @not;
  return $code;
}
sub make0_from_node___charset {
  my($cls,$m)=@_;
    my $pat = "$m";
    if($pat =~ /^([-+]?)\[(.+)\]$/s) {
      my $op = $1 eq '-' ? '-' : '+';
      my $set = $2;
      die "parse error - unescaped hyphen" if $set =~ /(^|[^\\])\-/;
      $set =~ s/\.\./-/g;
      $set =~ s/\\([\\\'])/\\\\\\$1/g;
      return $op."pat5('[$set]')";
    }
    elsif($pat =~ /^([-+]?)(\w+)$/) {
      my $op = $1 eq '-' ? '-' : '+';
      return $op."sr('?$2')";
    }
    else { die "bug" }
  }
sub make0_from_node___mod_inline {
  my($cls,$m)=@_;
    my $pat = "$m";
    return "mod_inline('$pat')";
  }
sub make0_from_node___inline5 {
  my($cls,$m)=@_;
  my $e = Regexp::ModuleA::P5->make0_from_match($m->{pattern});
  "mod_expr(':p5',$e)";
}
sub make0_from_node___space {
  my($cls,$m)=@_;
  my $pat = "$m";
  $pat =~ s/\#.*\n?//g;
  $pat =~ s/\\([\\\'])/\\\\\\$1/g;
  return "aspace('$pat')";
}
sub make0_from_node___dot {
  my($cls,$m)=@_;
  return "pat5('(?s:.)')";
}
sub make0_from_node___beosl {
  my($cls,$m)=@_;
    my $pat = "$m";
    my $npat = { '^' => '\A', '$' => '\z',
                 '^^' => '(?m:^)(?!(?=\z)(?<=\n))',
                 '$$' => '(?m:$)(?!(?=\z)(?<=\n))'
                 }->{$pat};
    return "pat5('$npat')";
  }
sub make0_from_node___word_boundary {
  my($cls,$m)=@_;
    my $pat = "$m";
    my $npat = { '<<' => '\b(?=\w)',
                 '>>' => '\b(?<=\w)',
                 "\x{abd}" => '\b(?=\w)',
                 "\x{bbd}" => '\b(?<=\w)'
                 }->{$pat};
    return "pat5('$npat')";
  }
sub make0_from_node___literal {
  my($cls,$m)=@_;
    my $pat = "$m";
    $pat =~ /^<'(.*)'>$/ or die "bug";
    $pat = $1;
    $pat =~ s/\\([\\\'])/\\\\\\$1/g;
    return "pat5('(?-xi:$pat)')";
  }

## This gets completely changed now....
#======================================================================
# Rx
#
package Regexp::ModuleA::Rx;
use Sub::Name;

sub _new_from_ast {
  my($rxclass,$ast,$pkg,$name,$f,$matchergen)=@_;
  $pkg ||= "";
  $name ||= "";
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

#; Local Variables:
#; perl-indent-level: 2
#; perl-continued-statement-offset: 2
#; perl-continued-brace-offset: -2
#; indent-tabs-mode: nil
#; End:
#; vim: shiftwidth=2:
