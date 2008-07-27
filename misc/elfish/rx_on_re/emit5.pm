
sub a_bit_of_p5_for__expand_backtrack_macros() is p5 {'
{ package BacktrackMacrosKludge;
  sub _let_gen {
    my($vars) = @_;
    my $nvars = 1+($vars =~ tr/,//);
    my $tmpvars = join(",",map{"\$__tmp${_}__"}(0..($nvars-1)));
    push(@SCRATCH::_let_stack,[$vars,$tmpvars]);
    "(do{my \$__v__ ; my($tmpvars); { local($vars)=($vars); \$__v__ = do{ ";
  }
  sub _let_end {
    my $e = shift(@SCRATCH::_let_stack) || die "LET(){ }LET pairs didnt match up";
    my($vars,$tmpvars) = @$e;
    "}; if(!FAILED(\$__v__)){ ($tmpvars)=($vars); }}; if(!FAILED(\$__v__)){ ($vars)=($tmpvars) }; \$__v__ })"
  }
}
'};
a_bit_of_p5_for__expand_backtrack_macros();


class EmitRegex {

  method expand_backtrack_macros ($code) {

    $code.re_sub('\bLET\(([^\)]+)\)\{','BacktrackMacrosKludge::_let_gen($1)','eg
');
    $code.re_sub('\}LET;','BacktrackMacrosKludge::_let_end().";"','eg');

    $code.re_sub_g('\bFAIL_IF_FAILED\(([^\)]+)\);','return($1) if FAILED($1);');
    $code.re_sub_g('\bFAIL\(\)','return(undef)');
    $code.re_sub_g('\bFAILED\(([^\)]+)\)','(!defined($1)||(!ref($1)&&($1<=0)))')
;

    $code.re_sub_g('\bFAIL_SEQUENCE\(\)','die("fail sequence\\\\n")');
    $code.re_sub_g('\bFAIL_GROUP\(\)','die("fail group\\\\n")');
    $code.re_sub_g('\bFAIL_REGEX\(\)','die("fail regex\\\\n")');
    $code.re_sub_g('\bFAIL_MATCH\(\)','die("fail match\\\\n")');

    $code.re_sub_g('\bTAILCALL\(([^,\)]+),?([^\)]*)\);','\@_=($2);goto \&$1;');

    #print $code;
    $code;
  };


  method regex_prelude () {
    self.expand_backtrack_macros('

{ package VersionConstraints;
  use Regexp::Common 2.122;
  use Sub::Name 0.03;
  use Filter::Simple 0.82;
}

package Regexp::ModuleA;
use strict;
use warnings;
use Carp;

#======================================================================
# Core Regexp Engine ("RMARE")
#
# NOTE: Time to refactor.  A perlbug used to prevent it.  Should be ok now.

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

  # noop

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
\n subname \'<conj \'.(\$sub_id++).\">\" => sub {my \$cn = \$_[0];
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
      $onto = [] if ref($onto) ne "ARRAY";
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
      $$m1->{RULE} ||= $name; #EEEP

      my $close = subname "<subrule-close ".($myid)." $name>" => sub {
	my $cn = $_[0];

        $Regexp::ModuleA::ReentrantEngine::Env::nested_data = $nd;

	$$m1->{match_to} = $Regexp::ModuleA::ReentrantEngine::Env::pos; #EEEP
	$$m1->{match_string} = substr($Regexp::ModuleA::ReentrantEngine::Env::str,$pos,$Regexp::ModuleA::ReentrantEngine::Env::pos-$pos);

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
#           LET($$m0->{match_hash}{$name}){
#             if($in_quant) {
#               $$m0->{match_hash}{$name} = [@{$$m0->{match_hash}{$name}||[]}];
#               push(@{$$m0->{match_hash}{$name}},$m1);
#             } else {
#               $$m0->{match_hash}{$name} = $m1;
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
          $$m1->{match_to} = $$m1->{match_from};
          $$m1->{match_string} = "";

# =pod
#           LET($$m0->{match_hash}{$name}){
#             $$m0->{match_hash}{$name} = [@{$$m0->{match_hash}{$name}||[]}];
#             push(@{$$m0->{match_hash}{$name}},$m1);
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
        $m->match_set(1,substr($Regexp::ModuleA::ReentrantEngine::Env::str,$start,$Regexp::ModuleA::ReentrantEngine::Env::pos-$start),$$m->{match_array},$$m->{match_hash},$start,$Regexp::ModuleA::ReentrantEngine::Env::pos);
        return $m;
      }
    }
    return Regexp::ModuleA::ReentrantEngine::Match0->new_failed();
  }

  # Commits

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
');
  };
};

eval_perl5( EmitRegex.regex_prelude() );
