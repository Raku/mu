
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
  sub replace_LETs {
    my($s)=@_;
    $s =~ s/\bLET\(([^\)]+)\)\{/BacktrackMacrosKludge::_let_gen($1)/eg;
    $s =~ s/\}LET;/BacktrackMacrosKludge::_let_end().";"/eg;
    $s;
  }
}
'};
a_bit_of_p5_for__expand_backtrack_macros();


class EmitRegex {

  method expand_LETs($s) is p5 {' BacktrackMacrosKludge::replace_LETs($s) '}
  method expand_backtrack_macros ($code) {

    $code = $.expand_LETs($code);
    $code.re_sub('\bFAIL_IF_FAILED\(([^\)]+)\);','return($1) if FAILED($1);','g');
    $code.re_sub('\bFAIL\(\)','return(undef)','g');
    $code.re_sub('\bFAILED\(([^\)]+)\)','(!defined($1)||(!ref($1)&&($1<=0)))','g')
;

    $code.re_sub('\bFAIL_SEQUENCE\(\)','die("fail sequence\\\\n")','g');
    $code.re_sub('\bFAIL_GROUP\(\)','die("fail group\\\\n")','g');
    $code.re_sub('\bFAIL_REGEX\(\)','die("fail regex\\\\n")','g');
    $code.re_sub('\bFAIL_MATCH\(\)','die("fail match\\\\n")','g');

    $code.re_sub('\bTAILCALL\(([^,\)]+),?([^\)]*)\);','\@_=($2);goto \&$1;','g');

    #print $code;
    $code;
  };


  method regex_prelude () {
    my $rmare = self.expand_backtrack_macros('
#line 1 "regex_prelude-rmare"

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
  package IRx1::RxBaseClass;

  use Sub::Name;
  our $sub_id = 1;

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
      my $a = $Regexp::ModuleA::ReentrantEngine::Env::leaf_match->{match_array};
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
#line 2 \"IRx1::RxBaseClass RMARE_conj\"
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
#line 2 \"IRx1::RxBaseClass RMARE_concat\"
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
    my $myid = $sub_id++;
    my $foo = subname "<group ".($myid).">" => sub {
      my $cn = $_[0];
      my $nd = $Regexp::ModuleA::ReentrantEngine::Env::nested_data;
      my $close = subname \'<capture-close \'.($myid).">" => sub {
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
        $m->match_set(1,substr($Regexp::ModuleA::ReentrantEngine::Env::str,$from,$to-$from),$m->{match_array},$m->{match_hash},$from,$to);
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
        $m->match_set(1,substr($Regexp::ModuleA::ReentrantEngine::Env::str,$from,$to-$from),$m->{match_array},$m->{match_hash},$from,$to);
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
    my $top = \'$Regexp::ModuleA::ReentrantEngine::Env::leaf_match\';
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
        $localize .= "{\'$key\'}" if !$is_final;
        if($is_final){
          $copy = \'{%{\'.$localize.\'}}\';
          $access = "{\'$key\'}";
        }
      } else { die "bug" };
    }
    my $array_alias = $root =~ /^\@/;
    my $code = \'
subname "<alias_wrap ".($myid).">" => sub {
  my($c)=@_;
  my $m = $Regexp::ModuleA::ReentrantEngine::Env::alias_match;
  if(1 || !defined($m)){#XXXXX
    $m = Regexp::ModuleA::ReentrantEngine::Match0->new_failed();
    if($is6) {
      my $a = [map{Regexp::ModuleA::ReentrantEngine::Match0->new_failed()} (1..$nparen6)];
      $m->{match_array} = $a;
    }
  }
  return LET(\'.$localize.\'){
    my $newa = \'.$copy.\';
    \'.$localize.\' = $newa;
    if(\'.($is6 && $in_quant ? 1 : 0).\') {
      my $onto = $newa->\'.$access.\';
      $onto = [] if ref($onto) ne "ARRAY";
      $onto = [@$onto,($array_alias ? @{$m->{match_array}} : $m)];
      $newa->\'.$access.\' = $onto;
    } else {
      $newa->\'.$access.\' = (\'.($array_alias?1:0).\' ? [$m] : $m);
    }
    local $Regexp::ModuleA::ReentrantEngine::Env::alias_match = $m;
    $f->($c);
  }LET;
}\';
#print STDERR $code;
    my $capf = eval($code);
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
        $m1 = Regexp::ModuleA::ReentrantEngine::Match0->new_failed();
      }
      $m1->match_set(1,"",[],{},$pos,undef);
      $m1->{match_rule} ||= $name; #EEEP

      my $close = subname "<subrule-close ".($myid)." $name>" => sub {
	my $cn = $_[0];

        $Regexp::ModuleA::ReentrantEngine::Env::nested_data = $nd;

	$m1->{match_to} = $Regexp::ModuleA::ReentrantEngine::Env::pos; #EEEP
	$m1->{match_string} = substr($Regexp::ModuleA::ReentrantEngine::Env::str,$pos,$Regexp::ModuleA::ReentrantEngine::Env::pos-$pos);

        my $post = $name."__post_action";
        if(UNIVERSAL::can($pkg9,$post)) {
          $m1->_prepare_match_for_embedded_code;
          $pkg9->$post($m1);
        }

	$Regexp::ModuleA::ReentrantEngine::Env::current_match = $m0;
	$Regexp::ModuleA::ReentrantEngine::Env::leaf_match = $m0b;
	local $Regexp::ModuleA::ReentrantEngine::Env::pkg = $pkg0;

# =pod
#         if(!$nocap) {
#           LET($m0->{match_hash}{$name}){
#             if($in_quant) {
#               $m0->{match_hash}{$name} = [@{$m0->{match_hash}{$name}||[]}];
#               push(@{$m0->{match_hash}{$name}},$m1);
#             } else {
#               $m0->{match_hash}{$name} = $m1;
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
          $m1->{match_to} = $m1->{match_from};
          $m1->{match_string} = "";

# =pod
#           LET($m0->{match_hash}{$name}){
#             $m0->{match_hash}{$name} = [@{$m0->{match_hash}{$name}||[]}];
#             push(@{$m0->{match_hash}{$name}},$m1);
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

  sub RMARE_aregex_create {
    my($o,$f,$nparenx)=@_;
    $nparenx = 0 if !defined $nparenx; #XXX arguments to subrules.  aregex not seeing an init.
    subname "<aregex ".($sub_id++).">" => sub {
      my($c)=@_;

      my $m = $Regexp::ModuleA::ReentrantEngine::Env::leaf_match;
      my $a = [map{Regexp::ModuleA::ReentrantEngine::Match0->new_failed()} (1..$nparenx)];
      $m->{match_array} = $a;

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
    my($o,$f,$s,$beginat,$minlen,$nparen)=@_;
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
        $m->match_set(1,substr($Regexp::ModuleA::ReentrantEngine::Env::str,$start,$Regexp::ModuleA::ReentrantEngine::Env::pos-$start),$m->{match_array},$m->{match_hash},$start,$Regexp::ModuleA::ReentrantEngine::Env::pos);
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


  #------------------------------------------------------------
  # These were originally non-core...

  sub RMARE_subrule_fetching_rx {
    my($o,$pkg,$pkg_override,$name,$exprse,$neg,$nocap,$in_quant,$target_spec)=@_;
    my $fetch = subname "<subrule-fetch for $name>" => sub {
      my $pkg9 = ($pkg_override ||
                  $Regexp::ModuleA::ReentrantEngine::Env::pkg ||
                  $pkg);
      die "assert" if !defined $pkg9;
      no strict;
      my $f;
      eval { $f = $pkg9->$name($name)->(\' api0\'); };
      Carp::confess $@ if $@;
      die if $@;
      use strict;
      die "assert" if !defined $f;
      $f;
    };
    $o->RMARE_subrule($fetch,$pkg,$pkg_override,$name,$exprse,$neg,$nocap,$in_quant,$target_spec);
  }


  sub RMARE_lookaround {
    my($o,$is_forward,$is_positive,$f)=@_;
    my $noop = $o->RMARE_noop;
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

  sub RMARE_conditional {
    my($o,$f_test,$idx,$f_then,$f_else)=@_;
    my $noop = $o->RMARE_noop;
    if(!$f_else) {
      $f_else = subname "<conditional else>" => sub {
        my $c = $_[0]; TAILCALL($c,$noop);
      };
    }
    if(defined($idx)) {
      $idx = $idx +0;
      $f_test = subname "<conditional test>" => sub {
        my $c = $_[0];
        my $a = $Regexp::ModuleA::ReentrantEngine::Env::current_match->match_array;
        FAIL() if $idx > @$a;
        my $m = $a->[$idx-1];
        FAIL() if !$m->match_boolean;
        TAILCALL($c,$noop);
      };
    }
    subname "<conditional>" => sub {
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

  # XXX high klude factor
  # (?{ ... })
  sub RMARE_code {
    my($o,$code)=@_;
    my $noop = $o->RMARE_noop;
    $code = "\'\'" if $code =~ /\A\s*\z/; #YYY XXX Why?
    my $tmp = _rewrite_matchvars($o,$code);
    my $need_match = $code ne $tmp || $code =~ /\$M\b/;
    $code = $tmp;
    my $src = \'
#line 2 "in Regexp::ModuleA::Code"
sub{my $__c__ = $_[0];
\'.(!$need_match ? \'\' :
\'  my $M = $Regexp::ModuleA::ReentrantEngine::Env::current_match;
  $M->_prepare_match_for_embedded_code;\').\'
 \'.$code.\';
 $__c__->($noop);}\';
    #print STDERR $src,"\n";
    eval($src) || die "Error compiling (?{$code}) :\n$@\n";
  }

   # XXX high klude factor
   # (??{ ... })
  sub RMARE_coderx {
    my($o,$code)=@_;
     $code = "\'\'" if $code =~ /\A\s*\z/;
     my $tmp = $o->_rewrite_matchvars($code);
     my $need_match = $code ne $tmp || $code =~ /\$M\b/;
     $code = $tmp;
     #XXX Really need to PPI the code.
     my $has_local = $code =~ /\blocal\b/;
     my $has_semi = $code =~ /;/;
     $code = ($has_semi && !$has_local) ? "do{$code}" : "($code)";
     warn "(??{...}) currently doesnt support code with multiple statments and local()" if $has_local && $has_semi;
     my $src = \'
 #line 2 "in Regexp::ModuleA::CodeRx"
 sub{my $__c__ = $_[0];
 \'.(!$need_match ? \'\' :
 \'  my $M = $Regexp::ModuleA::ReentrantEngine::Env::current_match;
   $M->_prepare_match_for_embedded_code;\').\'
   my $__rx__ = \'.$code.\';
   die "(??{...}) returned undef" if !defined $__rx__;
 #  $__rx__ = "(?!)" if !defined $__rx__;
   my $__f__ = (ref($__rx__) eq "Regexp" || !ref($__rx__)) ? $o->RMARE_eat_regexp("$__rx__") : $__rx__->(" api0");
   $__f__->($__c__) }\';
     #print STDERR $src,"\n";
     eval($src) || die "Error compiling (?{$code}) :\n$@\n";
   }
   sub _rewrite_matchvars {
     my($o_ignored,$s)=@_;
     local $_ = $s;
     s/\$([1-9])/\'$M->match_array->[\'.($1-1).\']\'/eg; #XXX more...
     $_;
   }

  sub RMARE_aregex {
    my($o,$pkg,$name,$f,$nparen)=@_;
    # Why the extra sub?  60+% shorter re_text runtime.  sigh.
    my $matchergen = subname "even with subname used?" => sub {
      subname "<an aregex-matcher for $o>" => sub {
        my($pkg9,$name1,$s,$beginat,$minlen)=@_;
        local $Regexp::ModuleA::ReentrantEngine::Env::pkg = $pkg9;
        my $m = $o->RMARE_do_match($f,$s,$beginat,$minlen,$nparen);
        $m->_prepare_match_for_return;
        $m->{match_rule} = $name1;
        if($name1) {
          my $post = $name1."__post_action";
          $pkg9->$post($m) if UNIVERSAL::can($pkg9,$post);
        }
        $m;
      }
    };
    Regexp::ModuleA::Rx->_new_from_ast($o,$pkg,$name,$f,$matchergen);
  }

  sub RMARE_biind {
    my($o,$pkg,$name,$fr)=@_;
    eval("package $pkg; *$name = \$fr"); die "assert" if $@;
    $fr;
  }

  sub RMARE_namespace {
    my($o,$pkg)=@_;
    eval("package $pkg;"); die "assert" if $@;
    undef;
  }

}
');
   my $rx = '
#line 1 "regex_prelude-rx"
#======================================================================
# Rx
#
#-- copy.

package Regexp::ModuleA::Rx;
use Sub::Name;

sub _new_from_ast {
  my($rxclass,$ast,$pkg,$name,$f,$matchergen)=@_;
  $pkg ||= "";
  $name ||= "";
  my $h = {ast=>$ast,pkg=>$pkg,name=>$name,f=>$f,matchergen=>$matchergen};
  my $self;
  my $showname = $name || \'*anon*\';
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
      if($request eq \' api0\') { return $f }
      if($request eq \' hash\') { return $h }
      if($request eq \' match\') {
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
  my $h = $o->(\' hash\');
  $h->{pattern} = $pat;
  $h->{modifiers} = $mods;
  $h->{regexp} = $re;
  $h->{mexpr} = $mexpr;
  $h->{ast} = $ast;
  $o;
}

sub match {
  my($o,$str)=@_;
  $o->(\' match\',$str);
}

sub _mexpr {
  my($o)=@_;
  $o->(\' hash\')->{mexpr};
}

';

   my $match = '
#line 1 "regexrx-match"
#======================================================================
# Match
#

{ package Regexp::ModuleA::ReentrantEngine::Match2;
  @Regexp::ModuleA::ReentrantEngine::Match2::ISA = qw(Match);

  sub _prepare_match_for_return { }
  sub _prepare_match_for_embedded_code { }

}
{ package Regexp::ModuleA::ReentrantEngine::Match1;
  @Regexp::ModuleA::ReentrantEngine::Match1::ISA =
      qw(Match Regexp::ModuleA::ReentrantEngine::Match_internal);

  sub _prepare_match_for_embedded_code { }

  use overload \'bool\' => \'match_boolean\', ;

}
{ package Regexp::ModuleA::ReentrantEngine::Match0;
  @Regexp::ModuleA::ReentrantEngine::Match0::ISA =
      qw(Match Regexp::ModuleA::ReentrantEngine::Match_internal);

  sub _prepare_match_for_embedded_code {
    my($o)=@_;
    bless $o, \'Regexp::ModuleA::ReentrantEngine::Match1\';
  }

  sub match_new {
    my($cls)=@_;
    my $h = {
      match_boolean => 1,
      match_string  => "",
      match_array   => [],
      match_hash    => {},
      match_from    => undef,
      match_to      => undef,
    };
    my $o = $h;
    bless $o,$cls;
    #$o->match_set(1,"",[],{},undef,undef);
    return $o;
  }
  sub new_failed {my($cls)=@_; $cls->match_new()->match_set_as_failed()}

}
{ package Regexp::ModuleA::ReentrantEngine::Match_internal;

  sub _prepare_match_for_return {
    my($o)=@_;
    use Carp; Carp::confess if ref($o) !~ /[a-z]/;
    for my $m (map{ref($_)eq\'ARRAY\'?@$_:$_}@{$o->match_array}) { $m->_prepare_match_for_return }
    for my $m (map{ref($_)eq\'ARRAY\'?@$_:$_}values %{$o->match_hash}) { $m->_prepare_match_for_return }
    bless $o, \'Regexp::ModuleA::ReentrantEngine::Match2\';
  }

  sub match_set {
    my($o,$b,$s,$a,$h,$from,$to)=@_;
    $o->{match_boolean} = $b;
    $o->{match_string}  = $s;
    $o->{match_array}   = $a;
    $o->{match_hash}    = $h;
    $o->{match_from}    = $from;
    $o->{match_to}      = $to;
    return $o;
  }
  sub match_set_as_failed {
    my($o)=@_;
    $o->match_set(0,"",[],{},undef,undef);
    return $o;
  }

}

';
    $rmare ~ "\n" ~ $match ~ "\n" ~ $rx;
  };
};

eval_perl5( EmitRegex.regex_prelude() );


#----------------------------------------------------------------------
# AST to RMARE emitters
#----------------------------------------------------------------------

package IRx1 {

  class RxBaseClass {
  }

  # any regexp
  class RxPat5 {
    method emit_RMARE () {
      my $re = $.RMARE_wrap_re_with_mods(self.<pat>);
      'IRx1::RxBaseClass->RMARE_eat_regexp("'~quotemeta($re)~'")';
    }
  }

  # \Qabc\E
  class RxExact {
    method emit_RMARE () {
      my $re = self.<text>;
      $re.re_sub('([^\w\s])','\\\\$1','g');
      $re = $.RMARE_wrap_re_with_mods($re);
      'IRx1::RxBaseClass->RMARE_eat_regexp("'~quotemeta($re)~'")';
    }
  }

  # (?imsx-imsx:...)
  class RxMod_expr {
    method emit_RMARE () {
      self.<expr>.emit_RMARE;
    }
  }

  # (?imsx-imsx)
  class RxMod_inline {
    method emit_RMARE () {
      'IRx1::RxBaseClass->RMARE_noop()';
    }
  }

  # ? * + {n,m} ?? *? etc
  class RxQuant {
    method emit_RMARE () {
      my $min = self.<min>;
      my $max = self.<max>;
      my $nongreedy = 0;
      if self.<nongreedy> { $nongreedy = 1 }
      $min = 0 if !defined $min;
      my $maxs = {if defined($max) { ""~$max } else { '1000**1000**1000' }};
      if $maxs eq 'inf' { $maxs = '1000**1000**1000' }
      $max = 1000**1000**1000 if !defined $max; #XXX inf
      die "assert - Quant min <= max" if $min > $max;
      my $f = self.<expr>.emit_RMARE;
      if self.<flags><ratchet> {
        'IRx1::RxBaseClass->RMARE_concat([IRx1::RxBaseClass->RMARE_repeat('~$f~','~$min~','~$maxs~','~$nongreedy~'), IRx1::RxBaseClass->RMARE_commit_sequence])';
      } else {
        'IRx1::RxBaseClass->RMARE_repeat('~$f~','~$min~','~$maxs~','~$nongreedy~')';
      }
    }
  }

  # a|b
  class RxAlt {
    method emit_RMARE {
      my $exprs = self.<exprs>.map(sub ($o){$o.emit_RMARE}).join(',');
      if self.<flags><ratchet> {
        'IRx1::RxBaseClass->RMARE_concat([IRx1::RxBaseClass->RMARE_alt('~$exprs~'),
                        IRx1::RxBaseClass->RMARE_commit_sequence()])';
      } else {
        'IRx1::RxBaseClass->RMARE_alt(['~$exprs~'])';
      }
    }
  }

  # a&b
  class RxConj {
    method emit_RMARE {
      my $exprs = self.<exprs>.map(sub ($o){$o.emit_RMARE}).join(',');
      'IRx1::RxBaseClass->RMARE_conj(['~$exprs~'])';
    }
  }

  # ab
  class RxSeq {
    method emit_RMARE {
      my $exprs = self.<exprs>.map(sub ($o){$o.emit_RMARE}).join(',');
      'IRx1::RxBaseClass->RMARE_concat(['~$exprs~'])';
    }
  }  

  # .. := ...
  class RxAlias {
    method emit_RMARE {
      my $target_spec = self.<target_spec>;
      my $construct_kind = self.<construct_kind>;
      my $construct_in_quant = self.<construct_in_quant>;
      my $f = self.<expr>.emit_RMARE;
      if ($construct_kind eq 'group'
          && $construct_in_quant
          && $target_spec[0].re_matchp('^\$'))
      {
        'IRx1::RxBaseClass->RMARE_alias_wrap(IRx1::RxBaseClass->RMARE_capture_string('~$f~'),
                           undef,1,0,0,'~$target_spec.perl~')';
      }
      else {
        $f;
      }
    }
  }

  # (?:a)
  class RxGrp {
    method emit_RMARE {
      my $target_spec = self.<target_spec>;
      my $in_quant = {if self.<in_quant> { 1 } else { 0 }};
      my $f = self.<expr>.emit_RMARE;
      'IRx1::RxBaseClass->RMARE_group('~$f~','~$target_spec.perl~','~$in_quant~')';
    }
  }

  # (a)
  class RxCap {
    method emit_RMARE {
      my $in_quant = {if self.<in_quant> { 1 } else { 0 }};
      my $target_spec = self.<target_spec>;
      my $is6_ = not self.<flags><p5>;
      my $is6 = $is6_ || 'undef';
      my $idx = {if $is6_
                 { self.<cap6_idx> } else
                 { self.<cap5_idx> }};
      my $nparen6 = self.<nparen6>;
      my $f = self.<expr>.emit_RMARE;
      'IRx1::RxBaseClass->RMARE_capture('~$idx~','~$f~','~$is6~','~$nparen6~','~$in_quant~','~$target_spec.perl~')';
    }
  }

  # \1
  class RxBackref {
    method emit_RMARE {
      my $noop = $.RMARE_noop;
      my $idx = self.<backref_n> -1;
      'IRx1::RxBaseClass->RMARE_eat_backref('~$idx~',"'~quotemeta('(?'~$.RMARE_imsx~')')~'")';
    } #XXX move imsx into eat
  }

  # <foo>
  class RxSubrule {
    method emit_RMARE {
      my $exprs = self.<exprs>.map(sub ($o){$o.emit_RMARE}).join(',');
      my $pkg = {if self.<pkg> {'"'~quotemeta(self.<pkg>)~'"'} else {'__PACKAGE__'}};
      my $name = {if self.<name> {'"'~quotemeta(self.<name>)~'"'} else {'undef'}};
      my $neg = self.<neg> ||'undef';
      my $nocap = self.<nocap> ||'undef';
      my $in_quant = {if self.<in_quant> { 1 } else { 0 }};
      my $pkg_override = 'undef';
      my $g = self.<name>.re_groups('^([\w\:\.]+)\.(\w+)$');
      if $g {
        $name = '"'~quotemeta($g[1])~'"';
        $pkg_override = '"'~quotemeta($g[0])~'"';
      }
      my $target_spec = self.<target_spec>;
      'IRx1::RxBaseClass->RMARE_subrule_fetching_rx('~$pkg~','~$pkg_override~','~$name~',['~$exprs~'],'~$neg~','~$nocap~','~$in_quant~','~$target_spec.perl~')';
    }
  }

  # (?(n)t|f)
  class RxConditional {
    method emit_RMARE {
      my $f_test = 'undef';
      my $idx = 'undef';
      my $f_else = 'undef';
      my $f_then = self.<expr_then>.emit_RMARE;
      if self.<expr_else> {
        $f_else = self.<expr_else>.emit_RMARE;
      }
      if self.<test>.isa("Int") {
        $idx = self.<test>;
      } else {
        $f_test = self.<test>.emit_RMARE;
      }
      'IRx1::RxBaseClass->RMARE_conditional('~$f_test~','~$idx~','~$f_then~','~$f_else~')';
    }
  }

  # (?=) (?<=) (?!) (?<!)
  class RxLookaround {
    method emit_RMARE {
      my $f = self.<expr>.emit_RMARE;
      my $is_forward = 0; if self.<is_forward> { $is_forward = 1 };
      my $is_positive = 0; if self.<is_positive> { $is_positive = 1 };
      'IRx1::RxBaseClass->RMARE_lookaround('~$is_forward~','~$is_positive~','~$f~')';
    }
  }

  # (?>)
  class RxIndependent {
    method emit_RMARE {
      my $f = self.<expr>.emit_RMARE;
      'IRx1::RxBaseClass->RMARE_independent('~$f~')'
    }
  }

  # nonexistent
  class RxCommitSequence {
    method emit_RMARE {
      'IRx1::RxBaseClass->RMARE_commit_sequence'
    }
  }

  # ::
  class RxCommitGroup {
    method emit_RMARE {
      'IRx1::RxBaseClass->RMARE_commit_group'
    }
  }

  # :::
  class RxCommitRegex {
    method emit_RMARE {
      'IRx1::RxBaseClass->RMARE_commit_regex'
    }
  }

  # <commit>
  class RxCommitMatch {
    method emit_RMARE {
      'IRx1::RxBaseClass->RMARE_commit_match'
    }
  }

  # (?{ ... })
  # XXX high klude factor
  # Code is currently p5!
  class RxCode {
    method emit_RMARE {
      my $code5 = self.<code>;
      'IRx1::RxBaseClass->RMARE_code("'~quotemeta($code5)~'")'
    }
  }

  # (??{ ... })
  # XXX high klude factor
  # Code is currently p5!
  class RxCodeRx {
    method emit_RMARE {
      my $code5 = self.<code>;
      'IRx1::RxBaseClass->RMARE_coderx("'~quotemeta($code5)~'")'
    }
  }

  # rx/a/
  class RxARegex {
    method RMARE_emit_and_eval {
      my $src = $.emit_RMARE;
      eval_perl5($src);
    }
    method emit_RMARE {
      my $pkg = {if self.<pkg> {'"'~quotemeta(self.<pkg>)~'"'} else {'undef'}};
      my $name = {if self.<name> {'"'~quotemeta(self.<name>)~'"'} else {'undef'}};
      my $nparenx = {if self.<flags><p5> { self.<nparen> } else { self.<nparen6> }};
      $nparenx = $nparenx || 'undef';
      my $nparen = self.<nparen> ||'undef'; #||undef needed?
      my $expr = self.<expr>.emit_RMARE;
      ('IRx1::RxBaseClass->RMARE_aregex('~$pkg~','~$name~
       ',IRx1::RxBaseClass->RMARE_aregex_create('~$expr~','~$nparenx~'),'~$nparen~')');
    }
  }

  # regex foo /a/; rule foo /a/; token foo /a/
  class RxBiind {
    method RMARE_emit_and_eval {
      my $src = $.emit_RMARE;
      eval_perl5($src);
    }
    method emit_RMARE {
      my $pkg = {if self.<pkg> {'"'~quotemeta(self.<pkg>)~'"'} else {'__PACKAGE__'}};
      my $name = {if self.<name> {'"'~quotemeta(self.<name>)~'"'} else {'undef'}};
      my $fr = self.<expr>.emit_RMARE;
      'IRx1::RxBaseClass->RMARE_biind('~$pkg~','~$name~','~$fr~')';
    }
  }
  
  # grammar Foo::Bar { ... }
  class RxNamespace {
    method RMARE_emit_and_eval {
      my $src = $.emit_RMARE;
      eval_perl5($src);
    }
    method emit_RMARE {
      my $pkg = self.<pkg>;
      '(do{ IRx1::RxBaseClass->RMARE_namespace("'~quotemeta($pkg)~'");'~
      '('~self.<bindings>.map(sub ($o){$o.emit_RMARE}).join(",\n")~') })';
    }
  }


}


class EmitSimpleP5 {
  method cb__RxARegex ($n) {
    $n.emit_RMARE;
  }
  method cb__RegexDef ($n) {
    $n.<pattern>.emit_RMARE;
  }
}

