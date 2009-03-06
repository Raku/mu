
class EmitRegex {

  method expand_LETs($s) is p5 {'
    my $_let_gen = sub {
      my($vars) = @_;
      my $nvars = 1+($vars =~ tr/,//);
      my $tmpvars = join(",",map{"\$__tmp${_}__"}(0..($nvars-1)));
      push(@SCRATCH::_let_stack,[$vars,$tmpvars]);
      "(do{my \$__v__ ; my($tmpvars); { local($vars)=($vars); \$__v__ = do{ ";
    };
    my $_let_end = sub {
      my $e = shift(@SCRATCH::_let_stack) || die "LET(){ }LET pairs didnt match up";
      my($vars,$tmpvars) = @$e;
      "}; if(!FAILED(\$__v__)){ ($tmpvars)=($vars); }}; if(!FAILED(\$__v__)){ ($vars)=($tmpvars) }; \$__v__ })"
    };
    $s =~ s/\bLET\(([^\)]+)\)\{/$_let_gen->($1)/eg;
    $s =~ s/\}LET;/$_let_end->().";"/eg;
    $s;
  '}

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
    my $rmare = $.expand_backtrack_macros('
#line 1 "regex_prelude-rmare"

#======================================================================
# Core Regexp Engine
#
# NOTE: This code is derived from a p5 CPANish module.  One which perlbugs
# prevented ever refactoring.  Hopefully that is over now.
# Lots of refactoring to do.  And since its no longer a general module,
# and doesnt have to cope with uncontrolled p5 code, there is legacy clutter
# and architecture to move away from.
# On the other hand, improving this code isnt project critical path,
# so in some places the cruft is getting deeper.  2009-Mar-02

{ package VersionConstraints;
  use Regexp::Common 2.122;
  use Sub::Name 0.03;
}

local $RXX::str;
local $RXX::pos;
local $RXX::current_match;
local $RXX::leaf_match;
local $RXX::pkg;
local $RXX::nested_data;
local $RXX::alias_match;

# $RXX::current_match; - regex-level $/ .
# $RXX::leaf_match; - $/ .
# $RXX::nested_data; - hash.  before modifying, set to shallow copy of hash.
# $RXX::alias_match; - only used between RMARE_alias_wrap and its wrapees.

{
  package IRx1::RxBaseClass;

  use Sub::Name;
  our $sub_id = 1;

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
  our $our_noop = $noop; #X


  { use re "eval";
  sub RMARE_eat_regexp {
    my($o,$re)=@_;
    my $noop = $o->RMARE_noop;
    my $qr = qr/\G($re)/;
    subname "<eat_regexp ".($sub_id++).">" => sub {
      my $c = $_[0];
      my $str = $RXX::str;
      pos($str) = $RXX::pos;
      $str =~ $qr or FAIL();
      $RXX::pos += length($1);
      TAILCALL($c,$noop);
    }
  }
  }

  sub RMARE_eat_backref {
    my($o,$idx,$mod5_re)=@_;
    my $noop = $o->RMARE_noop;
    subname "<eat_backref ".($sub_id++).">" => sub {
      my $c = $_[0];
      my $a = $RXX::leaf_match->{match_array};
      FAIL() if $idx >= @$a;
      my $m = $a->[$idx];
      $m = $m->[-1] if defined($m) && ref($m) eq "ARRAY";
      FAIL() if !defined($m) || !$m->match_boolean;
      my $re = $m->match_string;
      $re =~ s/(\W)/\\$1/g;

      my $str = $RXX::str;
      pos($str) = $RXX::pos;
      $str =~ /\G$mod5_re($re)/ or FAIL();
      $RXX::pos += length($1);
      TAILCALL($c,$noop);
    };
  }

  sub RMARE_imsx {
    my($o,$flags)=@_;
    my $mod = "";
    $mod .= "i" if $flags->{i};
    $mod .= "m" if $flags->{perl5_m};
    $mod .= "s" if $flags->{perl5_s};
    $mod .= "x" if $flags->{perl5_x};
    $mod;
  }

  sub RMARE_wrap_re_with_mods {
    my($o,$re,$flags)=@_;
    my $mod = $o->RMARE_imsx($flags);
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
        my $v = LET($RXX::pos){
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
  FAIL() if $__end__ != $RXX::pos;
  @_=\'.$code1;
      $code2 .= ";\ngoto \&\$cn}";
    }
    for my $i (reverse(2..$#fs)) {
      $code0 .= "my \$f$i = \$fs[$i]; ";
      $code1 = \'sub {
  FAIL() if $__end__ != $RXX::pos;
  $RXX::pos = $__start__;
  @_=\'.$code1;
      $code2 .= ";\ngoto \&\$f$i}";
    }
    { my $i = 1;
      $code0 .= "my \$f$i = \$fs[$i]; ";
      $code1 = \'sub {
  $__end__ = $RXX::pos;
  $RXX::pos = $__start__;
  @_=\'.$code1;
      $code2 .= ";\ngoto \&\$f$i}";
    }
    my $code = $code0."
#line 2 \"IRx1::RxBaseClass RMARE_conj\"
\n subname \'<conj \'.(\$sub_id++).\">\" => sub {my \$cn = \$_[0];
  my \$__start__ = \$RXX::pos;
  my \$__end__ = undef;
  my \$__f__ = ".$code1.$code2.\';
    LET($RXX::pos){
      $f0->($__f__);
    }LET;
  \'."}\n";
    #print STDERR $code;
    # Currently the macros are expanded in the string itself. :/
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
        if( !($repeat_previous_pos{$rid} < $RXX::pos) ||
            !($repeat_count{$rid} < $max))
        {
          TAILCALL($c,$noop);
        }
        local $repeat_previous_pos{$rid} = $RXX::pos;
        local $repeat_count{$rid} = $repeat_count{$rid} +1;
        
        my $v = LET($RXX::pos){
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
      my $nd = $RXX::nested_data;
      my $close = subname \'<capture-close \'.($myid).">" => sub {
        my($c)=@_;
        local $RXX::nested_data = $nd;
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

      my $from = $RXX::pos;

      my $m = $RXX::alias_match;
      local $RXX::alias_match;

      my $close = subname \'<capture_string-close \'.($myid).">" => sub {
        my $c0 = $_[0];
        my $to = $RXX::pos;
        $m->match_set(1,substr($RXX::str,$from,$to-$from),$m->{match_array},$m->{match_hash},$from,$to);
        TAILCALL($c0,$c);
      };

      $f->($close);
    };
  }

  sub RMARE_capture {
    my($o,$idx,$f,$is6,$nparen6,$in_quant,$target_spec)=@_;
    my $myid = $sub_id++;
    my $foo = subname \'<capture \'.($myid).">" => sub {
      my($c)=@_;

      my $from = $RXX::pos;

      my $m = $RXX::alias_match;
      local $RXX::alias_match;

      my $nd = $RXX::nested_data;

      my $leaf = $RXX::leaf_match;
      local $RXX::leaf_match = $is6 ? $m : $leaf;

      my $close = subname \'<capture-close \'.($myid).">" => sub {
        my $c0 = $_[0];
        local $RXX::nested_data = $nd;
        local $RXX::leaf_match = $leaf;
        my $to = $RXX::pos;
        $m->match_set(1,substr($RXX::str,$from,$to-$from),$m->{match_array},$m->{match_hash},$from,$to);
        my $v = eval { $c0->($c) };
        if($@) {
          die "jump ".$@ if $@ =~ /^fail /;
          die $@;
        }
        return $v;
      };

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

    my $spec = $target_spec ? [@$target_spec] : [\'$/\',\'[\'=>$idx];
    my $root = shift(@$spec);
    my $localize = \'$RXX::leaf_match\'; # root match
    my($copy,$access);
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
subname "<alias_wrap ".($sub_id++).">" => sub {
  my($c)=@_;

  my $m = $RXX::alias_match;
  local $RXX::alias_match;

  if(1 || !defined($m)){#XXXXX Punt multi-stage aliasing for now.
    $m = RXZ::Match0->new_failed();
    if($is6) {
      my $a = [map{RXZ::Match0->new_failed()} (1..$nparen6)];
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
    local $RXX::alias_match = $m;
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

      my $pkg0 = $RXX::pkg;
      my $pkg2 = $pkg_override || $pkg0;
      my $pkg9 = $pkg_override || $RXX::pkg || $pkg;

      my $pos = $RXX::pos;
      my $m0 = $RXX::current_match;
      my $m0b = $RXX::leaf_match;

      my $nd = $RXX::nested_data;

      my $m1 = $RXX::alias_match;
      local $RXX::alias_match;

      if(defined($m1)) {
        $m1->match_set(1,"",$m1->{match_array},$m1->{match_hash},$pos,undef);
      } else {
        $m1 = RXZ::Match0->new_failed();
        $m1->match_set(1,"",[],{},$pos,undef);
      }

      $m1->{match_rule} ||= $name; #EEEP

      my $close = subname "<subrule-close ".($myid)." $name>" => sub {
	my $cn = $_[0];

        local $RXX::nested_data = $nd;

	$m1->{match_to} = $RXX::pos; #EEEP
	$m1->{match_string} = substr($RXX::str,$pos,$RXX::pos-$pos);

        my $post = $name."__post_action";
        if(my $meth = UNIVERSAL::can($pkg9,$post)) {
          $m1->_prepare_match_for_embedded_code;
          $meth->($pkg9,$m1);
        }

	local $RXX::current_match = $m0;
	local $RXX::leaf_match = $m0b;
	local $RXX::pkg = $pkg0;

        #( subrule outake 1 )
        $neg ? 1 : $cn->($c);
      };

      my $v;
      { local $RXX::current_match = $m1;
        local $RXX::leaf_match = $m1;
        local $RXX::pkg = $pkg2;
        local $RXX::nested_data = {%$RXX::nested_data,"args"=>$args};
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
          #( subrule outake 2 )
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

  sub RMARE_wrap_foreign_method {
    my($o,$meth,$pkg9,$name)=@_;
    my $noop = $o->RMARE_noop;
    subname "<wrap_foreign_method ".($sub_id++).">" => sub {      
      my($c)=@_;
      my @args = @{$RXX::nested_data->{args}};
      my $m = $RXX::leaf_match; # set by subrule
      my $result = $meth->($pkg9,@args);
      my $result_is_Match = UNIVERSAL::isa($result,"Match");
      my $failed = $result_is_Match ? !$result->match_boolean() : !$result;
      if($failed) {
        $m->match_set_as_failed();
        FAIL();
      }
      if($result_is_Match) {
        my $from = $result->{match_from}; # may be different than $m\'s from;
        my $to = $result->{match_to};
        my $str = $result->{match_string}; #X
        $m->match_set(1,$str,$result->{match_array},$result->{match_hash},$from,$to);
      }
      else {
        my $from = $m->{match_from};
        my $to = $RXX::pos;
        my $str = substr($RXX::str,$from,$to-$from);
        $m->match_set(1,$str,[],{},$from,$to);
      }
      TAILCALL($c,$noop);
    };
  }

  sub RMARE_aregex_create {
    my($o,$f,$nparenx)=@_;
    $nparenx = 0 if !defined $nparenx;
    subname "<aregex ".($sub_id++).">" => sub {
      my($c)=@_;

      my $m = $RXX::leaf_match;
      my $a = [map{RXZ::Match0->new_failed()} (1..$nparenx)];
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
    my($o,$f,$s,$beginat,$scanp,$minlen,$nparen)=@_;
    my $len = length($s);
    $beginat = 0 if !defined($beginat);
    my $noop = $o->RMARE_noop;
    my $atend = $noop;
    if(defined $minlen) {
      my $min_end = $minlen + $beginat;
      $atend = subname "<atend ".($sub_id++).">" => sub {return undef if $RXX::pos < $min_end;return 1;}
    }
    for my $start ($beginat..$len) {
      local $RXX::str = $s;
      local $RXX::pos = $start;
      my $m = RXZ::Match0->new_failed();
      local $RXX::current_match = $m;
      local $RXX::leaf_match = $m;
      local $RXX::nested_data = {"args"=>[]};
      local $RXX::alias_match;
      
      my $ok = eval { $f->($atend) }; #try
      if($@) {
        die $@ unless ($@ eq "fail match\n" || $@ eq "fail regex\n" ||
                       $@ eq "fail group\n" || $@ eq "fail sequence\n");
        last;
      }
      if(not FAILED($ok)) {
        $m->match_set(1,substr($RXX::str,$start,$RXX::pos-$start),$m->{match_array},$m->{match_hash},$start,$RXX::pos);
        return $m;
      }
      last if $scanp;
    }
    return RXZ::Match0->new_failed();
  }

  sub RMARE_aregex {
    my($o,$pkg,$name,$f,$nparen,$prefix_re)=@_;
    # Why the extra sub?  60+% shorter re_text runtime.  sigh.
    my $matchergen = subname "even with subname used?" => sub {
      subname "<an aregex-matcher for $o>" => sub {
        my($pkg9,$name1,$s,$beginat,$scanp,$minlen)=@_;
        local $RXX::pkg = $pkg9;
        my $m = $o->RMARE_do_match($f,$s,$beginat,$scanp,$minlen,$nparen);
        $m->_prepare_match_for_return;
        $m->{match_rule} = $name1;
        if($name1) {
          my $post = $name1."__post_action";
          if(my $meth = UNIVERSAL::can($pkg9,$post)) {
            $m->_prepare_match_for_embedded_code;
            $meth->($pkg9,$m);
          }
        }
        $m;
      }
    };
    Regexp::ModuleA::Rx->_new_from_ast($o,$pkg,$name,$f,$matchergen,$prefix_re);
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
      #my(@args)=@_; # Args provided to permit compilation or memoization - ignored.
      my $pkg9 = ($pkg_override ||
                  $RXX::pkg ||
                  $pkg);
      die "assert" if !defined $pkg9;
      no strict;
      my $f;
      eval {
        my $meth = UNIVERSAL::can($pkg9,$name);
        if(ref($meth) eq "Regexp::ModuleA::Rx") {
          $f = $meth->($pkg9,$name)->(\' api0\');
        } elsif(!defined $meth) {
          $f = $pkg9->$name($name)->(\' api0\'); #invoke any AUTOLOAD
        } else {
          $f = IRx1::RxBaseClass->RMARE_wrap_foreign_method($meth,$pkg9,$name);
        }
      };
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
          { local($RXX::pos)=($RXX::pos);
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
          { local($RXX::pos)=($RXX::pos);
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
    my $pos = $RXX::pos;
    local $RXX::pos = $RXX::pos;
    my $at_pos = sub{ FAIL() if $RXX::pos != $pos; return 1;};
    for(my $i = $RXX::pos;$i>=0;$i--) {
      $RXX::pos = $i;
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
        my $a = $RXX::current_match->match_array;
        FAIL() if $idx > @$a;
        my $m = $a->[$idx-1];
        FAIL() if !$m->match_boolean;
        TAILCALL($c,$noop);
      };
    }
    subname "<conditional>" => sub {
      my $c = $_[0];
      my $v;
      { local($RXX::pos)=($RXX::pos);
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
    my($o,$code,$dont_rewrite)=@_;
    my $noop = $o->RMARE_noop;
    my $need_match;
    if($dont_rewrite) {
      $need_match = $code =~ /\$M\b/;
    } else {
      $code = "\'\'" if $code =~ /\A\s*\z/; #YYY XXX Why?
      my $tmp = _rewrite_matchvars($o,$code);
      $need_match = $code ne $tmp || $code =~ /\$M\b/;
      $code = $tmp;
    }
    my $src = \'
#line 2 "in Regexp::ModuleA::Code"
sub{my $__c__ = $_[0];
\'.(!$need_match ? \'\' :
\'  my $M = $RXX::current_match;
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
 \'  my $M = $RXX::current_match;
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

  # <?{ ... }>
  # NOT TESTED by the rx_on_re test suite.
  sub RMARE_codepredicate {
    my($o,$pred)=@_;
    my $noop = $o->RMARE_noop;
    my $src = \'
#line 2 "generated_by_RMARE_codepredicate"
    subname "<codepredicate>" => sub {
      my $__c__ = $_[0];
      FAIL() if !$pred->($__c__);
      $__c__->($noop)
    }\';
    #print STDERR $src,"\n";
    eval($src) || die "Error compiling <?{$code}> :\n$@\n";
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

  sub RMARE_category {
    my($o,$pkg,$categoryinfo_method)=@_;
    subname "<category ".($sub_id++).">" => sub {
      my $c = $_[0];
      my $obj = $pkg->$categoryinfo_method;
      my $info = $obj->info;
      if(!$info){ FAIL() }
      my $rest = substr($RXX::str,$RXX::pos);
      for my $branch (@$info) {
        my($prefix,$name,$rulename,$rx_sub) = @$branch;
        if($rest !~ $prefix) { next }
        my $f = $rx_sub->(" api0");
        my $v = $f->($c);
        if(FAILED($v)) { next }
        $RXX::alias_match->{match_rule} = $rulename; #XX kludge

        my $pkg_override = undef;
        my $pkg9 = $pkg_override || $RXX::pkg || $pkg;
        my $m = $RXX::alias_match;
        my $post = $name."__post_action";
        if(my $meth = UNIVERSAL::can($pkg9,$post)) {
          $m->_prepare_match_for_embedded_code;
          $meth->($pkg9,$m);
        }

        return $v;
      }
      FAIL();
    };
  }

{ package CategoryInfo;
  use Class::Inspector;
  sub newp {
    my($cls,$pkg,$filter)=@_;
    bless {
      pkg => $pkg,
      filter => $filter
    }, $cls;
  }
  sub info {
    my($o)=@_;
    $o->{info} ||= $o->_gather_info($o->{pkg},$o->{filter});
  }
  sub cache_invalidate {
    my($o)=@_;
    $o->{info} = undef;
  }
  sub _gather_info {
    my($o,$pkg,$filter)=@_;
    my $methods = Class::Inspector->methods($pkg) || [];
    my @names = grep(/$filter/, @{$methods});
    my @data = map {
      my $name = $_;
      my $rx_sub = UNIVERSAL::can($pkg,$name);
      if(!$rx_sub){die "bug: $pkg  $name"}
      my $prefix = $rx_sub->(\' hash\'){"prefix_re"}; #Note: reaching inside Rx;
      my $rulename = $rx_sub->(\' hash\'){"name"}; #Note: reaching inside Rx;
      [$prefix,$name,$rulename,$rx_sub]
    } @names;
    @data = sort { length($b->[0]) <=> length($a->[0]) } @data;
    #print Data::Dumper::Dumper($methods,$filter,\@names,\@data);
    [ map {$_} @data];  
  }
}

  sub RMARE_return_current_match {
    my($o)=@_;
    my $noop = $o->RMARE_noop;
    subname "<return_current_match ".($sub_id++).">" => sub {
      my $c = $_[0];
      my $v = $c->($noop);
      FAIL_IF_FAILED($v);
      my $current_match = $RXX::current_match;
      return $v if FAILED($current_match); #X die instead?
      return $current_match;
    };
  }
  our $our_return_current_match = __PACKAGE__->RMARE_return_current_match;

  sub RMARE_return_pos1 { # pos+1 because 0 is FAIL.
    my($o)=@_;
    my $noop = $o->RMARE_noop;
    subname "<return_current_match ".($sub_id++).">" => sub {
      my $c = $_[0];
      my $v = $c->($noop);
      FAIL_IF_FAILED($v);
      return ($RXX::pos +1);
    };
  }
  our $our_return_pos1 = __PACKAGE__->RMARE_return_pos1;

  #----------------------------------------------------------------------
  # RATCHET
  # These are all _compile time_, unlike most RMARE_foo.
  # They could/should be p6 instead of p5.
  # state: $__str__, pos($__str__), $__ok__
  # on fail: leave pos unchanged, false $__ok__.
  # differs from RMARE: $RXX::pos is unspecified.
  #   Why?  More for code clarity than performance.  LETs and such.
  #----------------------------------------------------------------------

  sub RATCHET_wrap_for_RMARE {
    my($o,$src)=@_;
    ("do{Sub::Name::subname \"<ratchet ".($sub_id++).">\" => sub { my \$__c__ = \$_[0];\n".
     "  my \$__str__ = \$RXX::str;\n".
     "  pos(\$__str__) = \$RXX::pos;\n".
     "  my \$__ok__=1;\n".
     $src."\n".
     "  FAIL() if !\$__ok__;\n".
     "  local \$RXX::pos = pos(\$__str__);\n".
     "  \$__c__->(\$IRx1::RxBaseClass::our_noop);\n". #X tailcall?
     "}}")
  }
  sub RATCHET_eat_regexp {
    my($o,$re)=@_;
    my $gre = \'\G\'."(?:$re)";
    "\$__str__ =~ /$gre/gc or do{\$__ok__=undef};"
  }
  sub RATCHET_alt {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne "ARRAY";
    return "" if @$aref == 0; #XX or fail?
    my @srcs = @$aref;
    my $src_last = pop(@srcs);
    my $code = "# alt\nwhile(1){\n";
    for my $src (@srcs) {
      $code .= $src."\n"."if(\$__ok__){last}else{\$__ok__=1};\n";
    }
    $code .= $src_last."\n";
    $code .= "last;\n}\n";
    $code;
  }
  sub RATCHET_concat {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne "ARRAY";
    my @srcs = @$aref;
    return "" if @srcs == 0;
    return $srcs[0] if @srcs == 1;
    my $code = "# concat\n{ my \$__old_pos = pos(\$__str__);\n";
    for my $src (@srcs) { #X could nest
      $code .= "if(\$__ok__) {\n".$src."\n}\n";
      #$code .= \'print STDERR "\n-------------\n",Data::Dumper::Dumper($RXX::current_match,$RXX::leaf_match,$RXX::alias_match),"\n";\'."\n";
    }
    $code .= "if(!\$__ok__) { pos(\$__str__) = \$__old_pos }\n}\n";
    $code;
  }
  sub RATCHET_repeat {
    my($o,$r,$min,$max,$ng)=@_;
    my $mins = "".($min||0);
    my $maxs = defined($max) ? $max : "10**10**10";
    return "" if $ng && ($min||0) == 0;
    my $code = ("# repeat\n{ my \$__old_pos = pos(\$__str__); ".
                " my \$__i = 0;\n".
                " my \$__min = $mins;\n".
                " my \$__max = $maxs;\n".
                " while(\$__i < \$__max && \$__ok__) {\n".$r."\n");
    if($ng) {
      $code .=  "  last if \$__i == \$__min || !\$__ok__;\n";
    } else {
      $code .=  "  if(!\$__ok__) { \$__ok__=1 if \$__i >= \$__min; last; }\n";
    }
    $code .=   (" }\n pos(\$__str__) = \$__old_pos if !\$__ok__;\n".
                "}\n");
    $code;
  }
  sub RATCHET_wrap_subrule {
    my($o,$subrule_var)=@_;
    ("{ my \$__old_pos = pos(\$__str__);\n".
     "  local \$RXX::pos = \$__old_pos;\n".
     "  my \$__v = do{ $subrule_var }->(\$IRx1::RxBaseClass::our_return_pos1);\n".
     "  if(FAILED(\$__v)) { \$__ok__=undef; pos(\$__str__)=\$__old_pos; }\n".
     "  else { pos(\$__str__) = \$__v-1 }\n".
     "}\n")
  }

}
');
   my $rx = '
#line 1 "regex_prelude-rx"
#======================================================================
# Rx
#

package Regexp::ModuleA::Rx;
use Sub::Name;

sub _new_from_ast {
  my($rxclass,$ast,$pkg,$name,$f,$matchergen,$prefix_re)=@_;
  $pkg ||= "";
  $name ||= "";
  $prefix_re ||= qr/\A/;
  my $h = {
    ast=>$ast,
    pkg=>$pkg,
    name=>$name,
    f=>$f,
    matchergen=>$matchergen,
    prefix_re=>$prefix_re
  };
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
sub _init { #XXX only used in remains_of_Regexp_ModuleA.pm.  should go away in Rx cleanup.
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
sub scan {
  my($o,$str,$beginat)=@_;
  $beginat ||= 0;
  $o->(\' match\',$str,$beginat,1);
}

#Commented out until someone needs them.
#sub ast {shift->(\' hash\')->{ast}}
#sub name {shift->(\' hash\')->{name}}

';

   my $match = '
#line 1 "regexrx-match"
#======================================================================
# Match
#

{ package RXZ::Match2;
  @RXZ::Match2::ISA = qw(Match);

  sub _prepare_match_for_return { }
  sub _prepare_match_for_embedded_code { }

}
{ package RXZ::Match1;
  @RXZ::Match1::ISA =
      qw(Match RXZ::Match_internal);

  sub _prepare_match_for_embedded_code { }

  use overload \'bool\' => \'match_boolean\', ;

}
{ package RXZ::Match0;
  @RXZ::Match0::ISA =
      qw(Match RXZ::Match_internal);

  sub _prepare_match_for_embedded_code {
    my($o)=@_;
    bless $o, \'RXZ::Match1\';
  }

  sub match_new0 {
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
  sub new_failed {my($cls)=@_; $cls->match_new0()->match_set_as_failed()}

}
{ package Any;
  sub _prepare_match_for_return {}
  sub _prepare_match_for_embedded_code {}
}
{ package RXZ::Match_internal;

  sub _prepare_match_for_return {
    my($o)=@_;
    use Carp; Carp::confess if ref($o) !~ /[a-z]/;
    for my $m (map{ref($_)eq\'ARRAY\'?@$_:$_}@{$o->match_array}) { $m->_prepare_match_for_return }
    for my $m (map{ref($_)eq\'ARRAY\'?@$_:$_}values %{$o->match_hash}) { $m->_prepare_match_for_return }
    bless $o, \'RXZ::Match2\';
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
    my $prelude_regexen = '
{ package Any;
  IRx1::RxBaseClass->RMARE_biind(__PACKAGE__,  "before",
    IRx1::RxBaseClass->RMARE_aregex(undef,undef,
      IRx1::RxBaseClass->RMARE_aregex_create(
        IRx1::RxBaseClass->RMARE_lookaround(1,1,
          IRx1::RxBaseClass->RMARE_coderx(
            \'$RXX::nested_data->{args}[0]||qr/(?!)/\'))
      ,undef),
    undef));
  IRx1::RxBaseClass->RMARE_biind(__PACKAGE__,  "after",
    IRx1::RxBaseClass->RMARE_aregex(undef,undef,
      IRx1::RxBaseClass->RMARE_aregex_create(
        IRx1::RxBaseClass->RMARE_lookaround(0,1,
          IRx1::RxBaseClass->RMARE_coderx(
            \'$RXX::nested_data->{args}[0]||qr/(?!)/\'))
      ,undef),
    undef));
  IRx1::RxBaseClass->RMARE_biind(__PACKAGE__,  "commit",
    IRx1::RxBaseClass->RMARE_aregex(undef,undef,
      IRx1::RxBaseClass->RMARE_aregex_create(
        IRx1::RxBaseClass->RMARE_commit_match()
      ,undef),
    undef));
}
';
    my $cruft = '
{ package GLOBAL;
  our $_cursor;
  sub parser_pos { $RXX::pos }
}
';
    $rmare ~ "\n" ~ $match ~ "\n" ~ $rx ~ $prelude_regexen ~ $cruft;
  };
};

eval_perl5( EmitRegex.regex_prelude() );


#----------------------------------------------------------------------
# AST to RMARE emitters
#----------------------------------------------------------------------

package IRx1 {

  class RxBaseClass {
    method emit_RMARE_optimized {
      if ($.notes<flags><ratchet> &&
          $.notes<equivalent_re> &&
          not($.notes<match_written>))
      {
        my $re = $.notes<equivalent_re>;
        'IRx1::RxBaseClass->RMARE_eat_regexp("'~quotemeta($re)~'")';
      }
      else { undef }
    }
    method emit_RATCHET { undef }

    our $gensym_counter_ = 0; #X STD_red misparses $_gen .
    method gensym {
      $gensym_counter_ = $gensym_counter_+1;
      "gensym"~$gensym_counter_
    }

  }

  # space - may be a no-op, literal, or <ws>.
  class RxASpace {
    method emit_RMARE () {
      $.notes<delegate>.emit_RMARE;
    }
    method emit_RATCHET {
      $.notes<delegate>.emit_RATCHET;
    }
  }

  # any regexp
  class RxPat5 {
    method _as_re {
      $.RMARE_wrap_re_with_mods($.pat,$.notes<flags>);
    }
    method emit_RMARE () {
      'IRx1::RxBaseClass->RMARE_eat_regexp("'~quotemeta($._as_re)~'")';
    }
    method emit_RATCHET {
      $.RATCHET_eat_regexp($._as_re);
    }
  }

  # \Qabc\E
  class RxExact {
    method _as_re {
      my $re = $.text;
      $re.re_sub('([^\w\s])','\\\\$1','g');
      $.RMARE_wrap_re_with_mods($re,$.notes<flags>);
    }
    method emit_RMARE () {
      'IRx1::RxBaseClass->RMARE_eat_regexp("'~quotemeta($._as_re)~'")';
    }
    method emit_RATCHET {
      $.RATCHET_eat_regexp($._as_re);
    }
  }

  # (?imsx-imsx:...)
  class RxMod_expr {
    method emit_RMARE () {
      $.expr.emit_RMARE;
    }
    method emit_RATCHET {
      $.expr.emit_RATCHET;
    }
  }

  # (?imsx-imsx)
  class RxMod_inline {
    method emit_RMARE () {
      'IRx1::RxBaseClass->RMARE_noop()';
    }
    method emit_RATCHET {
      "";
    }
  }

  # ? * + {n,m} ?? *? etc
  class RxQuant {
    method emit_RMARE () {
      my $min = $.min;
      my $max = $.max;
      my $nongreedy = 0;
      if $.nongreedy { $nongreedy = 1 }
      $min = 0 if !defined $min;
      my $maxs = {if defined($max) { ""~$max } else { '1000**1000**1000' }};
      if $maxs eq 'inf' { $maxs = '1000**1000**1000' }
      $max = 1000**1000**1000 if !defined $max; #XXX inf
      die "assert - Quant min <= max" if $min > $max;
      my $f = $.expr.emit_RMARE;
      if $.notes<flags><ratchet> {
        'IRx1::RxBaseClass->RMARE_concat([IRx1::RxBaseClass->RMARE_repeat('~$f~','~$min~','~$maxs~','~$nongreedy~'), IRx1::RxBaseClass->RMARE_commit_sequence])';
      } else {
        'IRx1::RxBaseClass->RMARE_repeat('~$f~','~$min~','~$maxs~','~$nongreedy~')';
      }
    }
    method emit_RATCHET {
      if $.notes<flags><ratchet> {
        my $r = $.expr.emit_RATCHET;
        return undef if !defined($r);
        $.RATCHET_repeat($r,$.min||0,$.max,$.nongreedy);
      }
      else { undef }
    }
  }

  # a|b
  class RxAlt {
    method emit_RMARE {
      my $exprs = $.exprs.map(sub ($o){$o.emit_RMARE}).join(',');
      if $.notes<flags><ratchet> {
        'IRx1::RxBaseClass->RMARE_concat([IRx1::RxBaseClass->RMARE_alt(['~$exprs~']),
                        IRx1::RxBaseClass->RMARE_commit_sequence()])';
      } else {
        'IRx1::RxBaseClass->RMARE_alt(['~$exprs~'])';
      }
    }
    method emit_RATCHET {
      if $.notes<flags><ratchet> {
        my $ok = 1;
        my $exprs = $.exprs.map(sub ($o){my $r = $o.emit_RATCHET; $ok=0 if !defined($r); $r});
        return undef if !$ok;
        $.RATCHET_alt($exprs);
      }
      else { undef }
    }
  }

  # a&b
  class RxConj {
    method emit_RMARE {
      my $exprs = $.exprs.map(sub ($o){$o.emit_RMARE}).join(',');
      'IRx1::RxBaseClass->RMARE_conj(['~$exprs~'])';
    }
  }

  # ab
  class RxSeq {
    method emit_RMARE {
      if ($.notes<flags><ratchet> && $.exprs.elems > 1 &&
          %*ENV<ELF_PRAGMA_RXRATCHET>)
      { 
        #XX calling emit_RATCHET and then emit_RMARE is exponential.  cache?
        my $es = $._RATCHET_to_RMARE.join(',');
        'IRx1::RxBaseClass->RMARE_concat(['~$es~'])';
      }
      else {
        my $es = $.exprs.map(sub ($o){$o.emit_RMARE}).join(',');
        'IRx1::RxBaseClass->RMARE_concat(['~$es~'])';
      }
    }
    method emit_RATCHET {
      if $.notes<flags><ratchet> {
        my $ok = 1;
        my $exprs = $.exprs.map(sub ($o){my $r = $o.emit_RATCHET; $ok=0 if !defined($r); $r});
        return undef if !$ok;
        $.RATCHET_concat($exprs);
      }
      else { undef }
    }
    method _RATCHET_to_RMARE { #XX painful varnames
      my $partition = [];

      my $tmp = [0];
      my $subrules = "";
      my $doing = sub ($n) {
        my $tmp0 = $tmp[0];
        if $tmp0 == $n { }
        elsif $tmp0 == 0 { $tmp[0] = $n; }
        elsif $tmp0 == 1 { $partition.push($tmp); $tmp = [$n]; }
        elsif $tmp0 == 2 { $tmp.push($subrules); $subrules = "";
                           $partition.push($tmp); $tmp = [$n]; }
        else { die "bug" }
      };
      my $exprs = $.exprs.clone;
      while $exprs.elems {
        my $e = $exprs.shift;
        if $e.WHAT eq 'IRx1::RxSeq' { $exprs.unshift($e.exprs.flatten); next } ;#X
        my $r = $e.emit_RATCHET;
        if defined($r) {
          $doing.(2);
          $tmp.push($r);
        }
        elsif $e.WHAT eq 'IRx1::RxSubrule' {
          $doing.(2);
          my $var = '$'~$.gensym;
          my $er = $e.emit_RMARE;
          $subrules = $subrules~" my "~$var~" = "~$er~";\n";
          $r = $.RATCHET_wrap_subrule($var);
          $tmp.push($r);
        }
        else {
          $doing.(1);
          my $em = $e.emit_RMARE;
          $tmp.push($em);
        }
      }
      $doing.(-42);

      my $exprs_RMARE = [];
      for $partition {
        my $tmp0 = $_.shift;
        if $tmp0 == 1 { 
          $exprs_RMARE.push($_.flatten);
        }
        else {
          my $subrules = $_.pop;
          my $ratch = $.RATCHET_concat($_);
          my $code = $.RATCHET_wrap_for_RMARE($ratch);
          if $subrules {
            $code = "(do{\n"~$subrules~$code~"})";
          }
          $exprs_RMARE.push($code);
        }
      }
      $exprs_RMARE;
    }
  }  

  # .. = ...
  # .. := ... (formerly)
  class RxAlias {
    method emit_RMARE {
      my $target_spec = $.notes<target_spec>;
      my $construct_kind = $.notes<construct_kind>;
      my $construct_in_quant = $.notes<construct_in_quant>;
      my $f = $.expr.emit_RMARE;
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

  # (a)
  class RxCap {
    method emit_RMARE {
      my $in_quant = {if $.notes<in_quant> { 1 } else { 0 }};
      my $target_spec = $.notes<target_spec>;
      my $is6_ = not $.notes<flags><p5>;
      my $is6 = $is6_ || 'undef';
      my $idx = $.notes<cap5_idx>;
      if $is6_ { $idx = $.notes<cap6_idx> };
      my $nparen6 = $.notes<nparen6>;
      my $f = $.expr.emit_RMARE;
      'IRx1::RxBaseClass->RMARE_capture('~$idx~','~$f~','~$is6~','~$nparen6~','~$in_quant~','~$target_spec.perl~')';
    }
  }

  # (?:a)
  class RxGrp {
    method emit_RMARE {
      my $target_spec = $.notes<target_spec>;
      my $in_quant = {if $.notes<in_quant> { 1 } else { 0 }};
      my $f = $.expr.emit_RMARE;
      'IRx1::RxBaseClass->RMARE_group('~$f~','~$target_spec.perl~','~$in_quant~')';
    }
    method emit_RATCHET {
      if $.notes<flags><ratchet> {
        $.expr.emit_RATCHET;
      }
      else { undef }
    }
  }

  # \1
  class RxBackref {
    method emit_RMARE {
      my $noop = $.RMARE_noop;
      my $idx = $.backref_n;
      'IRx1::RxBaseClass->RMARE_eat_backref('~$idx~',"'~quotemeta('(?'~$.RMARE_imsx($.notes<flags>)~')')~'")';
    } #XXX move imsx into eat
  }

  # <foo>
  class RxSubrule {
    method emit_RMARE {
      my $exprs = $.exprs.map(sub ($o){$o.emit_RMARE}).join(',');
      my $pkg = {if $.notes<pkg> {'"'~quotemeta($.notes<pkg>)~'"'} else {'__PACKAGE__'}};
      my $name = {if $.name {
          if $whiteboard::current_emitter {
            '"'~quotemeta($whiteboard::current_emitter.mangle_function_name($.name))~'"'
          } else { # for testing
            '"'~quotemeta($.name)~'"'
          }
        } else {'undef'}};
      my $neg = $.neg ||'undef';
      my $nocap = $.nocap ||'undef';
      my $in_quant = {if $.notes<in_quant> { 1 } else { 0 }};
      my $pkg_override = 'undef';
      my $g = $.name.re_groups('^([\w\:\.]+)\.(\w+)$');
      if $g {
        $name = '"'~quotemeta($g[1])~'"';
        $pkg_override = '"'~quotemeta($g[0])~'"';
      }
      my $target_spec = $.notes<target_spec>;
      'IRx1::RxBaseClass->RMARE_subrule_fetching_rx('~$pkg~','~$pkg_override~','~$name~',['~$exprs~'],'~$neg~','~$nocap~','~$in_quant~','~$target_spec.perl~')';
    }
  }

  # (?(n)t|f)
  class RxConditional {
    method emit_RMARE {
      my $f_test = 'undef';
      my $idx = 'undef';
      my $f_else = 'undef';
      my $f_then = $.expr_then.emit_RMARE;
      if $.expr_else {
        $f_else = $.expr_else.emit_RMARE;
      }
      if $.test.isa("Int") {
        $idx = $.test;
      } else {
        $f_test = $.test.emit_RMARE;
      }
      'IRx1::RxBaseClass->RMARE_conditional('~$f_test~','~$idx~','~$f_then~','~$f_else~')';
    }
  }

  # (?=) (?<=) (?!) (?<!)
  class RxLookaround {
    method emit_RMARE {
      my $f = $.expr.emit_RMARE;
      my $is_forward = 0; if $.is_forward { $is_forward = 1 };
      my $is_positive = 0; if $.is_positive { $is_positive = 1 };
      'IRx1::RxBaseClass->RMARE_lookaround('~$is_forward~','~$is_positive~','~$f~')';
    }
  }

  # (?>)
  class RxIndependent {
    method emit_RMARE {
      my $f = $.expr.emit_RMARE;
      'IRx1::RxBaseClass->RMARE_independent('~$f~')'
    }
  }

  # nonexistent
  class RxCommitSequence {
    method emit_RMARE {
      'IRx1::RxBaseClass->RMARE_commit_sequence'
    }
    method emit_RATCHET {
      if $.notes<flags><ratchet> {
        "";
      }
      else { undef }
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
  # A Str is considered p5, an IR node as p6.
  class RxCode {
    method emit_RMARE {
      my $code = $.code;
      if $code.WHAT eq 'Str' {
        'IRx1::RxBaseClass->RMARE_code("'~quotemeta($code)~'")'
      } else {
        my $src = $whiteboard::current_emitter.e($code);
        say $whiteboard::current_emitter if 0; #X avoid "used only once".
        'IRx1::RxBaseClass->RMARE_code("'~quotemeta($src)~'",1)'
      }
    }
  }

  # (??{ ... })
  # XXX high klude factor
  # Code is currently p5!
  class RxCodeRx {
    method emit_RMARE {
      my $code = $.code;
      if $code.WHAT ne 'Str' {
        $code = $whiteboard::current_emitter.e($code);
      }
      'IRx1::RxBaseClass->RMARE_coderx("'~quotemeta($code)~'")'
    }
  }

  # <?{ ... }>
  # NOT TESTED by the rx_on_re test suite.
  class RxCodePredicate {
    method emit_RMARE {
      my $code = $.code;
      my $code5 = $whiteboard::current_emitter.e($code);
      if 1 || $code.uses_match_variable { #XX unimplemented. performance hit.
        $code5 = 'my $M = $RXX::current_match;
        $M->_prepare_match_for_embedded_code;'~"\n" ~ $code5;
      }
      #XX need to subname the sub to avoid p5 idiocy?
      'IRx1::RxBaseClass->RMARE_codepredicate(sub{'~$code5~'})'
    }
  }

  # rx/a/
  class RxARegex {
    method RMARE_emit_and_eval {
      my $src = $.emit_RMARE;
      eval_perl5($src);
    }
    method emit_RMARE {
      my $pkg = {if $.notes<pkg> {'"'~quotemeta($.notes<pkg>)~'"'} else {'undef'}};
      my $prefix_re = 'undef';
      my $nameq = 'undef';
      if $.notes<name> {
        my $name = $.notes<name>;
        $nameq = '"'~quotemeta($name)~'"';
        my $g;
        if ($g = $name.re_groups('[^:]:([^:]+)\z')) { #XX kludge
          $prefix_re = 'qr/'~quotemeta($g[0])~'/';
        }
      }
      my $nparenx = {if $.notes<flags><p5> { $.notes<nparen> } else { $.notes<nparen6> }};
      $nparenx = $nparenx || 'undef';
      my $nparen = $.notes<nparen> ||'undef'; #||undef needed?
      my $expr = $.expr.emit_RMARE;
      my $opt = $.emit_RMARE_optimized;
      $expr = $opt if $opt; #XXX should be disabled by default;
      ('IRx1::RxBaseClass->RMARE_aregex('~$pkg~','~$nameq~
       ',IRx1::RxBaseClass->RMARE_aregex_create('~$expr~','~$nparenx~'),'~
       $nparen~','~$prefix_re~')');
    }
  }

  # regex foo /a/; rule foo /a/; token foo /a/
  class RxBiind {
    method RMARE_emit_and_eval {
      my $src = $.emit_RMARE;
      eval_perl5($src);
    }
    method emit_RMARE {
      my $pkg = {if $.notes<pkg> {'"'~quotemeta($.notes<pkg>)~'"'} else {'__PACKAGE__'}};
      my $name = {if $.name {'"'~quotemeta(mangle_name($.name))~'"'} else {'undef'}};
      my $fr = $.expr.emit_RMARE;
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
      my $pkg = $.pkg;
      '(do{ IRx1::RxBaseClass->RMARE_namespace("'~quotemeta($pkg)~'");'~
      '('~$.bindings.map(sub ($o){$o.emit_RMARE}).join(",\n")~') })';
    }
  }

  # proto token infix { <...> }
  # NOT TESTED by the rx_on_re test suite.
  class RxCategory {
    method emit_RMARE {
      my $name = $.name;
      my $categoryinfo_method = '__rxcategory_'~$name;
      my $decl = 'sub '~$categoryinfo_method~' { no strict; $'~$name~' ||= CategoryInfo->newp(__PACKAGE__,qr/\A'~quotemeta($name)~'_58(?!.*__post_action)/) };';
      $whiteboard::regex_category_header = $decl;
      'IRx1::RxBaseClass->RMARE_category(__PACKAGE__,\''~$categoryinfo_method~'\')'
    }
  }

}


class IRx1::RegexDef {
  method note_environment() {
    $.record_crnt_package;
    for $.child_nodes {$_.note_environment}
  }
}


class EmitSimpleP5 {
  method cb__RxARegex ($n) {
    $n.RAST_init.emit_RMARE;
  }
  method cb__RegexDef ($n) {
    my $ir = $n.<pattern>.RAST_init;
    #say $ir.irx1_describe;
    my $code = $ir.emit_RMARE;
    if $n.signature && $n.signature.return_type {
      my $type = $n.signature.return_type;
      my $pkg = $n.notes<crnt_package>;
      $type = $pkg~'::'~$type;
      my $name = mangle_name($n.ident);
      my $post = $name~"__post_action";
      $code = '(do{ sub '~$post~' {my($self,$m)=@_; '~$type~'->coerce($m) }; '~$code~' })';
    }
    $code;
  }
  method cb__RegexCategoryDecl ($n) {
    my $name = $n<name>;
    temp $whiteboard::regex_category_header;
    my $def = $.e($n<rx>);
    my $info = $whiteboard::regex_category_header;
    '# category '~$name~"\n"~$info~"\n"~$def;
  }
}

class CategoryDef {
}

class Match {
  method text { $.match_string } ;# for STD.pm
  method _REDUCE ($from,$rule) { # for STD.pm
    $.match_rule = $rule;
    $.match_boolean = 1;
    $.match_string = substr($RXX::str,$from,$RXX::pos-$from);
    $.match_from = $from;
    $.match_to = $RXX::pos;
    self;
  }
}

class FakeCursor {
  method pos { $RXX::pos }
  method advance_by_rule ($rulename) is p5 {'
    my $m = $self->$rulename()->scan($RXX::str,$RXX::pos);
    if($m->match_boolean) { $RXX::pos = $m->match_to; }
    $m;
  '}
  method new_Match is p5 {'
    RXZ::Match0->new_failed();
  '}
  method panic($msg) is p5 {'
no warnings;
print STDERR $msg,"\n";
print STDERR "pos: ",$RXX::pos,"\n";
Carp::croak if $ENV{VERBOSE};
exit(1);
'}
}
class STD is FakeCursor {
}


# since we're not faking $¢ yet.
package Undef {
  method panic($msg) is p5 {'
no warnings;
my $str = $RXX::str;
my $pos = $RXX::pos;
my $m1 = $RXX::current_match;
my $m2 = $RXX::leaf_match;
my $m3 = $RXX::alias_match;
my $pkg = $RXX::pkg;
my $data = $RXX::nested_data;
print STDERR $msg,"\n";
print STDERR "pos: ",$pos,"\n";
Carp::croak if $ENV{VERBOSE};
exit(1);
'}
}
