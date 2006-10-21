
package Regexp::Engine::Reentrant::Backtrack;

# Empty class to label noop subs. Private.
{ package Regexp::Engine::Reentrant::Backtrack::Noop; }

package Regexp::Engine::Reentrant::Backtrack::Make;
use strict;   #HMM - disable in the code evals?
use warnings; #HMM - disable in the code evals?

sub new {
  my $cls = shift;
  my @defaults = ( config_str_var => '$X::str',
		   config_pos_var => '$X::pos',
		   config_backtrack_vars => ['$X::pos'],
		   config_match_var => '$X::current_match',
		   config_cap_var => '$X::cap',
		   config_flag_var => '$X::flag'
		   );
  my %h = (@defaults,@_);
  bless \%h,$cls;
}

sub _eval_code {
  my($o,$code)=@_;
  my $noop = $o->sub_noop;
  my $res = eval($code);
  die $@.$code if $@;
  $res;
}
sub code_call {
  my($o,$f,@args)=@_;#HMM - $args?
  push(@args,'$noop') if !@args;
  $f.'->('.join(',',@args).')';
}
sub code_tailcall {
  my($o,$f,@args)=@_;#HMM - $args?
  push(@args,'$noop') if !@args;
  '@_=('.join(',',@args)."); goto \&{$f};";
}
sub code_tailcall_safely { # goto apparently sometimes kills lexical vars.
  my($o,$f,@args)=@_;#HMM - $args?
  push(@args,'$noop') if !@args;
  #$o->code_tailcall($f,@args);
  'return('.$o->code_call($f,@args).');';
}
sub code_noop_is {
  my($o,$var)=@_;
  "(ref($var) eq 'Regexp::Engine::Reentrant::Backtrack::Noop')";
}
sub code_fail {
  my($o,$var)=@_;
  $var = "undef" if not defined $var;
  "return($var)";
}
sub code_fail_is {
  my($o,$var)=@_;
  "(!defined($var) || (!ref($var) && $var <= 0))";
}
sub code_fail_isnot {
  my($o,$var)=@_;
  "(!".$o->code_fail_is($var).")";
}
sub code_fail_is_exception {
  my($o,$var)=@_;
  "(defined($var) && !ref($var) && $var <= 0)";
}
sub code_fail_propagate {
  my($o,$var)=@_;
  'if('.$o->code_fail_is($var).') {'.$o->code_fail($var).'}';
}
sub code_fail_propagate_exceptions {
  my($o,$var)=@_;
  'if('.$o->code_fail_is_exception($var).') {'.$o->code_fail($var).'}';
}

sub sub_noop {
  my($o)=@_;
  if(not exists $o->{cached_noop}) {
    my $noop;
    my $code = 'bless sub{
      my $c = $_[0];
      return 1 if '.$o->code_noop_is('$c').';
      '.$o->code_tailcall('$c','$noop').";
    }, 'Regexp::Engine::Reentrant::Backtrack::Noop';";
    $noop = $o->{cached_noop} = eval($code); die $@.$code if $@;
  }
  $o->{cached_noop};
}
sub sub_fail {
  my($o,@n)=@_;
  my $key = "cached_fail_".(@n?$n[0]:'undef');
  if(not exists $o->{$key}) {
    my $code = "sub{ ".$o->code_fail(@n)."; }";
    $o->{$key} = $o->_eval_code($code);
  }
  $o->{$key};
}

sub code_subwrap {
  my($o,$infosrc,$subsrc)=@_;
  return $subsrc;
  my $pkg = __PACKAGE__;
  "\&${pkg}::_subwrap($infosrc,$subsrc)";
}
sub _subwrap {
  my($info,$f)=@_;
  print STDERR $info," ",$f,"\n";
  $f;
}

sub genstr {
  sprintf("%x",int(rand(1_000_000_000)));
}
sub gensym {
  my($o,$stem)=@_;
  $stem = '$gensym_' if not defined $stem;
  $stem.$o->genstr;
}

sub source_temp_vars {
  my($o,$vars,$body)=@_;
  my $varl = join(",",@$vars);
  return ("(do{ $body })") if $varl eq "";
  return ("(do{ local($varl)=($varl); $body })");
}
sub source_let_vars {
  my($o,$vars,$body,@rest)=@_;
  $o->source_alt_vars($vars,[$body],@rest);
}
sub source_alt_vars {
  my($o,$vars,$bodies,$code_on_success,$code_on_failure,$code_on_each_failure)=@_;
  my $uniq = $o->genstr;
  my $v  = '$v_'.$uniq;
  my $ok = '$ok_'.$uniq;
  my $tmpstem = '$tmp'; my $tmpcnt = 0;
  my $tmpl = join(",",map{$tmpstem.($tmpcnt++).'_'.$uniq}@$vars);
  my $varl = join(",",@$vars);

  my $success = "";
  my $failure = "";
  my $eachfail = "";
  $success = "my \$_v_ = $v; $code_on_success" if defined $code_on_success;
  $failure = "{ my \$_v_ = $v; $code_on_failure }" if defined $code_on_failure;
  $eachfail = "my \$_v_ = $v; $code_on_each_failure" if defined $code_on_each_failure;
  #$failure is {}'ed to avoid conflicts with $eachfail's definition of $_v_.

  my $vars_setup = "my($tmpl)=($varl);";
  my $vars_reset = "($varl)=($tmpl);";
  my $vars_local = "local($varl)=($varl);";
  my $vars_save  = "($tmpl)=($varl);";
  my $vars_set   = "($varl)=($tmpl);";
  if($varl eq '') {
    $vars_setup = $vars_reset = $vars_local = $vars_save = $vars_set = "";
  }
  if(@$bodies == 0) { $bodies = ['undef'] }
  if(@$bodies == 1) {
    $vars_setup = "my($tmpl);";
    $vars_reset = "";
  }

  my $code = $failure;
  for my $body (reverse @$bodies) {
    $code = ("\n $v = do{ $body };"
            ." if(".$o->code_fail_is($v).") { $eachfail $vars_reset $code }"
            ." else {$ok = 1; $vars_save }");
  }  
  $code = "
  (do{
    my $v; my $ok; $vars_setup
    { $vars_local $code }
    if($ok){ $vars_set $success }
    $v
  })";
}  
sub source_concat {
  my($o,$bodies)=@_;
  join("",map{"do{ $_ };\n"}@$bodies);
}
sub source_repeat {
  my($o,$body,$min,$max,$nongreedy)=@_;
  $min = 0 if !defined $min;
  my $code = "";
  for (1..$min) {$code .= "{ $body }\n";}    
        
}


sub sub_temp {
  my($o,$f)=@_;
  my $key = 'cached_sub_temp';
  if(not exists $o->{$key}) {
    my $code = '
#line 2 "cached_sub_temp"
    sub {
      my($o,$f)=@_;
      '.$o->code_subwrap("'$key'",'sub {
        my $c = $_[0];
        '.$o->source_temp_vars($o->{config_backtrack_vars},
                               $o->code_tailcall('$f','$c')).'
      }').';
    }';
    $o->{$key} = $o->_eval_code($code);
  }
  $o->{$key}($o,$f);
}
sub sub_let {
  my($o,$f)=@_;
  my $key = 'cached_sub_let';
  if(not exists $o->{$key}) {
    my $code = '
    sub {
      my($o,$f)=@_;
      sub {
        my $c = $_[0];
        '.$o->source_let_vars($o->{config_backtrack_vars},
                              $o->code_call('$f','$c')).'
      };
    }';
    $o->{$key} = $o->_eval_code($code);
  }
  $o->{$key}($o,$f);
}
sub sub_alt {
  my($o,$afs)=@_;
  my $key = 'cached_sub_alt';
  if(not exists $o->{$key}) {
#XXX- "Useless use of private variable in void context at sub_alt line 15"
#end of the let.  XXX - let should do something about line numbering?
    my $code = '
#line 2 "sub_alt"
    sub {
      my($o,$afs)=@_;
      my @fs = @$afs;
      '.$o->code_subwrap("'sub_alt'",'sub {
        my $c = $_[0];
        for my $f (@fs) {
          '.$o->source_let_vars($o->{config_backtrack_vars},
                                $o->code_call('$f','$c'),
                                'return $_v_',undef,
                                $o->code_fail_propagate_exceptions('$_v_')).'
        }
        '.$o->code_fail.';
      }').';
    }';
    $o->{$key} = $o->_eval_code($code);
  }
  $o->{$key}($o,$afs);
}
sub sub_alt_dynamic {#TODO-refactor back into sub_alt?
  my($o,$afs)=@_;
  my $key = 'cached_sub_alt_dynamic';
  if(not exists $o->{$key}) {
    my $code = '
#line 2 "sub_alt_dynamic"
    sub {
      my($o,$afs)=@_;
      '.$o->code_subwrap("'sub_alt_dynamic'",'sub {
        my $c = $_[0];
        my @fs = @$afs;
        for my $f (@fs) {
          '.$o->source_let_vars($o->{config_backtrack_vars},
                                $o->code_call('$f','$c'),
                                'return $_v_',undef,
                                $o->code_fail_propagate_exceptions('$_v_')).'
        }
        '.$o->code_fail.';
      }').';
    }';
    $o->{$key} = $o->_eval_code($code);
  }
  $o->{$key}($o,$afs);
}

sub sub_concat { # XXX - currently ignoring the code_tailcall abstractions
  my($o,$afs)=@_;
  my $key = 'cached_sub_concat_v0';
  if(not exists $o->{$key}) {
    my $code = '
      sub {
        my($o,$afs)=@_;
	my @fs = @$afs;
	return $o->noop if @fs == 0;
	return $fs[0] if @fs == 1;
	my $code1 = ""; my $code2 = "";
	my $code0 = "my \$f0 = \$fs[0]; ";
	for my $i (reverse(1..$#fs)) {
	  $code0 .= "my \$f$i = \$fs[$i]; ";
	  $code1 .= "sub{\@_=";
	  $code2 .= ";goto \&\$f$i}";
	}
	my $code = $code0."\n sub{my \$cn = \$_[0]; \@_=".$code1."\$cn".$code2.";goto \&\$f0}\n";
	#print $code;
	eval($code) || die $@.$code;
      }
    ';
    $o->{$key} = $o->_eval_code($code);
  }
  $o->{$key}($o,$afs);
}

sub sub_repeat {
  my($o,$f,$min,$max,$ng)=@_;
  $min = 0 if !defined $min;
  $max = (1000**1000**1000) if !defined $max;
  $min += 0; $max += 0;
  $ng = $ng ? 'nongreedy' : 'greedy';
  Carp::confess "sub_repeat: min cant be greater than max" if $min > $max;
  my $stem = 'cached_sub_repeat_';
  my $key =  'cached_sub_repeat_'.$ng;
  if(not exists $o->{$key}) {
    my $gen_code = sub {
      my($first,$second)=@_;
      return '
#line 2 "cached_sub_repeat"
      sub {
        my($o,$f,$min,$max)=@_;
        '.$o->code_subwrap("'$key'",'sub{
          my $c = $_[0];
          my $pos_old = -1;
          my $i = 0;
          my($fmin,$fagain,$frest);
          $fmin = sub{
            if($i >= $min) {
              goto &$fagain;
            }
            $i++; '.$o->code_tailcall('$f','$fmin').'
          };
          $fagain = sub{
            if($pos_old >= '.$o->{config_pos_var}.'){
              '.$o->code_tailcall('$c').';
            }
            $pos_old = '.$o->{config_pos_var}.';
            goto &$frest;
          };
          $frest = sub{
            if($i >= $max) {
              '.$o->code_tailcall('$c').';
            }
            $i++;
            my $v = '.$o->source_let_vars($o->{config_backtrack_vars},
                                          $o->code_call(@$first)).';
            return $v if '.$o->code_fail_isnot('$v').';
            '.$o->code_tailcall(@$second).';
          };
          goto &$fmin;
        }').'
      }';
    };
    my $recurse = ['$f','$fagain'];
    my $continue= ['$c'];
    my $code_g  = $gen_code->($recurse,$continue);
    my $code_ng = $gen_code->($continue,$recurse);
    $o->{$stem.'greedy'}    = $o->_eval_code($code_g);
    $o->{$stem.'nongreedy'} = $o->_eval_code($code_ng);
  }
  $o->{$key}($o,$f,$min,$max,$ng);
}


package Regexp::Engine::Reentrant::Backtrack::MakeStringSearch;
use base 'Regexp::Engine::Reentrant::Backtrack::Make';

sub new {
  my $o = shift->SUPER::new(@_);
}

sub sub_eat_regexp {
  my($o,$re)=@_;
  Carp::confess if !defined($re);
  my $str = $o->{config_str_var};
  my $pos = $o->{config_pos_var};
  #print "Making eater for /$re/.\n";
  my $code = "
#line 2 \"sub_eat_regexp\"
  ".$o->code_subwrap("'sub_eat_regexp'","sub {
    my \$c = \$_[0];
    pos($str) = $pos;
    $str =~ /\\G($re)/ or ".$o->code_fail.";
    $pos += length(\$1);
    ".$o->code_tailcall('$c')."
  }");
#    print \"Trying to match /$re/ against '$str' pos $pos.\\n\"; # bad if $re contains \n
  $o->_eval_code($code);
}

sub sub_capture_variant1 {
  my($o,$idx,$f)=@_;
  my $key = 'cached_capture_variant1';
  if(not exists $o->{$key}) {
    my $pos = $o->{config_pos_var};
    my $str = $o->{config_str_var};
    my $cap = $o->{config_cap_var};
    my $code = '
#line 2 "sub_capture_variant1"
    sub {
      my($o,$idx,$f)=@_;
      sub {
        my $c = $_[0];
        my $m = MatchOne->new();
        my $from = '.$pos.';
        my $close = sub {
          my $c0 = $_[0];
          my $to = '.$pos.';
          $m->match_set(1,substr('.$str.',$from,$to-$from),[],{},$from,$to);
          '.$o->code_tailcall('$c0','$c').'
        };
        return '.$o->source_let_vars([@{$o->{config_backtrack_vars}},$cap],
             $cap.' = [@{'.$cap.'}];
           '.$cap.'->[$idx] = $m;
             my $v = $f->($close);
             $m->match_set_as_failed if '.$o->code_fail_is('$v').';
             $v;').'
      }
    }';
    $o->{$key} = $o->_eval_code($code);
  }
  $o->{$key}($o,$idx,$f);
}

sub sub_match {
  my($o)=@_;
  my $key = 'cached_match';
  if(not exists $o->{$key}) {
    my $pos = $o->{config_pos_var};
    my $str = $o->{config_str_var};
    my $cap = $o->{config_cap_var};
    my $mat = $o->{config_match_var};
    my $code = '
    sub {
      my($r,$s)=@_;
      my $len = length($s);
      for my $start (0..$len) {
        local '.$str.' = $s;
        local '.$pos.' = $start;
        local '.$cap.' = [];
        my $m = MatchOne->new();
        local '.$mat.' = $m;
        my $ok = $r->($noop);
        if('.$o->code_fail_isnot('$ok').') {
          my $a = '.$cap.';
          $m->match_set(1,substr('.$str.',$start,'.$pos.'-$start),
                  $a,\%{$m},$start,'.$pos.');
          return $m;
        }
      }
      return MatchOne->new()->match_set_as_failed;
    }';
    $o->{$key} = $o->_eval_code($code);
  }
  $o->{$key};
}

1;
__END__
