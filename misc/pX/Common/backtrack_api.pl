=for fyi

2006-09-19 - The idea here is Perl6::Compiler::Rule needs ranges
{n..m} and tailcalls implemented, and perhaps performance tuning.
The old regexp_spike code has these.  This file is a snapshot of
a quick attempt to strip the old code down to be easily used by
perl5/Pugs-Compiler-Rule/lib/Pugs/Emitter/Rule/Perl5.pm

Plan:
 Finish stripping down the code.
 Use a test jig(DONE) to run it against re_tests, to assure nothing broke.
 Integrate with PCR.  Perhaps an experimental Emitter/Perl5x.pm.


=cut

package Backtrack;

sub new {
  my $cls = shift;
  my $o = {};
  bless $o,$cls;
}

#----------------------------------------------------------------------
# Application arguments
# unimplemented.
# Passing arguments is not fast.  So locals are often a better way to
# pass data.  But it may be needed.

#----------------------------------------------------------------------
# Do-nothing continuation
#
# Evaluate your code with a lexical $noop defined, ie
#    my $noop = $this_api->continuation_noop();
#    ...your generated code here...
# eval_with_noop() is simply eval() with this extra line.
#

# Empty class to label noop subs. Private.
{package Backtrack::Noop;}

sub noop_definition {
  my($o)=@_;
  '
    bless sub {
      my $c = $_[0];
      return 1 if '.$o->is_noop('$c').';
      '.$o->tailcall('$c','$noop').";
    }, 'Backtrack::Noop';";
}

sub continuation_noop {
  my($o)=@_;
  my $key = 'cached_continuation_noop';
  if(not exists $o->{$key}) {
    $o->{$key} = eval($o->noop_definition()); die $@ if $@;
  }
  $o->{$key};
}

# Convenience wrapper around "my $noop = $o->continuation_noop();".
sub eval_with_noop {
  my($o,$code)=@_;
  my $noop = $o->continuation_noop;
  eval($code);
}

sub is_noop {
  my($o,$var)=@_;
  "(ref($var) eq 'Backtrack::Noop')";
}

#----------------------------------------------------------------------
# Calling

sub general_call {
  my($o,$f,@args)=@_;
  $f.'->('.join(',',@args).')';
}
sub general_tailcall {
  my($o,$f,@args)=@_;
  '@_=('.join(',',@args)."); goto \&{$f};";
}
#sub general_tailcall_safely { # goto apparently sometimes kills lexical vars?
#  my($o,$f,@args)=@_;
#  'return('.$o->call($f,@args).');';
#}

sub call {
  my($o,$f,$c)=@_;
  $c = '$noop' if !defined $c;
  $o->general_call($f,$c);
}
sub tailcall {
  my($o,$f,$c)=@_;
  $c = '$noop' if !defined $c;
  $o->general_tailcall($f,$c);
}
#sub tailcall_safely {
#  my($o,$f,$c)=@_;
#  $c = '$noop' if !defined $c;
#  $o->general_tailcall_safely($f,$c);
#}


#----------------------------------------------------------------------
# Failure
# Reserved return values: undef, negative numbers.

sub fail {
  my($o,$var)=@_;
  $var = "undef" if not defined $var;
  "return($var)";
}
sub is_failure {
  my($o,$var)=@_;
  "(!defined($var) || (!ref($var) && $var <= 0))";
}
sub is_not_failure {
  my($o,$var)=@_;
  "(!".$o->is_failure($var).")";
}
sub is_exception {
  my($o,$var)=@_;
  "(defined($var) && !ref($var) && $var <= 0)";
}
sub propagate_failure {
  my($o,$var)=@_;
  'if('.$o->is_failure($var).') {'.$o->fail($var).'}';
}
sub propagate_exceptions {
  my($o,$var)=@_;
  'if('.$o->is_exception($var).') {'.$o->fail($var).'}';
}

#======================================================================
# The rest is built on the core above.

#----------------------------------------------------------------------
# temp / let / alt

sub general_temp {
  my($o,$vars,$body)=@_;
  my $varl = join(",",@$vars);
  return ("(do{ $body })") if $varl eq "";
  return ("(do{ local($varl)=($varl); $body })");
}

sub general_let {
  my($o,$vars,$body,@rest)=@_;
  $o->general_alt($vars,[$body],@rest);
}

sub general_alt {
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
            ." if(".$o->is_failure($v).") { $eachfail $vars_reset $code }"
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


sub config_backtrack_vars {
  my($o)=@_;
  []; # array of variables to be restored upon backtracking.
}


sub temp {
  my($o,$body)=@_;
  $o->general_temp($o->config_backtrack_vars, $body);
}

sub let {
  my($o,$body)=@_;
  $o->general_let($o->config_backtrack_vars, $body);
}

sub alt {
  my($o,$bodies)=@_;
  $o->general_alt($o->config_backtrack_vars,
		  $bodies,
		  'return $_v_',
		  $o->fail,
		  $o->propagate_exceptions('$_v_'));
}

sub alt_on_array {
  my($o,$ary,$continuation)=@_;
  '
    for my $_f_ ('.$ary.') {
      '.$o->general_let($o->config_backtrack_vars,
			$o->call('$_f_',$continuation),
			'return $_v_',
			undef,
			$o->propagate_exceptions('$_v_')).'
    }
    '.$o->fail.';
';
}

1;
__END__

These havent be stripped down yet.

#----------------------------------------------------------------------
# repeat

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
        '.$o->subwrap("'$key'",'sub{
          my $c = $_[0];
          my $pos_old = -1;
          my $i = 0;
          my($fmin,$fagain,$frest);
          $fmin = sub{
            if($i >= $min) {
              goto &$fagain;
            }
            $i++; '.$o->tailcall('$f','$fmin').'
          };
          $fagain = sub{
            if($pos_old >= '.$o->config_pos_var.'){
              '.$o->tailcall('$c').';
            }
            $pos_old = '.$o->config_pos_var.';
            goto &$frest;
          };
          $frest = sub{
            if($i >= $max) {
              '.$o->tailcall('$c').';
            }
            $i++;
            my $v = '.$o->source_let_vars($o->config_backtrack_vars,
                                          $o->call(@$first)).';
            return $v if '.$o->is_not_failure('$v').';
            '.$o->tailcall(@$second).';
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


