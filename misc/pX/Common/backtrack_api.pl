=for fyi

This module encapsulates a backtracking protocol.

Notes

Most of the methods take perl code strings, and return them wrapped in
code to do backtracking things.  A few of the methods, named
sub_whatever, take and return continuations, which are what the
backtracker is built of.  One method, sub_makesub, takes code and
returns a continuation.

A continuation is a sub of a single argument, its own continuation.
Return values of undef and negative numbers are reserved for the
backtracker.  Undef means simple failure, ie, time to backtrack.
Negative numbers mean an exception, ie, a jump up the backtrack stack.
For example, a fail match, rule, or group.

Why only one argument?  Pushing and poping arguments from the stack is
slowwww.  Better to use locals, perhaps unless the variable changes
with almost every call.  pos is may be the only good candidate.  If
application arguments get implemented, it would be worth trying.

What is up with the noop ref checking?  It is fast, and since one of
the many places it gets called is at ultimate match success, it seemed
worth making flexible.  An alternative might be to have a single
canonical noop, and a local() callback called by noop when noop(noop).
But that could be problematic.

config_backtrack_vars is an array of local() variables (specified by
name), that should be localized on each choice point, and restored if
backtracking occurs.

config_pos_var is a local() variable which contains string scan
position.  It should probably be one of the config_backtrack_vars.
It is needed only in sub_repeat, to do the "making progress?" test.

The only dependency on the string search task is in one method,
repeat, aka quantification, which needs a concept of position to
determine if progress is being made.

A mutant regexp_engine_spike (not included), with backtrack_api
swapped in as the backtracking core, still passes re_tests.


History

2006-09-19 - The idea here is Perl6::Compiler::Rule needs ranges
{n..m} and tailcalls implemented, and perhaps performance tuning.
The old regexp_spike code has these.  This file is a snapshot of
a quick attempt to strip the old code down to be easily used by
perl5/Pugs-Compiler-Rule/lib/Pugs/Emitter/Rule/Perl5.pm

=cut

package Backtrack;
use Carp;
use strict;

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
# sub_makesub
# An attempt to make the backtracking api cleaner.
# Client code gets the contination in $c, and doesn't have to care
# where exactly it came from, and what other arguments might be present.
# It is not yet clear this is actually useful.

sub sub_makesub { # XXX - Untested
  my($o,$name_for_debugging,$code)=@_;
  my $fullcode = 'sub { my $c = $_[0]; '.$code.' }';
  $o->_eval_code($code);
}

#----------------------------------------------------------------------
# Do-nothing continuation
#
# Evaluate your code with a lexical $noop defined, ie
#    my $noop = $this_api->sub_noop();
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

sub sub_noop {
  my($o)=@_;
  my $key = 'cached_sub_noop';
  if(not exists $o->{$key}) {
    my $noop;
    $o->{$key} = $noop = eval($o->noop_definition()); die $@ if $@;
  }
  $o->{$key};
}

# Convenience wrapper around "my $noop = $o->sub_noop();".
sub eval_with_noop {
  my($o,$code)=@_;
  my $noop = $o->sub_noop;
  eval($code);
}
sub _eval_code {
  my($o,$code)=@_;
  my $result = $o->eval_with_noop($code);
  die $@.$code if $@;
  $result;
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
sub propagate_failure { # XXX - Untested
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

sub general_temp { # XXX - Untested
  my($o,$vars,$body)=@_;
  my $varl = join(",",@$vars);
  return ("(do{ $body })") if $varl eq "";
  return ("(do{ local($varl)=($varl); $body })");
}

sub general_let {
  my($o,$vars,$body,@rest)=@_;
  $o->general_alt($vars,[$body],@rest);
}

sub _genstr {sprintf("%x",int(rand(1_000_000_000)));} # XXX
sub general_alt {
  my($o,$vars,$bodies,$code_on_success,$code_on_failure,$code_on_each_failure)=@_;
  # The $code_on_whatever code can see the body result in $_v_.
  my $uniq = $o->_genstr;
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
  if(@$bodies == 0) { $bodies = ['undef'] }
  if(@$bodies == 1) {
    $vars_setup = "my($tmpl);";
    $vars_reset = "";
  }
  if($varl eq '') {
    $vars_setup = $vars_reset = $vars_local = $vars_save = $vars_set = "";
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


sub temp { # XXX - Untested
  my($o,$body)=@_;
  $o->general_temp($o->config_backtrack_vars, $body);
}

sub let { # XXX - Untested
  my($o,$body)=@_;
  $o->general_let($o->config_backtrack_vars, $body);
}

sub alt { # XXX - Untested
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

#----------------------------------------------------------------------
# repeat

sub sub_repeat {
  my($o,$f,$min,$max,$ng)=@_;
  $min = 0 if !defined $min;
  $max = (1000**1000**1000) if !defined $max; # Inf
  $min += 0; $max += 0;
  $ng = $ng ? 'nongreedy' : 'greedy';
  Carp::confess("sub_repeat: min cant be greater than max") if $min > $max;
  my $stem = 'cached_sub_repeat_';
  my $key =  'cached_sub_repeat_'.$ng;
  if(not exists $o->{$key}) {
    my $gen_code = sub {
      my($first,$second)=@_;
      return '
#line 2 "cached_sub_repeat"
      sub {
        my($o,$f,$min,$max)=@_;
        sub{
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
            my $v = '.$o->general_let($o->config_backtrack_vars,
                                      $o->call(@$first)).';
            return $v if '.$o->is_not_failure('$v').';
            '.$o->tailcall(@$second).';
          };
          goto &$fmin;
        }
      }';
    };
    my $recurse = ['$f','$fagain'];
    my $continue= ['$c'];
    my $code_g  = $gen_code->($recurse,$continue);
    my $code_ng = $gen_code->($continue,$recurse);
    $o->{$stem.'greedy'}    = $o->_eval_code($code_g);
    $o->{$stem.'nongreedy'} = $o->_eval_code($code_ng);
  }
  $o->{$key}($o,$f,$min,$max);
}

sub sub_concat { # XXX - currently ignoring the tailcall() abstraction
  my($o,$afs)=@_;
  my $key = 'cached_sub_concat_v0';
  if(not exists $o->{$key}) {
    my $code = '
      sub {
        my($o,$afs)=@_;
	my @fs = @$afs;
	return $o->sub_noop if @fs == 0;
	return $fs[0] if @fs == 1;
	my $code1 = ""; my $code2 = "";
	my $code0 = "my \$f0 = \$fs[0]; ";
	for my $i (reverse(1..$#fs)) {
	  $code0 .= "my \$f$i = \$fs[$i]; ";
	  $code1 .= "sub{\@_=";
	  $code2 .= ";goto \&\$f$i}";
	}
	my $code = $code0."\n sub{my \$cn = \$_[0]; \@_=".$code1."\$cn".$code2.";goto \&\$f0}\n";
	print $code;
	eval($code) || die $@.$code;
      }
    ';
    $o->{$key} = $o->_eval_code($code);
  }
  $o->{$key}($o,$afs);
}

1;
__END__



