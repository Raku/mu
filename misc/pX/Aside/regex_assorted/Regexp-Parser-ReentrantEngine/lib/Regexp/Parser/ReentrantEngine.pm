
package Regexp::Parser::ReentrantEngine;

use Regexp::Parser;
{
  package Regexp::Parser::ReentrantEngine::BacktrackMacros;
  
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
BEGIN { Regexp::Parser::ReentrantEngine::BacktrackMacros->import; };
use strict;
use warnings;

# Regexp::Parser author japhy has not been easy to contact.
if($Regexp::Parser::VERSION <= 0.20) {
  eval <<'END';
#line 1 "Regexp::Parser bug fix/workaround"
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


local $Regexp::Parser::ReentrantEngine::Env::str;
local $Regexp::Parser::ReentrantEngine::Env::pos;
local $Regexp::Parser::ReentrantEngine::Env::current_match;
local $Regexp::Parser::ReentrantEngine::Env::cap;


{
  package Regexp::Parser::ReentrantEngine::ParserNodeExtraMethods;

  sub RPRE_emit {
    my $cls = ref($_[0]);
    die "bug: $cls RPRE_emit() unimplemented\n";
  }

  my $noop;
  $noop = sub{
    my $c = $_[0];
    return 1 if !defined($c) || $c eq $noop;
    TAILCALL(&$c,$noop);
  };
  sub RPRE_noop { $noop }

  sub RPRE_eat_regexp {
    my($o,$re)=@_;
    my $noop = $o->RPRE_noop;
    my $qr = qr/\G($re)/;
    sub {
      my $c = $_[0];
      my($str) = $Regexp::Parser::ReentrantEngine::Env::str;
      pos($str) = $Regexp::Parser::ReentrantEngine::Env::pos;
      $str =~ $qr or FAIL();
      $Regexp::Parser::ReentrantEngine::Env::pos += length($1);
      TAILCALL(&$c,$noop);
    };
  }

  sub RPRE_concat {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne 'ARRAY';
    my @a = @$aref;
    return $o->RPRE_noop if @a == 0;
    return $a[0]->RPRE_emit if @a == 1;
    my @fs = map { $_->RPRE_emit } @a;
    my $code1 = ""; my $code2 = "";
    my $code0 = "my \$f0 = \$fs[0]; ";
    for my $i (reverse(1..$#a)) {
      $code0 .= "my \$f$i = \$fs[$i]; ";
      $code1 .= "sub{\@_=";
      $code2 .= ";goto \&\$f$i}";
    }
    my $code = $code0."
#line 2 \"Regexp::Parser::ReentrantEngine::ParserNodeExtraMethods RPRE_concat\"
\n sub{my \$cn = \$_[0]; \@_=".$code1."\$cn".$code2.";goto \&\$f0}\n";
    eval($code) || die "$@";
  }   

  sub RPRE_alt {
    my($o,$afs)=@_;
    my @fs = @$afs;
    my $f_last = pop(@fs);
    sub{
      my $c = $_[0];
      for my $f (@fs) {
        my $v = LET($Regexp::Parser::ReentrantEngine::Env::pos){ $f->($c) }LET;
        return $v if not FAILED($v);
      }
      TAILCALL(&$f_last,$c);
    };
  }

  sub RPRE_repeat {
    # You would think RPRE_repeat_real(@_) would work.  But no. XXX
    return Regexp::Parser::quant::RPRE_repeat_copy_placed_elsewhere_XXX(@_);
  }
  sub RPRE_repeat_real {
    my($o,$f,$min,$max,$ng)=@_;
    my $greedy = !$ng;
    my $noop = $o->RPRE_noop;
    sub{
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
        if( !($previous_pos < $Regexp::Parser::ReentrantEngine::Env::pos) ||
            !($count < $max))
        {
          TAILCALL(&$c,$noop);
        }
        $previous_pos = $Regexp::Parser::ReentrantEngine::Env::pos;
        $count++;

        my $v = LET($Regexp::Parser::ReentrantEngine::Env::pos){
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

  sub RPRE_capture {
    my($o,$idx,$f)=@_;
    sub{
      my $c = $_[0];
      my $m = Regexp::Parser::ReentrantEngine::Match->new();
      my $from = $Regexp::Parser::ReentrantEngine::Env::pos;
      my $close = sub {
        my $c0 = $_[0];
        my $to = $Regexp::Parser::ReentrantEngine::Env::pos;
        $m->match_set(1,substr($Regexp::Parser::ReentrantEngine::Env::str,$from,$to-$from),[],{},$from,$to);
        TAILCALL(&$c0,$c);
      };
      return LET($Regexp::Parser::ReentrantEngine::Env::cap){
        $Regexp::Parser::ReentrantEngine::Env::cap = [@$Regexp::Parser::ReentrantEngine::Env::cap];
        $Regexp::Parser::ReentrantEngine::Env::cap->[$idx] = $m;
        my $v = $f->($close);
        $m->match_set_as_failed if FAILED($v);
        $v;
      }LET;
    };
  }

}


# Regexp::Parser ast nodes

{
  package Regexp::Parser::__object__;
  use base 'Regexp::Parser::ReentrantEngine::ParserNodeExtraMethods';
}  

{
  # \A ^ \B \b \G \Z \z $
  package Regexp::Parser::anchor;
  sub RPRE_emit {
    my($o)=@_;
    my $raw = $o->raw();
    my $flag_m = $o->{'flags'} & $o->{rx}->FLAG_m;
    my $re = $raw;
    $re = "(?m:$re)" if $flag_m && ($raw eq '^' || $raw eq '$');
    return $o->RPRE_eat_regexp($re) if $raw ne '\G';
    # XXX - \G is unimplemented.
    my $noop = $o->RPRE_noop;
    return sub{
      FAIL(); #XXX posG not implemented
      FAIL() if !($Regexp::Parser::ReentrantEngine::Env::pos == $Regexp::Parser::ReentrantEngine::Env::posG);
      my $c = $_[0]; TAILCALL(&$c,$noop);
    };
  }
}
{
  # . \C
  package Regexp::Parser::reg_any;
  sub RPRE_emit {
    my($o)=@_;
    my $re = $o->raw;
    my $flag_s = $o->{'flags'} & $o->{rx}->FLAG_s;
    $re = "(?s:.)" if $flag_s;
    $o->RPRE_eat_regexp($re);
  }
}
{
  # \w \W
  package Regexp::Parser::alnum;
  sub RPRE_emit { my($o)=@_; $o->RPRE_eat_regexp($o->raw); }
}
{
  # \s \S
  package Regexp::Parser::space;
  sub RPRE_emit { my($o)=@_; $o->RPRE_eat_regexp($o->raw); }
}
{
  # \d \D
  package Regexp::Parser::digit;
  sub RPRE_emit { my($o)=@_; $o->RPRE_eat_regexp($o->raw); }
}
{
  package Regexp::Parser::anyof;
  sub RPRE_emit {
    my($o)=@_;
    my $re = $o->visual();
    my $flag_i = $o->{'flags'} & $o->{rx}->FLAG_i;
    $re = "(?i:$re)" if $flag_i;
    return $o->RPRE_eat_regexp($re);
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
  sub RPRE_emit {
    my($o)=@_;
    my(@fs) = map { $o->RPRE_concat($_) } @{$o->data};
    $o->RPRE_alt(\@fs);
  }
}
{
  package Regexp::Parser::exact;
  sub RPRE_emit{
    my($o)=@_;
    my $re = $o->visual;
    my $flag_i = $o->{'flags'} & $o->{rx}->FLAG_i;
    $re = "(?i:$re)" if $flag_i;
    $o->RPRE_eat_regexp($re);
  }
}
{
  package Regexp::Parser::quant;

  sub RPRE_repeat_copy_placed_elsewhere_XXX {
    my($o,$f,$min,$max,$ng)=@_;
    my $greedy = !$ng;
    my $noop = $o->RPRE_noop;
    sub{
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
        if( !($previous_pos < $Regexp::Parser::ReentrantEngine::Env::pos) ||
            !($count < $max))
        {
          TAILCALL(&$c,$noop);
        }
        $previous_pos = $Regexp::Parser::ReentrantEngine::Env::pos;
        $count++;

        my $v = LET($Regexp::Parser::ReentrantEngine::Env::pos){
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

  sub RPRE_emit {
    my($o)=@_;
    my($min,$max)= (@$o{'min','max'});
    $min = 0 if $min eq "";
    $max = 1000**1000**1000 if $max eq ""; #XXX inf
    $min += 0; $max += 0; 
    my $f = $o->data->RPRE_emit;
    $o->RPRE_repeat($f,$min,$max);
  }
}
{
  # *? +? {,}? - sort of: have to reach down into quant.
  package Regexp::Parser::minmod;
  sub RPRE_emit {
    my($o)=@_;
    my $noop = $o->RPRE_noop;
    my($min,$max)= (@{$o->data}{'min','max'});
    $min = 0 if $min eq "";
    $max = 1000**1000**1000 if $max eq ""; #XXX inf
    $min += 0; $max += 0; 
    my $f = $o->data->data->RPRE_emit; # reach down into quant below.
    $o->RPRE_repeat($f,$min,$max,1);
  }
}
{
  # ( non-capturing
  package Regexp::Parser::group;
  sub RPRE_emit {
    my($o)=@_;
    my $f = $o->RPRE_concat( $o->data );
    return $f;
  }
}
{
  # ( capturing
  package Regexp::Parser::open;
  sub RPRE_emit {
    my($o)=@_;
    my $f = $o->RPRE_concat( $o->data );
    my $idx = $o->{'nparen'} -1;
    $o->RPRE_capture($idx,$f);
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
  # XXX - should backrefs be flag_i sensitive?
  sub RPRE_emit {
    my($o)=@_;
    my $noop = $o->RPRE_noop;
    my $idx = $o->{'nparen'} -1;
    sub {
      my $c = $_[0];
      FAIL() if $idx >= @$Regexp::Parser::ReentrantEngine::Env::cap;
      my $m = $Regexp::Parser::ReentrantEngine::Env::cap->[$idx];
      my $s = defined($m) ? "$m" : "";
      my $pat = $s;
      $pat =~ s/(\W)/\\$1/g;
      $pat = "(?:(?i)(?:$pat))" if($o->{'flags'} & $o->{rx}->FLAG_i);
      my $ok = substr($Regexp::Parser::ReentrantEngine::Env::str,$Regexp::Parser::ReentrantEngine::Env::pos) =~ /\A($pat)/;
      FAIL() if !$ok;
      $Regexp::Parser::ReentrantEngine::Env::pos += length($1);
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
  sub RPRE_emit {
    my($o)=@_;
    my $noop = $o->RPRE_noop;
    my $f = $o->RPRE_concat( $o->data );
    my $dir = $o->{'dir'};
    if($dir>0) {
      sub {
        my $c = $_[0];
        { local($Regexp::Parser::ReentrantEngine::Env::pos)=($Regexp::Parser::ReentrantEngine::Env::pos);
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
    my $pos = $Regexp::Parser::ReentrantEngine::Env::pos;
    local $Regexp::Parser::ReentrantEngine::Env::pos = $Regexp::Parser::ReentrantEngine::Env::pos;
    my $at_pos = sub{ FAIL() if $Regexp::Parser::ReentrantEngine::Env::pos != $pos; return 1;};
    for(my $i = $Regexp::Parser::ReentrantEngine::Env::pos;$i>=0;$i--) {
      $Regexp::Parser::ReentrantEngine::Env::pos = $i;
      my $v = $f->($at_pos);
      return 1 if not FAILED($v);
    }
    return 0;
  }
}
{
  # (?!) (?<!)
  package Regexp::Parser::unlessm;
  sub RPRE_emit {
    my($o)=@_;
    my $noop = $o->RPRE_noop;
    my $f = $o->RPRE_concat( $o->data );
    my $dir = $o->{'dir'};
    if($dir>0) {
      sub {
        my $c = $_[0];
        my $v;
        { local($Regexp::Parser::ReentrantEngine::Env::pos)=($Regexp::Parser::ReentrantEngine::Env::pos);
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
  sub RPRE_emit {
    my($o)=@_;
    my $noop = $o->RPRE_noop;
    my $f = $o->RPRE_concat( $o->data );
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
  sub RPRE_emit {
    my($o)=@_;
    my $noop = $o->RPRE_noop;

    my $f_test = $o->data->[0]->RPRE_emit;
    my $f_then = $o->data->[1]->data->[0][0]->RPRE_emit;
    my $crufty = $o->data->[1]->data;
    my $f_else = sub{my $c = $_[0]; TAILCALL(&$c,$noop);};
    $f_else = $o->data->[1]->data->[1][0]->RPRE_emit if @{$o->data->[1]->data} > 1;
    sub {
      my $c = $_[0];
      my $v;
      { local($Regexp::Parser::ReentrantEngine::Env::pos)=($Regexp::Parser::ReentrantEngine::Env::pos);
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
  sub RPRE_emit {
    my($o)=@_;
    my $noop = $o->RPRE_noop;
    my $idx = $o->{'nparen'} -1;
    sub {
      my $c = $_[0];
      FAIL() if $idx >= @$Regexp::Parser::ReentrantEngine::Env::cap;
      my $m = $Regexp::Parser::ReentrantEngine::Env::cap->[$idx];
      FAIL() if !$m;
      TAILCALL(&$c,$noop);
    };
  }
}
{
  # (?{ ... })
  package Regexp::Parser::eval;
  sub RPRE_emit {
    my($o)=@_;
    my $noop = $o->RPRE_noop;
    my $embedded_code = join("",$o->data);
    my $code = '
#line 2 "in Regexp::Parser::eval"
sub{my $__c__ = $_[0]; '.$embedded_code.'; @_=$noop; goto &$__c__}';
    eval($code) || die "Error compiling (?{$embedded_code}) :\n$@\n";
  }
}
{
  # (??{ ... })
  package Regexp::Parser::logical;
  sub RPRE_emit { Regexp::Parser::eval::RPRE_emit(@_) }
}
{
  package Regexp::Parser::flags;
  sub RPRE_emit {
    my($o)=@_;
    $o->RPRE_noop;
  }
}


package Regexp::Parser::ReentrantEngine::Match;

use overload
    'bool' => 'match_boolean',
    '""'   => 'match_string',
    '@{}'  => 'match_array',
    '%{}'  => 'match_hash',
    ;

sub match_boolean {${$_[0]}->{match_boolean}}
sub match_string  {${$_[0]}->{match_string}}
sub match_array   {${$_[0]}->{match_array}}
sub match_hash    {${$_[0]}->{match_hash}}

sub from          {${$_[0]}->{match_from}}
sub to            {${$_[0]}->{match_to}}

sub new {
    my($cls)=@_;
    my $o = \(my $h = {
      match_boolean => 1,
      match_string  => "",
      match_array   => [],
      match_hash    => {},
      match_from    => undef,
      match_to      => undef
    });
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
    my($o)=@_;
    my $os = "$o";
    $os = $o->match__indent_except_top($os) if $os =~ /\n/;
    my $s = $o->match__describe_name_as;
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
sub match__indent {my($o,$s)=@_; $s =~ s/^(?!\Z)/  /mg; $s}
sub match__indent_except_top {my($o,$s)=@_; $s =~ s/^(?<!\A)(?!\Z)/  /mg; $s}
sub match__describe_name_as {
    my($o)=@_;
    my $s = overload::StrVal($o);
    $s;
}


{
  package Regexp::Parser::ReentrantEngine::SubruleKludge;
  our %rules;
  sub dorule {
    my($c,$name,$args)=@_;
    my $noop = Regexp::Parser::ReentrantEngine::ParserNodeExtraMethods->RPRE_noop;

    my $ru = $rules{$name};
    if(!defined $ru) {
      warn "Unknown rule '$name'";
      FAIL();
    }

    my $pos = $Regexp::Parser::ReentrantEngine::Env::pos;
    my $cap = $Regexp::Parser::ReentrantEngine::Env::cap;
    my $m0 = $Regexp::Parser::ReentrantEngine::Env::current_match;
    my $m1 = Regexp::Parser::ReentrantEngine::Match->new;
    $$m1->{'RULE'} ||= $name; #EEEP
    $m1->match_set(1,"",[],{},$pos,undef);

    my $rest = sub{
      my $cn = $_[0];
      $$m1->{'match_array'} = $Regexp::Parser::ReentrantEngine::Env::cap; #EEEP
      $$m1->{'match_to'} = $Regexp::Parser::ReentrantEngine::Env::pos; #EEEP
      $$m1->{'match_string'} = substr($Regexp::Parser::ReentrantEngine::Env::str,$pos,$Regexp::Parser::ReentrantEngine::Env::pos-$pos);
      local $m0->{$name} = [@{$m0->{$name}||[]}];
      push(@{$m0->{$name}},$m1); #see below
      $Regexp::Parser::ReentrantEngine::Env::cap = $cap;
      $Regexp::Parser::ReentrantEngine::Env::current_match = $m0;
      TAILCALL(&$cn,$c);
    };

    my $v;
    { local $Regexp::Parser::ReentrantEngine::Env::current_match = $m1;
      local $Regexp::Parser::ReentrantEngine::Env::cap = [];
      $v = $ru->($rest);
    }
    FAIL_IF_FAILED($v);
    unshift(@{$m0->{$name}},$m1);# sigh,
    # why twice?: once for inline code, once for the final Match tree.
    return $v;
  }
  sub def_rule {
    my($name,$re)=@_;
    my $r = ref($re) ? $re : compile($re);
    die if !defined $r;
    $Regexp::Parser::ReentrantEngine::SubruleKludge::rules{$name} = $r;
  }
  sub preprocess_re {
    my($re)=@_;
    $re =~ s/<(\w+)([^\>]*)>/(?{\@_=(\$__c__,'$1','$2');goto \&Regexp::Parser::ReentrantEngine::SubruleKludge::dorule;})/g;
    $re;
  }
}


package Regexp::Parser::ReentrantEngine;

sub new {
  my($cls,$pat,$mods)=@_;
  my $o = bless {
    pattern => $pat,
    modifiers => $mods
  },$cls;
  $o->init;
}
sub init {
  my($o)=@_;
  my $re   = $o->{pattern};
  my $mods = $o->{modifiers};
  $re = "(?$mods)(?:$re)" if $mods;
  $re = '(?:)' if $re eq ''; #Regexp::Parser bug workaround.
  #$re = Regexp::Parser::ReentrantEngine::SubruleKludge::preprocess_re($re);
  $o->{regexp} = $re;
  #print STDERR "COMPILING \"$re\" ",length($re),"\n";
  my $parser = Regexp::Parser->new($re);
  my $n = eval{ $parser->root };
  Carp::confess "compile \"$re\" failed: $@" if !defined $n;
  my $r = Regexp::Parser::ReentrantEngine::ParserNodeExtraMethods->RPRE_concat($n);
  my $nparens = $parser->nparen;
  $o->{parser} = $parser;
  $o->{nparens} = $nparens;
  $o->{matcher} = $r;
  $o;
}
sub match {
  my($o,$s,$beginat,$minlen)=@_;
  my $nparen = $o->{nparen};
  my $r = $o->{matcher};
  my $len = length($s);
  $beginat = 0 if !defined($beginat);
  my $noop = Regexp::Parser::ReentrantEngine::ParserNodeExtraMethods->RPRE_noop;
  my $atend = $noop;
  if(defined $minlen) {
    my $min_end = $minlen + $beginat;
    $atend = sub{return undef if $Regexp::Parser::ReentrantEngine::Env::pos < $min_end;return 1;}
  }
  for my $start ($beginat..$len) {
    local $Regexp::Parser::ReentrantEngine::Env::str = $s;
    local $Regexp::Parser::ReentrantEngine::Env::pos = $start;
    local $Regexp::Parser::ReentrantEngine::Env::cap = [];
    my $m = Regexp::Parser::ReentrantEngine::Match->new();
    local $Regexp::Parser::ReentrantEngine::Env::current_match = $m;
    my $ok = $r->($atend);
    if(not FAILED($ok)) {
      my $a = $Regexp::Parser::ReentrantEngine::Env::cap;
      if(defined($nparen) && $nparen > @$a) {
        for my $i (@$a..$nparen) {
          push(@$a,Regexp::Parser::ReentrantEngine::Match->new()->match_set_as_failed);
        }
      }
      for my $am (@$a) {
        $am = Regexp::Parser::ReentrantEngine::Match->new()->match_set_as_failed() if !defined($am);
      }
      $m->match_set(1,substr($Regexp::Parser::ReentrantEngine::Env::str,$start,$Regexp::Parser::ReentrantEngine::Env::pos-$start),
              $a,\%{$m},$start,$Regexp::Parser::ReentrantEngine::Env::pos);
      return $m;
    }
  }
  return Regexp::Parser::ReentrantEngine::Match->new()->match_set_as_failed;
}
sub match_re {
  my($pat,$mods,$str)=@_;
  ($mods,$str) = (undef,$mods) if !defined $str;
  my $o = __PACKAGE__->new($pat,$mods);
  $o->match($str);
}

1;
__END__
  sub _test_target {
    sub{my($mods,$re)=@_;
      my $o = __PACKAGE__->new($re,$mods);
      sub{my($s)=@_;$o->match($s)}
    };
  }
  if(@ARGV && $ARGV[0] eq '--test') {
    require './t/re_tests.pl';
    Pkg_re_tests::test(&_test_target);
    exit;
  }

