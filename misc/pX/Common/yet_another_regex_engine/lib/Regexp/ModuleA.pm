package Regexp::ModuleA;
use Carp;

package Regexp::RAST::ReentrantEngine;

{
  package Regexp::RAST::ReentrantEngine::BacktrackMacros;
  
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
BEGIN { Regexp::RAST::ReentrantEngine::BacktrackMacros->import; };

use strict;
use warnings;

local $Regexp::RAST::ReentrantEngine::Env::str;
local $Regexp::RAST::ReentrantEngine::Env::pos;
local $Regexp::RAST::ReentrantEngine::Env::current_match;
local $Regexp::RAST::ReentrantEngine::Env::cap;


{
  package Regexp::RAST::Base;

  use Sub::Name;
  my $id = 1;

  sub RRRE_emit {
    my $cls = ref($_[0]);
    die "bug: $cls RRRE_emit() unimplemented\n";
  }

  my $noop;
  $noop = subname "<noop ".($id++).">" => sub {
    my $c = $_[0];
    return 1 if !defined($c) || $c eq $noop;
    TAILCALL(&$c,$noop);
  };
  sub RRRE_noop { $noop }

  sub RRRE_eat_regexp {
    my($o,$re)=@_;
    my $noop = $o->RRRE_noop;
    my $qr = qr/\G($re)/;
    subname "<eat_regexp ".($id++).">" => sub {
      my $c = $_[0];
      my($str) = $Regexp::RAST::ReentrantEngine::Env::str;
      pos($str) = $Regexp::RAST::ReentrantEngine::Env::pos;
      $str =~ $qr or FAIL();
      $Regexp::RAST::ReentrantEngine::Env::pos += length($1);
      TAILCALL(&$c,$noop);
    }
  }
  sub RRRE_wrap_re_with_mods {
    my($o,$re)=@_;
    my $mod = "";
    $mod .= "i" if $o->{flags}{i};
    $mod .= "m" if $o->{flags}{m};
    $mod .= "s" if $o->{flags}{s};
    $mod .= "x" if $o->{flags}{x};
    return $re if $mod eq "";
    "(?$mod:$re)";
  }
  sub RRRE_alt {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne 'ARRAY';
    my @fs = @$aref;
    my $f_last = pop(@fs);
    subname "<alt ".($id++).">" => sub {
      my $c = $_[0];
      for my $f (@fs) {
        my $v = LET($Regexp::RAST::ReentrantEngine::Env::pos){ $f->($c) }LET;
        return $v if not FAILED($v);
      }
      TAILCALL(&$f_last,$c);
    }
  }
  sub RRRE_concat {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne 'ARRAY';
    my @a = @$aref;
    return $o->RRRE_noop if @a == 0;
    return $a[0]->RRRE_emit if @a == 1;
    my @fs = map { $_->RRRE_emit } @a;
    my $code1 = ""; my $code2 = "";
    my $code0 = "my \$f0 = \$fs[0]; ";
    for my $i (reverse(1..$#a)) {
      $code0 .= "my \$f$i = \$fs[$i]; ";
      $code1 .= "sub {\@_=";
      $code2 .= ";goto \&\$f$i}";
    }
    my $code = $code0."
#line 2 \"Regexp::RAST::Base RRRE_concat\"
\n subname '<concat '.(\$id++).'>' => sub {my \$cn = \$_[0]; \@_=".$code1."\$cn".$code2.";goto \&\$f0}\n";
    eval($code) || die "$@";
  }   
  sub RRRE_repeat {
    my($o,$f,$min,$max,$ng)=@_;
    my $greedy = !$ng;
    my $noop = $o->RRRE_noop;
    subname "<repeat ".($id++).">" => sub {
      if(!defined $noop){die "this perlbug workaround line didn't"}
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
        if( !($previous_pos < $Regexp::RAST::ReentrantEngine::Env::pos) ||
            !($count < $max))
        {
          TAILCALL(&$c,$noop);
        }
        $previous_pos = $Regexp::RAST::ReentrantEngine::Env::pos;
        $count++;

        my $v = LET($Regexp::RAST::ReentrantEngine::Env::pos){
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
  sub RRRE_capture {
    my($o,$idx,$f)=@_;
    my $myid = $id++;
    subname "<capture ".($myid).">" => sub {
      my $c = $_[0];
      my $m = Regexp::RAST::ReentrantEngine::Match->new();
      my $from = $Regexp::RAST::ReentrantEngine::Env::pos;
      my $close = subname '<capture-close '.($myid).">" => sub {
        my $c0 = $_[0];
        my $to = $Regexp::RAST::ReentrantEngine::Env::pos;
        $m->match_set(1,substr($Regexp::RAST::ReentrantEngine::Env::str,$from,$to-$from),[],{},$from,$to);
        TAILCALL(&$c0,$c);
      };
      return LET($Regexp::RAST::ReentrantEngine::Env::cap){
        $Regexp::RAST::ReentrantEngine::Env::cap = [@$Regexp::RAST::ReentrantEngine::Env::cap];
        $Regexp::RAST::ReentrantEngine::Env::cap->[$idx] = $m;
        my $v = $f->($close);
        $m->match_set_as_failed if FAILED($v);
        $v;
      }LET;
    };
  }
  sub RRRE_subrule {
    my($o,$fetch,$name,$args)=@_;
    my $noop = $o->RRRE_noop;
    my $f = undef;
    my $myid = $id++;
    subname "<subrule ".($myid).">" => sub {
      my($c)=@_;
      $f = $fetch->(@$args) if !defined $f;

      my $pos = $Regexp::RAST::ReentrantEngine::Env::pos;
      my $cap = $Regexp::RAST::ReentrantEngine::Env::cap;
      my $m0 = $Regexp::RAST::ReentrantEngine::Env::current_match;
      my $m1 = Regexp::RAST::ReentrantEngine::Match->new;
      $$m1->{'RULE'} ||= $name; #EEEP
      $m1->match_set(1,"",[],{},$pos,undef);

      my $rest = subname "<subrule-rest ".($myid).">" => sub {
	my $cn = $_[0];
	$$m1->{'match_array'} = $Regexp::RAST::ReentrantEngine::Env::cap; #EEEP
	$$m1->{'match_to'} = $Regexp::RAST::ReentrantEngine::Env::pos; #EEEP
	$$m1->{'match_string'} = substr($Regexp::RAST::ReentrantEngine::Env::str,$pos,$Regexp::RAST::ReentrantEngine::Env::pos-$pos);
	local $m0->{$name} = [@{$m0->{$name}||[]}];
	push(@{$m0->{$name}},$m1); #see below
	$Regexp::RAST::ReentrantEngine::Env::cap = $cap;
	$Regexp::RAST::ReentrantEngine::Env::current_match = $m0;
	TAILCALL(&$cn,$c);
      };

      my $v;
      { local $Regexp::RAST::ReentrantEngine::Env::current_match = $m1;
	local $Regexp::RAST::ReentrantEngine::Env::cap = [];
	$v = $f->($rest);
      }
      FAIL_IF_FAILED($v);
      unshift(@{$m0->{$name}},$m1);# sigh,
      # why twice?: once for inline code, once for the final Match tree.
      return $v;
    };
  }
  sub RRRE_do_match {
    my($o,$f,$s,$beginat,$minlen)=@_;
    my $nparen = $o->{nparen};
    my $len = length($s);
    $beginat = 0 if !defined($beginat);
    my $noop = $o->RRRE_noop;
    my $atend = $noop;
    if(defined $minlen) {
      my $min_end = $minlen + $beginat;
      $atend = subname "<atend ".($id++).">" => sub {return undef if $Regexp::RAST::ReentrantEngine::Env::pos < $min_end;return 1;}
    }
    for my $start ($beginat..$len) {
      local $Regexp::RAST::ReentrantEngine::Env::str = $s;
      local $Regexp::RAST::ReentrantEngine::Env::pos = $start;
      local $Regexp::RAST::ReentrantEngine::Env::cap = [];
      my $m = Regexp::RAST::ReentrantEngine::Match->new();
      local $Regexp::RAST::ReentrantEngine::Env::current_match = $m;
      my $ok = $f->($atend);
      if(not FAILED($ok)) {
        my $a = $Regexp::RAST::ReentrantEngine::Env::cap;
        if(defined($nparen) && $nparen > @$a) {
          for my $i (@$a..$nparen) {
            push(@$a,Regexp::RAST::ReentrantEngine::Match->new()->match_set_as_failed);
          }
        }
        for my $am (@$a) {
          $am = Regexp::RAST::ReentrantEngine::Match->new()->match_set_as_failed() if !defined($am);
        }
        $m->match_set(1,substr($Regexp::RAST::ReentrantEngine::Env::str,$start,$Regexp::RAST::ReentrantEngine::Env::pos-$start),
                      $a,\%{$m},$start,$Regexp::RAST::ReentrantEngine::Env::pos);
        return $m;
      }
    }
    return Regexp::RAST::ReentrantEngine::Match->new()->match_set_as_failed;
  }

}

{ 
  package Regexp::RAST::Pat5;

  sub RRRE_emit {
    my($o)=@_;
    my $re = $o->RRRE_wrap_re_with_mods($o->{pat});
    $o->RRRE_eat_regexp($re);
  }
}
{ # (?imsx-imsx:...)
  package Regexp::RAST::Mod_expr;

  sub RRRE_emit {
    my($o)=@_;
    $o->{expr}->RRRE_emit;
  }
}
{ # (?imsx-imsx)
  package Regexp::RAST::Mod_inline;

  sub RRRE_emit {
    my($o)=@_;
    $o->RRRE_noop;
  }
}
{
  package Regexp::RAST::Exact;

  sub RRRE_emit {
    my($o)=@_;
    my $re = $o->{text};
    $re =~ s/(\W)/\\$1/g;
    $re = $o->RRRE_wrap_re_with_mods($re);
    $o->RRRE_eat_regexp($re);
  }
}
{
  package Regexp::RAST::Quant;

  sub RRRE_emit {
    my($o)=@_;
    my($min,$max,$nongreedy)= (@$o{'min','max','nongreedy'});
    $min = 0 if !defined $min;
    $max = 1000**1000**1000 if !defined $max; #XXX inf
    my $f = $o->{expr}->RRRE_emit;
    $o->RRRE_repeat($f,$min,$max,$nongreedy);
  }
}

{
  package Regexp::RAST::Alt;

  sub RRRE_emit {
    my($o)=@_;
    $o->RRRE_alt([map{$_->RRRE_emit}@{$o->{exprs}}]);
  }
}

{
  package Regexp::RAST::Seq;

  sub RRRE_emit {
    my($o)=@_;
    $o->RRRE_concat($o->{exprs});
  }
}

{
  package Regexp::RAST::Cap5;

  sub RRRE_emit {
    my($o)=@_;
    my $idx = $o->{cap5_idx};
    my $f = $o->{expr}->RRRE_emit;
    $o->RRRE_capture($idx,$f);
  }
}
{
  package Regexp::RAST::Grp;

  sub RRRE_emit {
    my($o)=@_;
    $o->{expr}->RRRE_emit;
  }
}

{
  package Regexp::RAST::Subrule;
  use Sub::Name;

  sub RRRE_emit {
    my($o)=@_;
    my $args = $o->{args} || [];
    my $name = $o->{name};
    my $where = $Regexp::RAST::Namespace::_current_namespace_;
    $where = $o->{created_in_pkg} if !defined $where;
    my $fetch = subname "<subrule-fetch for $name in $where>" => sub {
      my $subname = "${where}::$name";
      no strict;
      my $f = $subname->('api0');
      use strict;
      die "assert" if !defined $f;
      $f;
    };
    $o->RRRE_subrule($fetch,$name,$args);
  }
}
{
  package Regexp::RAST::ARule;
  use Sub::Name;

  sub RRRE_emit {
    my($o)=@_;
    my $f = $o->{expr}->RRRE_emit;
    my $matcher = subname "<an arule-matcher for $o>" => sub {
      my($s,$beginat,$minlen)=@_;
      $o->RRRE_do_match($f,$s,$beginat,$minlen);
    };
    subname "<an arule for $o>" => sub {
      my($request)=@_;
      if(@_ == 0) { return $matcher }
      if($request eq 'api0') { return $f }
      if($request eq 'RRRE-tree') { return $o }
      Carp::confess("ui assert");
      die "ui assert";
    }
  }
}
{
  package Regexp::RAST::Bind;

  sub RRRE_emit {
    my($o)=@_;
    my $name = $o->{name};
    my $where = $Regexp::RAST::Namespace::_current_namespace_;
    $where = $o->{created_in_pkg} if !defined $where;
    local $Regexp::RAST::Namespace::_current_name_ = $name;
    my $f = $o->{expr}->RRRE_emit;
    eval("package $where; *$name = \$f"); die "assert" if $@;
    $f;
  }
}
{
  package Regexp::RAST::Namespace;

  sub RRRE_emit {
    my($o)=@_;
    my $pkg = $o->{created_in_pkg};
    my $name = $o->{name};
    my $where = $name =~ /^::(.*)/ ? $1 : $name eq '' ? $pkg : "$pkg::$name";
    eval("package $where;"); die "assert" if $@;
    local $Regexp::RAST::Namespace::_current_namespace_ = $where;
    map{$_->RRRE_emit;} @{$o->{bindings}};
    undef;
  }
}

#======================================================================

{
  package Regexp::RAST::ReentrantEngine::Match;

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
sub match__indent {my($o,$s)=@_; $s =~ s/^(?!\Z)/  /mg; $s}
sub match__indent_except_top {my($o,$s)=@_; $s =~ s/^(?<!\A)(?!\Z)/  /mg; $s}
sub match__describe_name_as {
  my($o)=@_;
  my $s = overload::StrVal($o);
  $s;
}
}

#======================================================================

{
  package Regexp::RAST::Make0;
  BEGIN{
  require Exporter;
  @Regexp::RAST::Make0::ISA=qw(Exporter);
  @Regexp::RAST::Make0::EXPORT_OK = qw(pat5 mod_expr mod_inline exact quant quant_ng alt seq cap5 grp sr arule bind namespace  ques star plus  ques_ng star_ng plus_ng  inf );
  @Regexp::RAST::Make0::EXPORT    = qw(pat5 mod_expr mod_inline exact quant quant_ng alt seq cap5 grp sr arule bind namespace  ques star plus  ques_ng star_ng plus_ng  inf );
  }
  sub pat5 { Regexp::RAST::Pat5->new(@_) }
  sub mod_expr { Regexp::RAST::Mod_expr->new(@_) }
  sub mod_inline { Regexp::RAST::Mod_inline->new(@_) }
  sub exact { Regexp::RAST::Exact->new(@_) }
  sub quant { Regexp::RAST::Quant->new(@_) }
  sub quant_ng { Regexp::RAST::Quant->new(@_,'ng') }
  sub alt { Regexp::RAST::Alt->new(@_) }
  sub seq { Regexp::RAST::Seq->new(@_) }
  sub cap5 { Regexp::RAST::Cap5->new(@_) }
  sub grp { Regexp::RAST::Grp->new(@_) }
  sub sr { my($pkg)=caller; Regexp::RAST::Subrule->new($pkg,shift,[@_]) }
  sub arule { Regexp::RAST::ARule->new(@_) }
  sub bind { my($pkg)=caller; Regexp::RAST::Bind->new($pkg,@_) }
  sub namespace { my($pkg)=caller; Regexp::RAST::Namespace->new($pkg,@_) }

  sub ques { quant(0,1,    (@_ > 1 ? seq(@_) : @_)); }
  sub star { quant(0,undef,(@_ > 1 ? seq(@_) : @_)); }
  sub plus { quant(1,undef,(@_ > 1 ? seq(@_) : @_)); }

  sub ques_ng { quant(0,1,    (@_ > 1 ? seq(@_) : @_)); }
  sub star_ng { quant(0,undef,(@_ > 1 ? seq(@_) : @_)); }
  sub plus_ng { quant(1,undef,(@_ > 1 ? seq(@_) : @_)); }

  sub inf () { 1000**1000**1000 } #XXX There has to be a better way, no?
}

{
  package Regexp::RAST::Base;
  sub RAST_init { Carp::confess "bug - unimplemented" }
}
{
  package Regexp::RAST::Pat5;
  @Regexp::RAST::Pat5::ISA=qw(Regexp::RAST::Base);
  sub new {
    my($cls,$pat)=@_; die "api assert" if @_ != 2;
    bless {pat=>$pat}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    $o->{flags} = {%$Regexp::RAST::Env::flags};
  }
}
{
  package Regexp::RAST::Exact;
  @Regexp::RAST::Exact::ISA=qw(Regexp::RAST::Base);
  sub new {
    my($cls,$text)=@_; die "api assert" if @_ != 2;
    bless {text=>$text}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    $o->{flags} = {%$Regexp::RAST::Env::flags};
  }
}
{
  package Regexp::RAST::Mod_expr;
  @Regexp::RAST::Mod_expr::ISA=qw(Regexp::RAST::Base);
  sub _new_hlp {
    my($cls,$modpat)=@_;
    my %m;
    for my $mod (split(":",$modpat)) {
      next if $mod eq '';
      $mod =~ /^(\w+)(?:[[(<](.*?)[])>])?$/ or die "assert";
      my($k,$v) = ($1,$2);
      $v = '1' if !defined $v;
      $v = eval($v);
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
    my $flags = {%$Regexp::RAST::Env::flags};
    foreach my $key (keys(%{$o->{mods}})) {
      $flags->{$key} = $o->{mods}{$key};
    }
    $flags;
  }
  sub RAST_init {
    my($o)=@_;
    local $Regexp::RAST::Env::flags = $o->_add_mods;
    $o->{expr}->RAST_init;
  }
}
{
  package Regexp::RAST::Mod_inline;
  @Regexp::RAST::Mod_inline::ISA=qw(Regexp::RAST::Mod_expr);#
  sub new {
    my($cls,$modpat)=@_; die "api assert" if @_ != 2;
    my $m = $cls->_new_hlp($modpat);
    bless {mods=>$m}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    $Regexp::RAST::Env::flags = $o->_add_mods;
  }
}
{
  package Regexp::RAST::Quant;
  @Regexp::RAST::Quant::ISA=qw(Regexp::RAST::Base);
  sub new {
    my($cls,$min,$max,$expr,$nongreedy)=@_; die "api assert" if @_ < 4||@_ > 5;
    bless {min=>$min,max=>$max,expr=>$expr,nongreedy=>$nongreedy}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    $o->{expr}->RAST_init;
  }
}
{
  package Regexp::RAST::Cap5;
  @Regexp::RAST::Cap5::ISA=qw(Regexp::RAST::Base);
  sub new {
    my($cls,$expr)=@_;
    bless {expr=>$expr}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    $o->{cap5_idx} = $Regexp::RAST::Env::nparen++;
    local $Regexp::RAST::Env::flags = {%$Regexp::RAST::Env::flags};
    $o->{expr}->RAST_init;
  }
}
{
  package Regexp::RAST::Grp;
  @Regexp::RAST::Grp::ISA=qw(Regexp::RAST::Base);
  sub new {
    my($cls,$expr)=@_;
    bless {expr=>$expr}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    local $Regexp::RAST::Env::flags = {%$Regexp::RAST::Env::flags};
    $o->{expr}->RAST_init;
  }
}
{
  package Regexp::RAST::Alt;
  @Regexp::RAST::Alt::ISA=qw(Regexp::RAST::Base);
  sub new {
    my($cls,@exprs)=@_;
    bless {exprs=>\@exprs}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    [map{$_->RAST_init} @{$o->{exprs}}];
  }
}
{
  package Regexp::RAST::Seq;
  @Regexp::RAST::Seq::ISA=qw(Regexp::RAST::Base);
  sub new {
    my($cls,@exprs)=@_;
    bless {exprs=>\@exprs}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    [map{$_->RAST_init} @{$o->{exprs}}];
  }
}

{
  package Regexp::RAST::Subrule;
  @Regexp::RAST::Subrule::ISA=qw(Regexp::RAST::Base);
  sub new {
    my($cls,$pkg,$name,$args)=@_; die "api assert" if @_ != 4;
    bless {created_in_pkg=>$pkg,name=>$name,args=>$args}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
  }
}
{
  package Regexp::RAST::ARule;
  @Regexp::RAST::ARule::ISA=qw(Regexp::RAST::Base);
  sub new {
    my($cls,$expr)=@_; die "api assert" if @_ != 2;
    bless {expr=>$expr}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    local $Regexp::RAST::Env::nparen = 0;
    local $Regexp::RAST::Env::flags = {};
    $o->{expr}->RAST_init;
    $o->{nparen} = $Regexp::RAST::Env::nparen;
  }
}
{
  package Regexp::RAST::Bind;
  @Regexp::RAST::Bind::ISA=qw(Regexp::RAST::Base);
  sub new {
    my($cls,$pkg,$name,$expr)=@_; die "api assert" if @_ != 4;
    bless {created_in_pkg=>$pkg,name=>$name,expr=>$expr}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    $o->{expr}->RAST_init;
  }
}
{
  package Regexp::RAST::Namespace;
  @Regexp::RAST::Namespace::ISA=qw(Regexp::RAST::Base);
  sub new {
    my($cls,$pkg,$name,@bindings)=@_; die "api assert" if @_ < 3;
    bless {created_in_pkg=>$pkg,name=>$name,bindings=>\@bindings}, $cls;
  }
  sub RAST_init {
    my($o)=@_;
    [map{$_->RAST_init} @{$o->{bindings}}];
  }
}

#======================================================================
package Regexp::ModuleA::P5;
BEGIN { Regexp::RAST::Make0->import; };
my $nonmeta = '[^[)({^$?*+\\\\\.|]';
namespace("",
	  bind('pattern',arule(sr('_pattern'))),
          bind('_pattern',arule(seq(sr('_non_alt'),star(exact('|'),sr('_non_alt'))))),
	  bind('_non_alt',arule(star(sr('_element')))),
	  bind('_element',arule(seq(sr('_non_quant'),ques(pat5('[?*+]\??|{\d+(?:,\d*)?}\??'))))),
	  bind('_non_quant',arule(alt(sr('_mod_inline'),sr('_mod_expr'),sr('_cap5'),sr('_paren'),sr('_charclass'),sr('_esc'),sr('_nonmeta'),sr('_passthru')))),
	  bind('_mod_inline',arule(pat5('\(\?[imsx-]+\)'))),
	  bind('_mod_expr',arule(seq(pat5('\(\?[imsx-]+:'),sr('_pattern'),exact(')')))),
	  bind('_paren',arule(seq(pat5('\((?!\?[imsx-]+:)\?'),sr('_pattern'),exact(')')))),
	  bind('_cap5',arule(seq(pat5('\((?!\?)'),sr('_pattern'),exact(')')))),
	  bind('_charclass',arule(pat5('\[\^?\]?([^\]\\\\]|\\\\.)*\]'))),
	  bind('_esc',arule(pat5('\\\\.|\.'))),#rename
	  bind('_nonmeta',arule(pat5("$nonmeta(?:$nonmeta+(?![?*+{]))?"))),
	  bind('_passthru',arule(pat5('[$^]')))
	  )->RRRE_emit;


sub match_tree_to_mexpr {
  my($m)=@_;
  my $mexpr = match_tree_to_mexpr_helper($m);
  if(!defined $mexpr) {
    return undef;
  }
  "arule($mexpr)";
}
#XXX blech:
sub match_tree_to_mexpr_helper {
  my($m)=@_;
  my $r = $$m->{RULE};
  my @v = map{match_tree_to_mexpr_helper($_)} map{@$_} values(%{$m});
  my @ret = @v;
  if(defined($r)) {
    if($r eq '_nonmeta') {
      @ret = ("exact('$m')");
    }
    if($r eq '_passthru') {
      @ret = ("pat5('$m')");
    }
    if($r eq '_element') {
      my $s = "$m";
      my($e)= @v;
      if($s =~ /([?*+])(\?)?$/) {
        my $ng = defined $2 ? '_ng' : '';
        $e = "ques${ng}($e)" if $1 eq '?';
        $e = "star${ng}($e)" if $1 eq '*';
        $e = "plus${ng}($e)" if $1 eq '+';
      }
      if($s =~ /{(\d+)(?:,(\d*))?}(\?)?$/) {
        my $ng = defined $3 ? '_ng' : '';
        my $min = $1;
        my $max = !defined $2 ? $min : $2 ne "" ? $2 : 1000**1000**1000; # inf
        $e = "quant${ng}($min,$max,$e)";
      }
      @ret = ($e);
    }
    if($r eq '_esc') {
      my $pat = "$m";
#      $pat =~ s/(\W)/\\\1/g;
      @ret = ("pat5('$pat')");
    }
    if($r eq '_charclass') {
      my $pat = "$m";
      @ret = ("pat5('$pat')");
    }
    if($r eq '_paren') {
      @ret = ("grp($v[0])");
    }
    if($r eq '_cap5') {
      @ret = ("cap5($v[0])");
    }
    if($r eq '_mod_expr') {
      "$m" =~ /^\(\?([imsx]*)(?:-([imsx]*))?/ or die 'bug';
      my $on  = join(":",split("",$1));
      my $off = join(":",map{"${_}(0)"}split("",defined $2 ? $2 : ""));
      @ret = ("mod_expr('$on:$off',$v[0])");
    }
    if($r eq '_mod_inline') {
      "$m" =~ /^\(\?([imsx]*)(?:-([imsx]*))?/ or die 'bug';
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
  # print $match->match_describe,"\n";
  my $m_exp = match_tree_to_mexpr($match);
  # print $m_exp,"\n";
  die "assert" if !defined $m_exp;
  my $ast = eval($m_exp);
  die if $@;
  $ast->RAST_init;
  my $matcher = $ast->RRRE_emit;
  wantarray ? ($match,$m_exp,$ast,$matcher) : $matcher;
}

#abc
#======================================================================

package Regexp::RAST::ReentrantEngine;

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
  $re = "(?$mods)$re" if $mods;
  $o->{regexp} = $re;
  my $n = eval { Regexp::ModuleA::P5::mk_matcher_from_re($re); };
  $o->{matcher} = $n;
  Carp::confess "compile \"$re\" failed: $@" if !defined $n;
  $o;
}
sub match_re {
  my($pat,$mods,$str)=@_;
  ($mods,$str) = (undef,$mods) if !defined $str;
  my $o = __PACKAGE__->new($pat,$mods);
  $o->match($str);
}
sub match {
  my($o,$str)=@_;
  $o->{matcher}()($str);
}

#======================================================================

sub Regexp::ModuleA::test_target {
  sub {
    my($mods,$re)=@_;
    my $o = Regexp::RAST::ReentrantEngine->new($re,$mods);
    sub{my($s)=@_;$o->match($s)}
  };
}
if(@ARGV && $ARGV[0] eq '--test') {
  require './t/re_tests.pl';
  Pkg_re_tests::test(&Regexp::ModuleA::test_target);
  exit;
}

package Regexp::ModuleA;
sub convert_p5_re_literal_to_p5_re {
  use re 'eval';
  my($lit5)=@_;
  $lit5 =~ s/^\s+//; $lit5 =~ s/\s+$//;

  my $modre = qr/[imsxogce]/;
  my %close = ('('=>qr/\)/,'{'=>qr/}/,'['=>qr/]/,'<'=>qr/>/);
  my $cl = sub{my $s = $_[0]; $close{$s}||qr/$s/ };
  my($op,$delim,$pat5,$delimC,$subst,$mod5);
  if($lit5 =~ /^()(\/)(.+?)(\/)()($modre*)$/) {
    ($op,$delim,$pat5,$delimC,$subst,$mod5)=($1,$2,$3,$4,$5,$6);
  }
  elsif($lit5 =~ /^(qr|m)(.)(.+?)((??{$cl->($2)}))()($modre*)$/) {
    ($op,$delim,$pat5,$delimC,$subst,$mod5)=($1,$2,$3,$4,$5,$6);
  }
# s///ubstitution is not supported.
#  elsif($lit5 =~ /^(s)(.)(.+?)((??{$cl->($2)}))\2?(.+?)\4($modre*)$/){
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
    if(/^(\/|(m|s|qr)\W)/) {
      $re = convert_p5_re_literal_to_p5_re($re);
      print "As regexp: $re\n";
    }
    my($match,$m_exp,$ast,$matcher) = Regexp::ModuleA::P5::mk_matcher_from_re($re);
    print "As m-expr: ",$m_exp,"\n";
    print "Enter string to match against.  Blank line to stop.\nstring: ";
    while(<>) {
      chomp;
      last if /^$/;
      print $matcher->()($_)->match_describe(),"\n";
      print "string: ";
    }
    print $prompt1;
  }
}
if(@ARGV && $ARGV[0] eq '--repl') {
  shift;
  repl;
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
