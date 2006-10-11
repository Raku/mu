
package Regexp::Parser::ConvertToSix;
use Regexp::Parser;

# Work around a Regex::Parser bug.  Author is not responding.
if($Regexp::Parser::VERSION <= 0.20) {
  eval <<'END';
#line 1 "Regexp::Parser bug fix/workaround"
    # Regexp::Parser bug fix/workaround
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

local $Regexp::Parser::ConvertToSix::env;

{
  package Regexp::Parser::__object__;

  sub _join_visual6 {
    my($o)=@_;
    join("",map { $_->visual6() } @{$o->data});
  }
  sub _backref {
    my($o)=@_;
    my $idx = $o->{nparen};
    if($Regexp::Parser::ConvertToSix::env->{preserve_capture_numbers}) {
      '$'.$idx;
    } else {
      $Regexp::Parser::ConvertToSix::env->{capture_map}{$idx};
    }
  }
}


{
  # \A ^ \B \b \G \Z \z $
  package Regexp::Parser::anchor;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->visual();
    my $flag_m = $o->{flags} & $o->{rx}->FLAG_m;
    return '^' if $pat eq '\A';
    return '$' if $pat eq '\z';
    return '\n?$' if $pat eq '\Z';
    return ($flag_m ? '^^' : '^') if $pat eq '^';
    return ($flag_m ? '$$' : '\n?$') if $pat eq '$';
    return '\B' if $pat eq '\B';
    return '\b' if $pat eq '\b';
    if($pat eq '\G') {
     $Regexp::Parser::ConvertToSix::env->{required_modifiers} .= ':pos';
      #XXX - /(?<=\G..)./g
      return '#(\G)';
    }
    die "didn't implement $pat"
  }
}
{
  # . \C
  package Regexp::Parser::reg_any;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->visual();
    if($pat eq '.') {
      my $flag_s = $o->{flags} & $o->{rx}->FLAG_s;
      $flag_s ? '.' : '\N';
    } elsif($pat eq '\C') {
      '[:bytes .]'
    } else {
      die "didn't implement $pat";
    }
  }
}
{
  # \w \W
  package Regexp::Parser::alnum;
  sub visual6 { shift->visual() }
  sub _can_be_inlined_in_character_list {1}
}
{
  # \s \S
  package Regexp::Parser::space;
  sub visual6 { shift->visual() }
  sub _can_be_inlined_in_character_list {1}
}
{
  # \d \D
  package Regexp::Parser::digit;
  sub visual6 { shift->visual() }
  sub _can_be_inlined_in_character_list {1}
}
{
  package Regexp::Parser::anyof;
  local $Regexp::Parser::ConvertToSix::CHARSET_SENSE = 0;
  sub visual6 {
    my($o)=@_;
    my $neg = $o->neg ? 1 : 0;
    my $op = $o->neg() ? '-' : '+';
    my $pat = "";
    my $cls = "";
    local $Regexp::Parser::ConvertToSix::CHARSET_SENSE = $neg;
    for my $n (@{$o->data}) {
      if($n->_can_be_inlined_in_character_list) {
        $cls .= $n->visual6();
      } else {
        $pat .= $op."[$cls]" if $cls ne '';
        $pat .= $n->visual6();
      }
    }
    $pat .= $op."[$cls]" if $cls ne '';
    $pat =~ s/^\+//;
    '<'.$pat.">";
  }
}
{
  package Regexp::Parser::anyof_char;
  sub visual6 { shift->visual() }
  sub _can_be_inlined_in_character_list {1}
}
{
  package Regexp::Parser::anyof_range;
  sub visual6 {
    my($o)=@_;
    my($lhs,$rhs) = map {$_->visual6()} @{$o->data};
    "$lhs..$rhs";
  }
  sub _can_be_inlined_in_character_list {1}
}
{
  package Regexp::Parser::anyof_class;
  sub visual6 {
    my($o)=@_;
    if($o->data eq 'POSIX') {
      my $sense = $Regexp::Parser::ConvertToSix::CHARSET_SENSE;
      die "assert: posix class used outside charclass" if !defined $sense;
      my $neg = ($o->neg ? 1 : 0) ^ ($sense || 0);
      my $sgn = $neg ? '-' : '+';
      $sgn.$o->{type}; # Not $o->type();
    } else {
      $o->data->visual6();
    }  
  }
  sub _can_be_inlined_in_character_list {
    my($o)=@_;
    return 0 if $o->data eq 'POSIX';
    if(ref($o->data) eq 'ARRAY') {
      for my $n (@{$o->data}) {
        return 0 if !$n->_can_be_inlined_in_character_list;
      }
      return 1;
    }
    return $o->data->_can_be_inlined_in_character_list;
  }
}
{
  # not needed
  package Regexp::Parser::anyof_close;
}
{
  package Regexp::Parser::prop;
  sub visual6 {
    my($o)=@_;
    my $sense = $Regexp::Parser::ConvertToSix::CHARSET_SENSE;
    my $inside_class = defined $sense;
    my $neg = ($o->neg ? 1 : 0) ^ ($sense || 0);
    my $sgn = $neg ? '-' : '+';
    my $name = $o->type;
    if($name =~ s/^is//i){
      $name = ucfirst $name;
      $name = "is".$name;
    }
    $inside_class ? $sgn.$name : '<'.$sgn.$name.'>';
  }
  sub _can_be_inlined_in_character_list {0}
}
{
  # \X
  package Regexp::Parser::clump;
  sub visual6 { "[<-isM><isM>*]" }
}
{
  # |
  package Regexp::Parser::branch;
  sub visual6 {
    my($o)=@_;
    my $env = $Regexp::Parser::ConvertToSix::env;
    my $first = $env->{capture_var}[-1];
    my $next_cap = $first;
    my $pat = join("|",map {
      my $a = $_;
      local $env->{capture_var} = [@{$env->{capture_var}}];
      $env->{capture_var}[-1] = $first;
      my $re = join("", map {$_->visual6()} @$a);
      my $cap = $env->{capture_var}[-1];
      $next_cap = $cap if $cap > $next_cap;
      $re;
    } @{$o->data});
    $env->{capture_var}[-1] = $next_cap;
    $pat;
  }
}
{
  package Regexp::Parser::exact;
  sub visual6{
    my($o)=@_;
    my $flag_x = $o->{flags} & $o->{rx}->FLAG_x;
    my $pat = "";
    for my $c (@{$o->{vis}}) {
      $c =~ s/^\\N{(.+)}$/\\c[$1]/;
      $c =~ s/^([^\w\s])$/\\$1/;
      $c =~ s/^([\s])$/\\$1/ if !$flag_x;
      $pat .= $c;
    }
    $pat;
  }
}
{
  package Regexp::Parser::quant;
  sub visual6 {
    my($o)=@_;
    my($min,$max)= ($o->min,$o->max);
    $min = 0 if $min eq "";
    my $pat = $o->data->visual6();
    my $type = $o->type;
    return $pat."*" if $type eq 'star';
    return $pat."+" if $type eq 'plus';
    return $pat."?" if $min == 0 && $max == 1;
    return $pat."**{$min..$max}";
  }
}
{
  # *? +? {,}?
  package Regexp::Parser::minmod;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->data->visual6();
    $pat."?";
  }
}
{
  # ( non-capturing
  package Regexp::Parser::group;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->_join_visual6();
    my $mod = "";
    $mod .= ":i " if $o->on =~ /i/;
    $mod .= ":i<0> " if $o->off =~ /i/;
    '['.$mod.$pat.']';
  }
}
{
  # ( capturing
  package Regexp::Parser::open;
  sub visual6 {
    my($o)=@_;
    my $pat;
    my $capture_var = $Regexp::Parser::ConvertToSix::env->{capture_var};
    {
      my $down = [@$capture_var,0];
      local $Regexp::Parser::ConvertToSix::env->{capture_var} = $down;
      $pat = $o->_join_visual6();
    }
    my $own_capture_var = [@$capture_var];
    $capture_var->[-1]++;

    my $idx = $o->{nparen};
    if($Regexp::Parser::ConvertToSix::env->{preserve_capture_numbers}) {
      $Regexp::Parser::ConvertToSix::env->{capture_map}{$idx} = $o->_capture($idx);
      " \$$idx := [$pat] ";
    } else {
      $Regexp::Parser::ConvertToSix::env->{capture_map}{$idx} = $o->_capture(@$own_capture_var);
      "($pat)";
    }
  }
  sub _capture {
    my($o,@cap)=@_;
    my $head = shift @cap;
    '$'.$head.join("",map {"[$_]"} @cap);
  }
}
{
  # ) closing
  # not needed
  package Regexp::Parser::close;
}
{
  # ) for non-captures
  # not needed
  package Regexp::Parser::tail;
}
{
  # \1 (backrefs)
  package Regexp::Parser::ref;
  sub visual6 {
    my($o)=@_;
    $o->_backref;
  }
}
{
  # not needed
  package Regexp::Parser::assertion;
}
{
  # (?=) (?<=)
  package Regexp::Parser::ifmatch;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->_join_visual6();
    my $dir = $o->{'dir'};
    $dir>0 ? "<?before $pat>" : "<?after $pat>";
  }
}
{
  # (?!) (?<!)
  package Regexp::Parser::unlessm;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->_join_visual6();
    my $dir = $o->{'dir'};
    $dir>0 ? "<?!before $pat>" : "<?!after $pat>";
  }
}
{
  # (?>)
  package Regexp::Parser::suspend;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->_join_visual6();
    "[$pat]:";
  }
}
{
  # (?(n)t|f)
  package Regexp::Parser::ifthen;
  sub visual6 {
    my($o)=@_;
    my $test = $o->data->[0]->visual6();
    my $crufty = $o->data->[1]->data;
    my $then = $crufty->[0][0]->visual6();
    my $else = "<null>";
    $else = $crufty->[1][0]->visual6() if @{$crufty} > 1;
    "<{$test ?? /$then/ :: /$else/}>";
  }
}
{
  # the N in (?(N)t|f) when N is a number
  package Regexp::Parser::groupp;
  sub visual6 {
    my($o)=@_;
    $o->_backref;
  }
}
{
  # (?{ ... })
  package Regexp::Parser::eval;
  sub visual6 {
    my($o)=@_;
    my $code = join("",$o->data);
    '{ eval(q{'.$code.'},:lang<perl5>) }';
  }
}
{
  # (??{ ... })
  package Regexp::Parser::logical;
  sub visual6 {
    my($o)=@_;
    my $code = join("",$o->data);
    '<{ eval(q{'.$code.'},:lang<perl5>) }>';
  }
}
{
  package Regexp::Parser::flags;
  sub visual6 {
    my($o)=@_;
    return ' :i ' if $o->on =~ /i/;
    return ' :i<0> ' if $o->off =~ /i/;
    return '';
  }
}

package Regexp::Parser;

sub convert_pattern_to_six {
  my($self,$preserve_capture_numbers)=@_;
  my $env = { preserve_capture_numbers => $preserve_capture_numbers,
              capture_map => {0=>'$/'},
              capture_var => [0],
              required_modifiers => "",
              avoid_delim => ""
          };
  $self->{ConvertToSix_env} = $env;
  local $Regexp::Parser::ConvertToSix::env = $env;
  join("",map{$_->visual6()} @{$self->root});
}

sub visual6 {
  my($self)=@_;
  $self->convert_pattern_to_six();
}

sub convert_string_with_match_vars {
  my($self,$str)=@_;
  my $slash = 0; # need_slash
  my $map = $self->{ConvertToSix_env}{capture_map};
  my $noesc = qr/(?<!\\)|(?<=\\\\)/;
  $str =~ s/$noesc\$(\d+)/$map->{$1} || ''/eg;
  $str =~ s/$noesc\$\-\[(\d+)\]/$slash++ if $1 == 0;'{'.$map->{$1}.'.from}'/eg;
  $str =~ s/$noesc\$\+\[(\d+)\]/$slash++ if $1 == 0;'{'.$map->{$1}.'.to}'/eg;
  $str =~ s/$noesc\$&/\$\//g and $slash++;
  $self->{ConvertToSix_env}{avoid_delim} .= '/' if $slash;
  $str;
}

sub convert_literal_to_six {
  use re 'eval';
  my($self,$lit5)=@_;
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
  elsif($lit5 =~ /^(s)(.)(.+?)((??{$cl->($2)}))\2?(.+?)\4($modre*)$/){
    ($op,$delim,$pat5,$delimC,$subst,$mod5)=($1,$2,$3,$4,$5,$6);
  }
  else { die "invalid literal: $lit5" }

  my $premod = $mod5;
  $premod =~ s/[^msx]//g;
  my $use_pat = "(?:$pat5)";
  $use_pat = "(?$premod)$use_pat" if $premod ne "";
  $self->regex($use_pat);
  my $pat = $self->convert_pattern_to_six();
  $pat =~ s/^\s*\[//; $pat =~ s/\]\s*$//;

  my $rest = "";
  if($op eq 's') {
    my $subst6 = $self->convert_string_with_match_vars($subst);

    my $avoid = qr/[ $self->{ConvertToSix_env}{avoid_delim}]/;
    if($delim =~ $avoid) {
      die 'assert' if '{' =~ $avoid;
      $delim = '{'; $delimC = '}';
    }

    $subst6 = "eval(q{$subst6},:lang<perl5>)" if $mod5 =~ /e/;
    $rest = ($delim eq $delimC ? '' : $delim).$subst6.$delimC;
  }

  my $mod = "";
  $mod .= $self->{ConvertToSix_env}{required_modifiers};
  $mod .= ':i' if $mod5 =~ /i/;
  $mod .= ':g' if $mod5 =~ /g/;
  $mod .= ':pos' if $mod5 =~ /c/;

  my $op6 .= ($op eq 's') ? 's' : ($delim eq '/' && $mod eq '') ? '' : 'rx';
  $op6.$mod.$delim.$pat.$delimC.$rest;
}


package main;
sub repl {
  use Data::Dumper;
  print "Enter a p5 regexp pattern or literal.\n";
  while(<>) {
    chomp;
    my $parser = Regexp::Parser->new($_);  
    print "Regenerated: "; print $parser->visual(),"\n";
#    print Dumper($parser->root); print $parser->visual(),"\n";
    print "As pattern (oddball): ";
    print $parser->convert_pattern_to_six(1),"\n";
    print "As pattern (normal):  ";
    print $parser->convert_pattern_to_six(),"\n";
    print "Captures:    ";
    print $parser->convert_string_with_match_vars('$1, $2, $3, $4, $5, $6'),"\n";
    print "As literal:  ";
    eval { print $parser->convert_literal_to_six($_),"\n"; } or print $@;
  }
}
repl;

=pod

BUGS

This module should be a class which inherits from Regexp::Parser, rather than infesting it.
\G handling - unclear how to support non-leftmost \G in p6.
Regexp::Parser does not support $^N $+ $' $` in patterns.

TODO

What do $' $` look like in p6?
Run over re_tests.
Test with //x.
Can we avoid losing comments?
The order dependency on avoid_delim is problematic.  Should be local()?  And it's fragile ('[').
More selective slashification.
Make it a module.

=cut
