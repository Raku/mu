#!/usr/bin/perl -w

package Regexp::ModuleA;
use strict;
use warnings;
use Carp;

#======================================================================
# Match
#
#-- delete.
   { package Regexp::ModuleA::ReentrantEngine::Match0;
#XXX Removing this next line seems to tickle a perlbug.
# Defer problem until we migrate onto elf's Match class.
     sub new_failed {my($cls)=@_; $cls->new()->match_set_as_failed()}
   }


#======================================================================
# AST analysis
#
#-- translate analysis, leave make0's.

{
  local $whiteboard::rx_pkg;
  local $whiteboard::rx_name;

  # RxBaseClass
  package IRx1::RxBaseClass;
  sub newx { shift->newp(undef,@_) }

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

  # RxPat5
  package IRx1::RxPat5;
  sub RAST_to_make0 {my($o)=@_; 'pat5('.($o->RAST_quote($o->{pat})).')';}

  # RxExact
  package IRx1::RxExact;
  sub RAST_to_make0 {my($o)=@_; 'exact('.($o->RAST_quote($o->{text})).')';}

  # RxMixinMod
  package IRx1::RxMixinMod;

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

  # RxMod_expr
  package IRx1::RxMod_expr;
  @IRx1::RxMod_expr::ISA=qw(IRx1::RxMixinMod IRx1::RxBaseClass);
  sub newx {
    my($cls,$modpat,$expr)=@_; die "api assert" if @_ != 3;
    my $mods = $cls->mods_from_modpat($modpat);
    bless {mods=>$mods,expr=>$expr}, $cls;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    'mod_expr('.$o->_RAST_to_make0_hlp.",".$o->RAST_to_make0_children.')';
  }
  
  # RxMod_inline
  package IRx1::RxMod_inline;
  @IRx1::RxMod_inline::ISA=qw(IRx1::RxMixinMod IRx1::RxBaseClass);
  sub newx {
    my($cls,$modpat)=@_; die "api assert" if @_ != 2;
    my $mods = $cls->mods_from_modpat($modpat);
    bless {mods=>$mods}, $cls;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    'mod_inline('.$o->_RAST_to_make0_hlp.')';
  }

  # RxBackref
  package IRx1::RxBackref;
  sub RAST_to_make0 {my($o)=@_; 'backref('.$o->{backref_n}.')';}

  # RxCap
  package IRx1::RxCap;

  # RxGrp
  package IRx1::RxGrp;

  # RxAlias
  package IRx1::RxAlias;
  sub newx {
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

  # RxQuant
  package IRx1::RxQuant;
  sub RAST_to_make0 {
    my($o)=@_;
    my $min = $o->{min}; $min = 'undef' if !defined $min;
    my $max = $o->{max}; $max = 'undef' if !defined $max;
    my $expr = $o->RAST_to_make0_children;
    my $ng = $o->{nongreedy}; $ng = defined $ng ? ",'ng'" : "";
    'quant('."$min,$max,$expr$ng".')';
  }

  # RxAlt
  package IRx1::RxAlt;
  sub newx {
    my($cls,@exprs)=@_;
    bless {exprs=>\@exprs}, $cls;
  }

  # RxConj
  package IRx1::RxConj;
  sub newx {
    my($cls,@exprs)=@_;
    bless {exprs=>\@exprs}, $cls;
  }

  # RxSeq
  package IRx1::RxSeq;
  sub newx {
    my($cls,@exprs)=@_;
    bless {exprs=>\@exprs}, $cls;
  }

  # RxASpace
  package IRx1::RxASpace;
  sub RAST_to_make0 {my($o)=@_; 'aspace('.($o->RAST_quote($o->{text})).')';}

  # RxSubrule
  package IRx1::RxSubrule;
  sub newx {
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

  # RxARegex
  package IRx1::RxARegex;
  @IRx1::RxARegex::ISA=qw(IRx1::RxMixinMod IRx1::RxBaseClass);
  sub newx {
    my($cls,$modpat,$expr)=@_; die "api assert" if @_ != 3;
    my $mods = $cls->mods_from_modpat($modpat);
    bless {modpat=>$modpat,mods=>$mods,expr=>$expr}, $cls;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    'aregexm('.$o->RAST_quote($o->{modpat}).','.$o->RAST_to_make0_children.')';
  }

  # RxBiind
  package IRx1::RxBiind;
  sub newx {
    my($cls,$inpkg,$name,$expr)=@_; die "api assert" if @_ != 4;
    die "api assert $name"  if $name =~ /::/;
    bless {created_in_pkg=>$inpkg,name=>$name,expr=>$expr}, $cls;
  }
  sub RAST_to_make0 {
    my($o)=@_;
    'biind('.$o->RAST_quote($o->{name}).','.$o->RAST_to_make0_children.')';
  }

  # RxNamespace
  package IRx1::RxNamespace;
  sub newx {
    my($cls,$inpkg,$nsname,@bindings)=@_; die "api assert" if @_ < 3;
    my $pkg = ($nsname =~ /\A::(.*)/) ? $1 : $nsname eq '' ? $inpkg : "${inpkg}::$nsname";
    bless {created_in_pkg=>$inpkg,nsname=>$nsname,bindings=>\@bindings,pkg=>$pkg}, $cls;
  }
  sub RAST_children { [@{shift->{bindings}}] }
  sub RAST_to_make0 {
    my($o)=@_;
    'namespace('.$o->RAST_quote($o->{nsname}).",\n".$o->RAST_to_make0_children.')';
  }

  # RxCode
  package IRx1::RxCode;
  sub RAST_to_make0 {
    my($o)=@_;
    'code('.$o->RAST_quote($o->{code}).')';
  }

  # RxCodeRx
  package IRx1::RxCodeRx;
  sub RAST_to_make0 {
    my($o)=@_;
    'coderx('.$o->RAST_quote($o->{code}).')';
  }

  # RxIndependent
  package IRx1::RxIndependent;
  sub RAST_to_make0 {my($o)=@_; 'independent('.$o->RAST_to_make0_children.')';}

  # RxConditional
  package IRx1::RxConditional;
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

  # RxLookaround
  package IRx1::RxLookaround;
  sub RAST_to_make0 {
    my($o)=@_;
    my $a = $o->{is_forward} ? '1' : '0';
    my $b = $o->{is_positive} ? '1' : '0';
    'lookaround('."$a,$b,".$o->RAST_to_make0_children.')';
  }

}


#======================================================================
# AST Make
# 
#-- For testing only.
{
  package Regexp::ModuleA::AST::Make0;
  require Exporter;
  @Regexp::ModuleA::AST::Make0::ISA=qw(Exporter);
  @Regexp::ModuleA::AST::Make0::EXPORT_OK = qw(pat5 mod_expr mod_inline exact quant quant_ng alt conj seq cap grp aspace sr alias aregex aregexm biind namespace  backref  ques star plus  ques_ng star_ng plus_ng  inf  code coderx independent conditional lookaround commit_sequence commit_group commit_regex commit_match);
  @Regexp::ModuleA::AST::Make0::EXPORT    = @Regexp::ModuleA::AST::Make0::EXPORT_OK;
  sub pat5 { IRx1::RxPat5->newx(@_) }
  sub mod_expr { IRx1::RxMod_expr->newx(@_) }
  sub mod_inline { IRx1::RxMod_inline->newx(@_) }
  sub exact { IRx1::RxExact->newx(@_) }
  sub quant { IRx1::RxQuant->newx(@_) }
  sub quant_ng { IRx1::RxQuant->newx(@_,'ng') }
  sub alt { IRx1::RxAlt->newx(@_) }
  sub conj { IRx1::RxConj->newx(@_) }
  sub seq { IRx1::RxSeq->newx(@_) }
  sub cap { IRx1::RxCap->newx(@_) }
  sub grp { IRx1::RxGrp->newx(@_) }
  sub aspace { my($pkg)=caller; IRx1::RxASpace->newx($pkg,@_) }
  sub sr { my($pkg)=caller; IRx1::RxSubrule->newx($pkg,shift,[@_]) }
  sub alias { IRx1::RxAlias->newx(@_) }
  sub aregex { IRx1::RxARegex->newx('',@_) }
  sub aregexm { IRx1::RxARegex->newx(@_) }
  sub biind { my($pkg)=caller; IRx1::RxBiind->newx($pkg,@_) }
  sub namespace { my($pkg)=caller; IRx1::RxNamespace->newx($pkg,@_) }

  sub backref { IRx1::RxBackref->newx(@_) }
  sub code { IRx1::RxCode->newx(@_) }
  sub coderx { IRx1::RxCodeRx->newx(@_) }
  sub independent { IRx1::RxIndependent->newx(@_) }
  sub conditional { IRx1::RxConditional->newx(@_) }
  sub lookaround { IRx1::RxLookaround->newx(@_) }
  sub commit_sequence { IRx1::RxCommitSequence->newx(@_) }
  sub commit_group { IRx1::RxCommitGroup->newx(@_) }
  sub commit_regex { IRx1::RxCommitRegex->newx(@_) }
  sub commit_match { IRx1::RxCommitMatch->newx(@_) }

  sub ques { quant(0,1,    (@_ > 1 ? seq(@_) : @_)); }
  sub star { quant(0,undef,(@_ > 1 ? seq(@_) : @_)); }
  sub plus { quant(1,undef,(@_ > 1 ? seq(@_) : @_)); }

  sub ques_ng { quant_ng(0,1,    (@_ > 1 ? seq(@_) : @_)); }
  sub star_ng { quant_ng(0,undef,(@_ > 1 ? seq(@_) : @_)); }
  sub plus_ng { quant_ng(1,undef,(@_ > 1 ? seq(@_) : @_)); }

  sub inf () { 1000**1000**1000 } #XXX There has to be a better way, no?
}
{
  package Regexp::ModuleA::AST::Make1;
  sub pat5 {shift; IRx1::RxPat5->newx(@_) }
  sub mod_expr {shift; IRx1::RxMod_expr->newx(@_) }
  sub mod_inline {shift; IRx1::RxMod_inline->newx(@_) }
  sub exact {shift; IRx1::RxExact->newx(@_) }
  sub quant {shift; IRx1::RxQuant->newx(@_) }
  sub quant_ng {shift; IRx1::RxQuant->newx(@_,'ng') }
  sub alt {shift; IRx1::RxAlt->newx(@_) }
  sub conj {shift; IRx1::RxConj->newx(@_) }
  sub seq {shift; IRx1::RxSeq->newx(@_) }
  sub cap {shift; IRx1::RxCap->newx(@_) }
  sub grp {shift; IRx1::RxGrp->newx(@_) }
  sub aspace { my($pkg)=caller; IRx1::RxASpace->newx($pkg,@_) }
  sub sr {my $pkg = shift; IRx1::RxSubrule->newx($pkg,shift,[@_]) }
  sub alias {shift; IRx1::RxAlias->newx(@_) }
  sub aregex {shift; IRx1::RxARegex->newx('',@_) }
  sub aregexm {shift; IRx1::RxARegex->newx(@_) }
  sub biind {my $pkg = shift; IRx1::RxBiind->newx($pkg,@_) }
  sub namespace {my $pkg = shift; IRx1::RxNamespace->newx($pkg,@_) }

  sub backref {shift; IRx1::RxBackref->newx(@_) }
  sub code {shift; IRx1::RxCode->newx(@_) }
  sub coderx {shift; IRx1::RxCodeRx->newx(@_) }
  sub independent {shift; IRx1::RxIndependent->newx(@_) }
  sub conditional {shift; IRx1::RxConditional->newx(@_) }
  sub lookaround {shift; IRx1::RxLookaround->newx(@_) }
  sub commit_sequence {shift; IRx1::RxCommitSequence->newx(@_) }
  sub commit_group {shift; IRx1::RxCommitGroup->newx(@_) }
  sub commit_regex {shift; IRx1::RxCommitRegex->newx(@_) }
  sub commit_match {shift; IRx1::RxCommitMatch->newx(@_) }

  sub ques {shift->quant(0,1,    (@_ > 1 ? seq(@_) : @_)); }
  sub star {shift->quant(0,undef,(@_ > 1 ? seq(@_) : @_)); }
  sub plus {shift->quant(1,undef,(@_ > 1 ? seq(@_) : @_)); }

  sub ques_ng {shift->quant_ng(0,1,    (@_ > 1 ? seq(@_) : @_)); }
  sub star_ng {shift->quant_ng(0,undef,(@_ > 1 ? seq(@_) : @_)); }
  sub plus_ng {shift->quant_ng(1,undef,(@_ > 1 ? seq(@_) : @_)); }

  sub inf {shift; 1000**1000**1000 }
}


#======================================================================
#-- For testing only.
package Regexp::ModuleA::Api::GatherMethodsA;
  require Exporter;
  @Regexp::ModuleA::Api::GatherMethodsA::ISA=qw(Exporter);
  @Regexp::ModuleA::Api::GatherMethodsA::EXPORT_OK = qw(gather_methods);
  @Regexp::ModuleA::Api::GatherMethodsA::EXPORT = @Regexp::ModuleA::Api::GatherMethodsA::EXPORT_OK;

sub gather_methods {
  my($cls,%args)=@_;
  for(keys %args){Carp::confess("invalid argument $_") if !/^(filter|pkg)$/;}
  $args{pkg} ||= $cls;
  $args{filter} ||= qr/^(.+)$/;
  my $filter = $args{filter};
  my $bottom_up;
  $bottom_up = sub {
    my(@pkgs)=@_;
    no strict 'refs';
    map{
      my $a = $_."::ISA";
      my $b = $_."::";
      my @isa = eval{@$a};
      ((!$@ ? &$bottom_up(@isa) : ()),keys(%$b));
    } reverse @pkgs;
  };
  my @meth_list = &$bottom_up($args{pkg});
  my %meth_map = map{$_ =~ $filter ? ($1,$_) : ()} @meth_list;
  \%meth_map;
}
1;
#======================================================================
# P5 Regexps
#
#-- For testing and reference.
package Regexp::ModuleA::P5;
Regexp::ModuleA::AST::Make0->import;
use Regexp::Common;
sub mod_helper {
  my($mod)=@_;
  my $h = {%$Regexp::ModuleA::ReentrantEngine::Env::nested_data};
  #my($on,$off) = split('-',$mod); # Can cause segfaults.
  my $idx = index($mod,"-");
  my($on,$off);
  if($idx < 0) { ($on,$off) = ($mod,undef) }
  else         { ($on,$off) = (substr($mod,0,$idx),substr($mod,$idx+1)) }
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
            )->RAST_init->RMARE_emit_and_eval;
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
      Carp::confess "$err / <== HERE $re/" if !$m || $m->from != 0; #XX should set beginat
      my $at = $m->to+1;
      Carp::confess "$err /".substr($re,0,$at)." <== HERE ".substr($re,$at)."/";
    }
    $mexpr = $cls->make0_from_match($m);
    die "assert" if !defined $mexpr;
    print STDERR $mexpr,"\n" if $verbose;
    $ast = eval("namespace('::$inpkg',$mexpr)");
    die if $@;
    $ast->RAST_init;
    #use Data::Dumper; print STDERR Dumper $ast;##
    my($rx) = $ast->RMARE_emit_and_eval;
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
#-- For testing and reference.
package Regexp::ModuleA::P5WithSubrules;
@Regexp::ModuleA::P5WithSubrules::ISA=qw(Regexp::ModuleA::P5);
Regexp::ModuleA::AST::Make0->import;

{
  my $nonmeta = '[^[)({^$?*+\\\\\.|<]';
  namespace(""
            ,biind('_subrule',aregex(seq(pat5('\<[?!]*\w+'),ques(seq(pat5('\s+'),plus(sr('pattern')))),exact('>'))))
            ,biind('_nonmeta',aregex(pat5("$nonmeta(?:$nonmeta+(?![?*+{]))?")))
            ,biind('test1',aregex(pat5('\w{2}')))
            )->RAST_init->RMARE_emit_and_eval;
}

sub make0_from_node___subrule {
  my($cls,$m)=@_;
  my @v = map{$cls->make0_from_node($_)} @{$m->{pattern}};
  "$m" =~ /\A<([?!]*(\w+))/ or die "bug";
  my $name = $1;
  my $args = (@v ? "," : "").join(",",map{"aregex($_)"}@v);
  return "sr('$name'$args)";
}


#======================================================================
# P6 Regexps
#
#======================================================================
#-- translate.

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
          )->RAST_init->RMARE_emit_and_eval;

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
  namespace("",nrx($name,pat5("\\p{$class}")))->RAST_init->RMARE_emit_and_eval;
  push(@Regexp::ModuleA::Api::PreludeA::EXPORT,$name);
  push(@Regexp::ModuleA::Api::PreludeA::EXPORT_OK,$name);
}
for my $class (@unicode_bidi_classes) {
  my $name = "isBidi$class";
  namespace("",nrx($name,pat5("\\p{BidiClass:$class}")))->RAST_init->RMARE_emit_and_eval;
  push(@Regexp::ModuleA::Api::PreludeA::EXPORT,$name);
  push(@Regexp::ModuleA::Api::PreludeA::EXPORT_OK,$name);
}
#XXX Lr - it's defined in propcharset.t, but its not in perlunicode.
namespace("",nrx('isLr',pat5("\\p{Ll}|\\p{Lu}|\\p{Lt}")))->RAST_init->RMARE_emit_and_eval;
push(@Regexp::ModuleA::Api::PreludeA::EXPORT,'isLr');
push(@Regexp::ModuleA::Api::PreludeA::EXPORT_OK,'isLr');

1;
#======================================================================
#-- For testing and reference.

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

            )->RAST_init->RMARE_emit_and_eval;
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

#======================================================================
# Api
#
#======================================================================
#-- For reference only.
package Regexp::ModuleA::Api::RegexApi0;
sub create {
  my($cls,$what,$name,$pat,%args)=@_;
  for(keys %args){Carp::confess("invalid argument $_") if !/^(env|pkg|mods)$/;}
  $args{pkg} ||= caller;
  $args{mods} = undef if !exists $args{mods};
  my $rx = Regexp::ModuleA::P6->new_rx_from_re($args{pkg},$pat,$args{mods});
  Regexp::ModuleA::P6->biind_rx($args{pkg},$name,$rx) if defined $name;
  $rx;
}
sub lookup {
  my($cls,$name,%args)=@_;
  for(keys %args){Carp::confess("invalid argument $_") if !/^(pkg)$/;}
  $args{pkg} ||= caller;
  $args{pkg}->${name}($name);
}
sub match {
  my($cls,$rx,$string,%args)=@_;
  for(keys %args){Carp::confess("invalid argument $_") if 1;}#!/^()$/;}
  $rx->match($string);
}
sub create_and_match {
  my($cls,$what,$name,$pat,$string,%args)=@_;
  $args{pkg} ||= caller;
  my $rx = $cls->create($what,$name,$pat,%args);
  $cls->match($rx,$string);
}

1;
#======================================================================
#-- For testing only.
# Command-line and glue. 
#
package main;

# Also used by t/re_tests.t.  Replace that with API, once it exists.
sub Regexp::ModuleA::test_target {
  sub {
    my($mods,$re)=@_;
    my $o = Regexp::ModuleA::P5->new_rx_from_re('main',$re,$mods);
    sub{my($s)=@_;$o->match($s)}
  };
}
sub Regexp::ModuleA::test_target6 {
  Regexp::ModuleA::Api::PreludeA->import;
  sub {
    my($mods,$re)=@_;
    my $o = Regexp::ModuleA::Api::RegexApi0->create('regex',undef,$re,mods=>$mods);
    sub{my($s)=@_;$o->match($s)}
  };
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
