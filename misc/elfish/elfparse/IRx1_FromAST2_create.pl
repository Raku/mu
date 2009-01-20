#!/usr/bin/perl
use strict;
use warnings;

my $output_file = "./IRx1_FromAST2.pm";
my $def = <<'END_DEF';
comp_unit
CompUnit.newp($m<statementlist>)

statementlist
$m<statement>

statement
my $labels = $m<label>;
my $result = $m<EXPR> || $m<statement_control>;
if $o<EXPR> && ($o<statement_mod_loop>.elems || $o<statement_mod_cond>.elems) {
  temp $blackboard::statement_expr = $result;
  $result = $m<statement_mod_loop>[0] || $m<statement_mod_cond>[0];
  if $o<mod_condloop> { #XXX still exists?
    $blackboard::statement_expr = $result;
    $result = $m<mod_condloop>;
  }
}
if $labels {
  Label.newp($labels,$result);
} else {
  $result;
}

nulltermish
$m<termish>[0]

termish
if $o<quantified_atom> {
  my $atoms = $m<quantified_atom>;
  if $atoms.elems == 1 {
    return $atoms[0];
  } else {
    return RxSeq.newp($atoms);
  }
}
my $noun = $m<noun>;
temp $blackboard::expect_term_base = $noun;
my $ops = [];
if $o<PRE> { $ops.push($o<PRE>.flatten) };
if $o<POST> { $ops.push($o<POST>.flatten) };
for $ops {
  $blackboard::expect_term_base = ir($_)
}
$blackboard::expect_term_base

EXPR
if $o<infix> {
  my $op = $m<infix><sym_name>;
  my $args = [$m<left>,$m<right>];
  if $op eq '=>' {
    if $args[2] { die "chained => unimplemented" }
    Pair.newp($args[0],$args[1])
  }
  elsif $op eq '=' && $args[0].WHAT.re_matchp('VarDecl') {
    # To simplify elf_h STD_red vs STD_blue diffs.
    $args[0].default_expr = $args[1];
    $args[0];
  }
  else {
    my $ret = Apply.newp("infix:"~$op,Capture.newp1($args));
    if $o<sym_name> eq '=' && $op ne '=' {
      $ret = Apply.newp("infix:"~"=",Capture.newp1([$args[0],$ret]));
    }
    $ret;
  }
}
elsif $o<prefix> {
  temp $blackboard::expect_term_base = $m<arg>;
  $m<prefix>
}
elsif $o<chain> {
  my $chain = $m<chain>;
  my $op = $o<chain>[1]<sym_name>;
  my $args = [$chain[0],$chain[2]];
  Apply.newp("infix:"~$op,Capture.newp1($args))
}
elsif $o<list> {
  my $op = $o<delims>[0]<sym_name>;
  my $args = $m<list>;
  if $o<delims>[0]<rxinfix> {
    if $op eq '|' {
      RxAlt.newp($args)
    } else {
      die "Unimplemented regex list operator: "~$op;
    }
  } else {
    Apply.newp("infix:"~$op,Capture.newp1($args))
  }
}
elsif $o<value> {
  $m<value>
}
elsif $o<noun> {
  $m<noun>
}
elsif $o<dotty> {
  temp $blackboard::expect_term_base = $m<arg>;
  $m<dotty>;
}
elsif $o<postop> {
  temp $blackboard::expect_term_base = $m<arg>;
  $m<postop>;
}
elsif $o<quantified_atom> {
  my $seq = $m<quantified_atom>;
  my $n = $seq.elems;
  if $n == 1 { $seq[0] }
  elsif $n > 1 { RxSeq.newp($seq) }
  else { undef }
}
else { die "Didn't understand an EXPR node" }

PRE
$m<prefix>

POST
$m<postop> || $m<dotty>

postop
$m<postfix> || $m<postcircumfix>

prefix
my $op = *text*;
my $name = "prefix:"~$op;
if $op eq 'temp' {
  my $scope = 'temp';
  my $typenames = undef;
  my $variable = $blackboard::expect_term_base;
  my $traits = undef;
  my $default_value = undef;
  return VarDecl.newp($scope,$typenames,undef,$variable,undef,$traits,'=',$default_value)
}
if $op.re_matchp('\A\w+\z') { # XXX elf_h compatability kludge.  prefix:not->not
  $name = $op;
  my $base = $blackboard::expect_term_base;
  if ($base.WHAT eq 'IRx1::Apply' &&
      $base.function eq 'circumfix:( )' &&
      $base.capture.arguments.elems == 1)
  {
    $blackboard::expect_term_base = $base.capture.arguments[0];
  }
}
Apply.newp($name,Capture.newp1([$blackboard::expect_term_base]))

postfix
my $op = *text*;
Apply.newp("postfix:"~$op,Capture.newp1([$blackboard::expect_term_base]))

postcircumfix
my $name = $m<sym_name>;
my $ident = "postcircumfix:"~$name;
my $args = $m<semilist> || $m<nibble>;
if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
Call.newp($blackboard::expect_term_base,$ident,Capture.newp1($args||[]))

circumfix
my $op = $m<sym_name>;
if $op eq '{ }' {
  if ($o<pblock> &&
      $o<pblock><lambda>.elems == 0 &&
      $o<pblock><signature>.elems == 0 &&
      $o<pblock><block> &&
      $o<pblock><block><statementlist><statement>.elems == 0) {
    return Hash.newp($m<pblock><block><statementlist><statement>)
  }
}
my $args = $m<semilist>;
if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
Apply.newp("circumfix:"~$op,Capture.newp1($args||[]))

circumfix:pblock
if $o<block><statementlist>.elems == 0 or $o<block><statementlist>[0].match_string.re_matchp('^:') {
  Hash.newp($m<block><statementlist>)
} elsif $o<block><statementlist>[0]<EXPR> and $o<block><statementlist>[0]<EXPR><sym> and $o<block><statementlist>[0]<EXPR><sym> eq "," { # XXX Not p6.  Remove once off elf_e, and Match updated.
  Hash.newp($m<block><statementlist>)
} elsif $o<block><statementlist>[0]<EXPR> and $o<block><statementlist>[0]<EXPR><sym> and $o<block><statementlist>[0]<EXPR><sym> eq "=>" {
  Hash.newp($m<block><statementlist>)
} elsif not($m<lambda>) and not($m<signature>) {
  $m<block>
} else {
  die "AST handler circumfix:pblock partially unimplemented";
}


infix
my $op = $m<sym_name>;
$op;

semilist
$m<statement>

dotty
#temp $blackboard::dottyop = $m<sym_name>;
$m<dottyop>

dottyop
$m<methodop> || $m<postop>

methodop
my $args = $m<semilist>[0];
Call.newp($blackboard::expect_term_base,$m<longname>,Capture.newp1($args||[]))

noun
temp $blackboard::is_proto; #X placement uncertain.
#Should just be *1*, but all versions of node contain a colonpair too.
$m<fatarrow> || $m<variable> || $m<package_declarator> || $m<scope_declarator> || $m<multi_declarator> || $m<routine_declarator> || $m<regex_declarator> || $m<type_declarator> || $m<circumfix> || $m<dotty> || $m<value> || $m<capterm> || $m<sigterm> || $m<term> || $m<statement_prefix> || $m<colonpair>[0]

infixish
$m<colonpair> || $m<infix> || $m<infix_prefix_meta_operator> || $m<infix_circumfix_meta_operator> || $m<infix>

desigilname
$m<longname>

deflongname
my $suffix = "";
if $o<colonpair> && $o<colonpair>[0] {
  my $k = $o<colonpair>[0]<k>;
  my $v = $o<colonpair>[0]<v>;
  if $v eq '1' { # infix:foo
    $suffix = ':'~$k;
  }
  elsif $k eq 'sym' {  # infix:sym<foo>
    $suffix = ':'~$o<colonpair>[0]<v><nibble><nibbles>[0];
  }
  else { # infix:<foo>
    $suffix = ':'~$o<colonpair>[0]<v><nibble><nibbles>[0];
  }
}
$m<name>~$suffix

longname
$m<name>

name
my $parts = [$m<identifier>];
if $m<morename> { $parts.push($m<morename>.flatten) }
$parts.join("::")

morename
$m<identifier>[0]

identifier
*text*


value
*1*

number
*1*

integer
NumInt.newp(*text*,10)

dec_number
NumInt.newp(*text*,10)

fatarrow
Pair.newp($m<key>,$m<val>)


term
my $text = $m<sym_name>;
if $text eq '*' {
  Apply.newp('whatever',Capture.newp1([]))
} else {
  Apply.newp($text,Capture.newp1([]))
}

term:name
*text*

term:identifier
my $ident = $m<identifier>;
my $args;
if $o<args><semilist> {
  $args = $m<args><semilist>;
}
elsif defined($o<args><listopargs>) {
  if ($ident.re_matchp('\A[A-Z][:\w]*\z') &&
      $o<args><listopargs>.elems == 0)
  { # Typenames.  Foo.new();  # STD_red elf_h compatibility.
    return $ident;
  }
  if $o<args><listopargs>.elems {
    $args = ir($o<args><listopargs>[0]<EXPR>);
    if $args.WHAT ne 'Array' { $args = [$args] }
  } else {
    $args = [];
  }
}
else { die "Didn't understand a term:identifier node" }
Apply.newp($ident,Capture.newp1($args||[]))


statement_control:use
Use.newp('use',$m<module_name>,$m<EXPR>)

module_name:depreciated
*text*

module_name:normal
$m<longname>

role_name
*text*

quote:/
die("quote:/ is unimplemented"); #"

quote:s
my $left = RxARegex.newp("",{},$m<pat><left>);
temp $blackboard::quote = '" "';
my $right = $m<pat><right>;
Apply.newp('s',Capture.newp1([$left,$right]));

quote:m
RxARegex.newp("",{},$m<quibble><nibble>);

quote
temp $blackboard::quote = $o<sym_name>;
my $q = $o<sym_name>;
if $q eq '/ /' {
  return RxARegex.newp("",{},$m<nibble>);
}
if $o<quote_mod> && $o<quote_mod><sym_name> eq 'w' {
  my $s = $m<quibble><nibble><nibbles>[0];
  my $a = $s.re_gsub('^\s+','').re_gsub('\s+$','').split('\s+');
  $a = $a.map(sub ($x){Buf.newp($x)});
  return Apply.newp("circumfix:[ ]",Capture.newp1($a));
}
my $nibs = $m<nibble><nibbles>;
my $args = $nibs.map(sub ($x){
  if $x.WHAT eq 'Str' {Buf.newp($x);}
  else {$x}
});
if $args.elems < 2 && $nibs[0].WHAT ne 'Str' { $args.push(Buf.newp("")) }
my $tmp = $args.shift;
for $args {
  if $tmp.WHAT eq 'IRx1::Buf' && $_.WHAT eq 'IRx1::Buf' {
    $tmp = Buf.newp($tmp.buf ~ $_.buf);
  } else {
    $tmp = Apply.newp('infix:~',Capture.newp1([$tmp,$_]));
  }
}
$tmp

#nibbles:\
#my $which = $m<item><sym_name>;
#if $which eq 'n' { "\n" }
#elsif $which eq 't' { "\t" }
#else { $which }

#nibbles
#$m<variable>

escape
my $e = *text*;
if $blackboard::quote eq "' '" {
  if    $e eq '\\\\' { Buf.newp('\\') }
  elsif $e eq '\\\'' { Buf.newp("'") }
  else { Buf.newp($e) }
}
elsif $blackboard::quote eq '" "' {
  if    $e eq '\n' { Buf.newp("\n") }
  elsif $e eq '\t' { Buf.newp("\t") }
  else {
    my $g = $e.re_groups('\A\\\\(.)\z');
    if $g { Buf.newp($g[0]) }
    elsif $o<variable> { $m<variable> }
    elsif $o<item> { $m<item> }
    else { die "Unsupported qq escape: "~$e }
  }
}
else { die "Unsupported quote: "~$blackboard::quote }

backslash:x
my $xNNNN = *text*;
my $s = ('0'~$xNNNN).hex.chr;
Buf.newp($s)


nibbler
#XXX I've sooo no idea.
if $o<nibbles> {
  #X $m<nibbles>[0]
  *text*
}
elsif $o<EXPR> {
  $m<EXPR>
}
else {
  die "nibbler is a work in progress";
}

colonpair
my $v = $m<v>;
if $o<v>.isa('Match') && $o<v><nibble> { #XXX :x<2> bypass postcircumfix:< >.
  $v = $o<v><nibble><nibbles>[0];
  $v = Buf.newp($v)
}
my $k = $m<k>; # or $m<identifier> ?
Pair.newp($k,$v)


scope_declarator:my
temp $blackboard::scope = 'my';
$m<scoped>

scope_declarator:has
temp $blackboard::scope = 'has';
$m<scoped>

scope_declarator:our
temp $blackboard::scope = 'our';
$m<scoped>

scope_declarator:temp
temp $blackboard::scope = 'temp';
$m<scoped>

scope_declarator:constant
temp $blackboard::scope = 'constant';
$m<scoped>

scoped
temp $blackboard::typenames = $m<fulltypename>;
$m<declarator>

declarator
$m<variable_declarator> || $m<signature> || $m<routine_declarator> || $m<regex_declarator> || $m<type_declarator>

variable_declarator
my $scope = $blackboard::scope; temp $blackboard::scope;
my $typenames = $blackboard::typenames; temp $blackboard::typenames = undef;
VarDecl.newp($scope,$typenames,undef,$m<variable>,undef,$m<trait>,'=',$m<default_value>)
#XXX default_value is going to take some non-local work.

variable
my $tw = $m<twigil>[0] || "";
if $tw eq "." && $o<postcircumfix>.elems {
  my $slf = Apply.newp('self',Capture.newp1([]));
  my $args = ir($o<postcircumfix>[0]<semilist>);
  return Call.newp($slf,$m<desigilname>,Capture.newp1($args||[]))
}
my $v;
if $o<desigilname> {
  $v = Var.newp($m<sigil>,$tw,$m<desigilname>);
} elsif $o<special_variable> {
  $v = $m<special_variable>;
} else { die "Unimplemented variable form" }
if $o<postcircumfix>.elems {
  temp $blackboard::expect_term_base = $v;
  $m<postcircumfix>;
} else {
  $v;  
}

sigil
*text*

twigil
*text*

special_variable
my $v = *text*;
my $s = substr($v,0,1);
my $n = substr($v,1,$v.chars-1);
Var.newp($s,undef,$n)


modifier_expr
$m<EXPR>

statement_control:BEGIN
ClosureTrait.newp('BEGIN',$m<block>)

statement_control:for
For.newp($m<xblock><EXPR>,$m<xblock>)

statement_mod_loop:for
For.newp($m<modifier_expr>,$blackboard::statement_expr)

statement_control:while
Loop.newp($m<xblock><EXPR>,$m<xblock><pblock>,undef,undef)

statement_mod_loop:while
Loop.newp($m<modifier_expr>,$blackboard::statement_expr,undef,undef)

statement_control:until
my $test = Apply.newp("not",Capture.newp1([$m<EXPR>]));
Loop.newp($test,$m<block>,undef,undef)

statement_mod_loop:until
my $test = Apply.newp("not",Capture.newp1([$m<modifier_expr>]));
Loop.newp($test,$blackboard::statement_expr,undef,undef)

statement_control:loop
my $e1 = $m<loop_eee><loop_e1>;
my $e2 = $m<loop_eee><loop_e2>;
my $e3 = $m<loop_eee><loop_e3>;
my $block = $m<loop_block>;
my $body = Loop.newp($e2,Block.newp([$block,$e3]),undef,undef);
Block.newp([$e1,$body])

statement_control:if
my $if_expr = $m<xblock><EXPR>;
my $if_block = $m<xblock><pblock>;
my $els;
if maybe($o<else>) { $els = ir($o<else>[0]<pblock>) }
my $elsif = $m<elsif>||[];
Cond.newp([[$if_expr,$if_block]].push($elsif.flatten),$els,undef)

statement_control__S_036if_elsif
[$m<xblock><EXPR>,$m<xblock><pblock>]


statement_mod_cond:if
Cond.newp([[$m<modifier_expr>,$blackboard::statement_expr]],undef,undef)

statement_control:unless
Cond.newp([[$m<EXPR>,$m<block>]],undef,1)

statement_mod_cond:unless
Cond.newp([[$m<modifier_expr>,$blackboard::statement_expr]],undef,1)


statement_control:given
Given.newp($m<EXPR>,$m<block>)

statement_mod_loop:given
Given.newp($m<modifier_expr>,$blackboard::statement_expr)

statement_control:when
When.newp($m<EXPR>,$m<block>)

statement_mod_cond:when
When.newp($m<modifier_expr>,$blackboard::statement_expr)

statement_control:default
When.newp(undef,$m<block>)


statement_prefix:do
Apply.newp("statement_prefix:do",Capture.newp1([$m<statement>]))

statement_prefix:try
Apply.newp("statement_prefix:try",Capture.newp1([$m<statement>]))

statement_prefix:gather
Apply.newp("statement_prefix:gather",Capture.newp1([$m<statement>]))

statement_prefix:contend
Apply.newp("statement_prefix:contend",Capture.newp1([$m<statement>]))

statement_prefix:async
Apply.newp("statement_prefix:async",Capture.newp1([$m<statement>]))

statement_prefix:lazy
Apply.newp("statement_prefix:lazy",Capture.newp1([$m<statement>]))


pblock
if $o<signature>.elems {
  SubDecl.newp(undef,undef,undef,undef,$m<signature>[0],undef,$m<block>)
} else {
  $m<block>
}

block
Block.newp($m<statementlist>)

xblock
$m<pblock>

multi_declarator:multi
temp $blackboard::plurality = 'multi';
$m<declarator> || $m<routine_def>

multi_declarator:proto
temp $blackboard::is_proto = 1;
$m<declarator>

routine_declarator:sub
$m<routine_def>

routine_declarator:method
$m<method_def>

method_def
my $plurality = $blackboard::plurality; temp $blackboard::plurality;
my $multisig = $m<multisig>;
if $multisig.elems == 0 { $multisig = [Signature.newp([],undef)]; }
MethodDecl.newp(undef,undef,$plurality,$m<longname>,$multisig.[0],maybe($m<trait>),$m<block>,undef,undef)

routine_def
my $scope = $blackboard::scope; temp $blackboard::scope;
my $plurality = $blackboard::plurality; temp $blackboard::plurality;
my $ident = "";
if $o<deflongname>.elems { $ident = $m<deflongname>[0]  };
if ($ident && not($scope)) { $scope = "our" };
my $sig = Signature.newp([],undef);
if $o<multisig>.elems { $sig = ir($o<multisig>[0]) };
SubDecl.newp($scope,undef,$plurality,$ident,$sig,maybe($m<trait>),$m<block>)

multisig
$m<signature>[0]

signature
Signature.newp($m<parameter>,undef)

parameter
my $var = $m<param_var>;
my $quantchar = $m<quant>;
my $type_constraint = $m<type_constraint>;
#X gimme5 is emitting two copies of the constraint.
if $type_constraint && $type_constraint.elems == 2 { $type_constraint.pop }
#XXX elf_h backcompat with STD_red, and elf_h's 'if [] { incorrectly true }'.
if $type_constraint.elems == 0 { $type_constraint = undef }
my $default_value = $m<default_value>[0];
Parameter.newp($type_constraint,$quantchar,$var,undef,undef,undef,$default_value)

default_value
$m<EXPR>

param_var
ParamVar.newp($m<sigil>,$m<twigil>[0]||"",$m<identifier>[0])

type_constraint
$m<fulltypename>

fulltypename
$m<typename>[0]

typename
$m<longname>


capture
if not($o<EXPR>) {
  Capture.newp1([])
}
elsif $o<EXPR><noun> {
  Capture.newp1([$m<EXPR><noun>])
}
elsif $o<EXPR><sym> && $o<EXPR><sym> eq ':' {
  my $args = $m<EXPR><args>;
  my $inv = $args.shift;
  Capture.newp($args||[],$inv)
}
elsif $o<EXPR><sym> && $o<EXPR><sym> eq ',' {
  my $args = $o<EXPR><args>;
  my $arg0 = $args && $args[0];
  my $inv = undef;
  if $arg0 && $arg0<sym> && $arg0<sym> eq ':' {
    $args.shift;
    $inv = $arg0<args>[0];
    if $arg0<args>[1] {
      $args.unshift($arg0<args>[1]);
    }
  }
  Capture.newp(ir($args)||[],ir($inv))
}
else { die "capture AST form not recognized" }


package_declarator:role
temp $blackboard::package_declarator = 'role';
$m<package_def>

package_declarator:class
temp $blackboard::package_declarator = 'class';
$m<package_def>

package_declarator:module
temp $blackboard::package_declarator = 'module';
$m<package_def>

package_declarator:package
temp $blackboard::package_declarator = 'package';
$m<package_def>

package_declarator:grammar
temp $blackboard::package_declarator = 'grammar';
$m<package_def>

package_def
PackageDecl.newp(undef,undef,$blackboard::package_declarator,$m<module_name>.[0],maybe($m<trait>),$m<block>)

trait
$m<trait_auxiliary>

trait_auxiliary:is
Trait.newp('is',$m<longname>)

trait_auxiliary:does
Trait.newp('does',$m<module_name>)



regex_declarator
my $kind = $m<sym_name>;
temp $blackboard::regex_kind = $kind;
$m<regex_def>



regex_def
my $kind = $blackboard::regex_kind;
my $name = $m<deflongname>[0];
my $sig = $m<signature>; #X
my $trait = $m<trait>; #X
my $regex = $m<regex_block>;
if $blackboard::is_proto { # category decl
  my $regex = RxCategory.newp($name);
  my $rx = RxBiind.newp(undef,$name,RxARegex.newp('',{},$regex));
  my $def = RegexDef.newp($kind,$name,$sig,$trait,$rx);
  return RegexCategoryDecl.newp($name,$def);
}
my $rx = RxBiind.newp(undef,$name,RxARegex.newp('',{},$regex));
RegexDef.newp($kind,$name,$sig,$trait,$rx)


regex_block
$m<nibble>

quantified_atom
my $quant = $m<quantifier>[0];
my $atom = $m<atom>;
my $g;
if not($quant) { return $atom; }
elsif ($g = $quant.re_groups('{(\d+)(?:,(\d*))?}(\?)?\z')) {
  my $ng = $g[2];
  my $min = $g[0];
  my $max = $g[1]; if !defined($max) { $max = $min } elsif $max eq '' { $max = 1000**1000**1000 }; #XXX inf
  RxQuant.newp($min,$max,$atom,$ng)
}
elsif ($g = $quant.re_groups('([?*+])(\?)?\z')) {
  my $ng = $g[1];
  my $op = $g[0];
  if    $op eq '?' { RxQuant.newp(0,1,$atom,$ng) }
  elsif $op eq '*' { RxQuant.newp(0,undef,$atom,$ng) }
  elsif $op eq '+' { RxQuant.newp(1,undef,$atom,$ng) }
  else { die "bug" }
}
else {
  die "quantified_atom incompletely implemented";
}

atom
if $o<metachar> {
  $m<metachar>
}
else {
  my $char = *text*;
  RxExact.newp($char)
}

quantifier
*text*


metachar
my $x = *text*;
if $o<sym_name> { $x = $m<sym_name> }
if $o<mod_internal> {
  temp $blackboard::is_P5 = $blackboard::is_P5;
  my $mod = $m<mod_internal>;
  my $exprs = [$mod];
  if $o<mod_internal><nibbler> {
    my $rest = $m<mod_internal><nibbler><EXPR>;
    if $rest.WHAT ne 'Array' { $rest = [$rest] }
    $exprs = $exprs.concat($rest);
  }
  RxSeq.newp($exprs);
} elsif $x eq '.' { RxPat5.newp('.')
} elsif $x eq '^' {
  if $blackboard::is_P5 { RxPat5.newp('^') } else { RxPat5.newp('\A') }
} elsif $x eq '$' {
  if $blackboard::is_P5 { RxPat5.newp('$') } else { RxPat5.newp('\z') }
} elsif $x eq '^^' { RxPat5.newp('(?m:^)(?!(?=\z)(?<=\n))')
} elsif $x eq '$$' { RxPat5.newp('(?m:$)(?!(?=\z)(?<=\n))')
} elsif $x eq '< >' {
  if $o<assertion><sym_name> && $o<assertion><sym_name> eq '[' {
    my $v = $o<assertion><cclass_elem>; #X $o not $m, for $op below.
    my $inc=[]; my $excl=[];
    $v.map(sub ($opset){
      my $op = '+'; #XXX not in tree. :(
      my $set = irbuild_ir($opset);
      if $op eq '-' { $excl.push($set) }
      else { $inc.push($set) }
    });
    my $ast;
    if $inc.elems == 0 {
      $ast = RxPat5.newp('(?s:.)');
    } elsif $inc.elems == 1 {
      $ast = $inc[0];
    } else {
      $ast = RxAlt.newp($inc);
    }
    if $excl.elems {
      my $exast;
      if $excl.elems == 1 {
        $exast = $excl[0];
      } else {
        $exast = RxAlt.newp($excl);
      }
      $ast = RxSeq.newp(RxLookaround.newp(1,0,$exast),
			$ast);
    }
    return $ast;
  }
  my $pkg = undef;
  my $neg = undef;
  my $nocap = undef;
  my $name;
  my $args;
  my $exprs = [];
  my $sr = $o<assertion>;
  if $sr<sym_name> {
    if $sr<sym_name> eq '?' { $nocap = 1; $sr = $sr<assertion>; }
    if $sr<sym_name> eq '!' { $neg = 1; $sr = $sr<assertion>; }
  }
  if $sr<arglist> && not(defined($sr<identifier>)) {
    $name = $sr.match_string.re_gsub_pat(':<([^>]+)>\z',':$1').re_gsub_pat(':«([^»]+)»\z',':$1');
  }
  else {
    $name = irbuild_ir($sr<identifier>);
    $args = irbuild_ir($sr<nibbler>);#X?
    $exprs = $args;
  }
  RxSubrule.newp($pkg,$name,$exprs,$neg,$nocap);
} elsif $x eq '( )' {
  my $e = $m<nibbler>;
  my $p;
  if $e.WHAT ne 'Array' { $p = $e }
  elsif $e.elems == 0 { $p = RxSeq.newp([]) } #X noop
  elsif $e.elems == 1 { $p = $e[0] }
  else { $p = RxSeq.newp($e) }
  RxCap.newp($p);
} elsif $x eq '(? )' {
  my $sym = $m<assertion><sym_name>;
  my $rx = $m<assertion><rx> || RxSeq.newp([]);
  if $sym eq ':' { RxGrp.newp($rx) }
  elsif $sym eq '=' { RxLookaround.newp(1,1,$rx) }
  elsif $sym eq '!' { RxLookaround.newp(1,0,$rx) }
  elsif $sym eq '<' {
    my $sym2 = $m<assertion><assertion><sym_name>;
    $rx = $m<assertion><assertion><rx> || RxSeq.newp([]);
    if $sym2 eq '=' { RxLookaround.newp(0,1,$rx) }
    elsif $sym2 eq '!' { RxLookaround.newp(0,0,$rx) }
    else { die "(?<X bug"; }
  }
  elsif $sym eq 'mod' { $m<assertion> }
  elsif $sym eq '>' { RxIndependent.newp($rx) }
  else { die "Unimplemented (? ) assertion: "~$sym }
} elsif $x eq '[ ]' {
  if $blackboard::is_P5 {
    RxPat5.newp(*text*)
  } else {
    my $e = $m<nibbler>;
    my $p;
    if $e.WHAT ne 'Array' { $p = $e }
    elsif $e.elems == 1 { $p = $e[0] }
    else { $p = RxSeq.newp($e) }
    RxGrp.newp($p);
  }
} elsif $x eq '{ }' {
  my $stmts = $m<codeblock><statementlist>;
  RxCode.newp(Block.newp($stmts))
} elsif $x eq ':::' { RxCommitRegex.newp();
} elsif $x eq '::' { RxCommitGroup.newp();
} elsif $x eq ':' { RxCommitSequence.newp();
} elsif $x eq '\\' {
  if $o<backslash> {
    my $sym = $o<backslash><sym>;
    if $blackboard::is_P5 && $sym eq 'p' {
      RxPat5.newp(*text*)
    }
    elsif $o<backslash><number> {
      my $num = substr(*text*,1);
      my $n = $num.Num;
      if not($num.re_matchp('\A0')) && $n < 10 {
        RxBackref.newp($n);
      } else {
        # XXX kludge. Interpretation of \10 is much more complex.
        RxPat5.newp(*text*);
      }
    }
    else {
      RxPat5.newp(*text*)
    }
  }
  else {
    die "Unimplemented metachar: "~$x;
  }
} else {
  die "Unimplemented metachar: "~$x;
}

p5mods
my $mods = {};
my $normalize = sub ($x,$v) {
  if $x eq 'i' { $mods{$x} = $v }
  else { $mods{"perl5_"~$x} = $v }
};
$m<on>.split('').map(sub ($x){$normalize.($x,1)});
if $o<off>.elems {
  $m<off>[0].split('').map(sub ($x){$normalize.($x,0)});
}
$mods;

p5mod
*text*

assertion:mod
my $onoff = $m<p5mods>;
if $o<rx> && $o<rx>.elems {
  RxMod_expr.newp($onoff,$m<rx>[0]);
} else {
  RxMod_inline.newp($onoff);
}

assertion
*text*

cclass_elem
temp $blackboard::quote = "' '";
my $set = $m<quibble><nibble>;
my $pat5 = $set.re_gsub('\.\.','-').re_gsub('(?<!\\\\)\s','');
$pat5 = '['~$pat5~']';
RxPat5.newp($pat5);
# my $set = $m<quibble><nibble>
# my $op = substr(*text*,0,1);
# if $op ne '-' { $op = '+' }
# if $set.re_matchp('(^|[^\\])\-') { die "parse error - unescaped hyphen" }
# my $pat5 = $set.re_gsub('\.\.','-');#.re_gsub_pat('\\([\\\'])','\\\\\\$1');
# $pat5 = '['~$pat5~']';
# RxPat5.newp('['~$pat5~']');
#     my $pat = $m->match_string;
#     if($pat =~ /^([-+]?)\[(.+)\]$/s) {
#       my $op = $1 eq '-' ? '-' : '+';
#       my $set = $2;
#       die "parse error - unescaped hyphen" if $set =~ /(^|[^\\])\-/;
#       $set =~ s/\.\./-/g;
#       $set =~ s/\\([\\\'])/\\\\\\$1/g;
#       return $op."pat5('[$set]')";
#     }
#     elsif($pat =~ /^([-+]?)(\w+)$/) {
#       my $op = $1 eq '-' ? '-' : '+';
#       return $op."sr('?$2')";
#     }
#     else { die "bug" }
#   }


rx
$m<nibbler>

mod_internal
if $o<statement> {
  my $stmt = $m<statement>;
  return RxCode.newp($stmt); # no Block.
}
my $text = *text*;
my $modpat = $text;
if $o<nibbler> {
  my $after = $o<nibbler>.from;
  $modpat = substr($text,0,$after-$m.from);
}
my $mods = IRx1::RxMixinMod.mods_from_modpat($modpat);
if $mods.{'P5'} { $mods.{'p5'} = 1; $blackboard::is_P5 = 1 }
RxMod_inline.newp($mods)


END_DEF

my $header_code = <<'END_CODE';
# Warning: This file is mechanically written.  Your changes will be overwritten.

class IRx1_Build2 {
  has $.constructors;
  method add_constructor($k,$constructor) {
    if $.constructors {} else {
      my $h = {};
      $.constructors = $h;
    }
    $.constructors{$k} = $constructor;
  };
  method make_ir_from_Match_tree($m) {
    my $rule = $m.match_rule;
    my $constructor = $.constructors{$rule};
    if $constructor {
      $constructor.($m);
    } else {
      my $g = $rule.re_groups('\A([^:]+):');
      if $g { $constructor = $.constructors{$g[0]} }
      if $constructor {
        $constructor.($m);
      } else {
        die "Unknown rule: "~$rule~"\nIt needs to be added to ast_handlers .\n";
      }
    }
  };
};
class Match {
  method make_ir_from_Match_tree() {
    $main::irbuilder.make_ir_from_Match_tree(self)
  }
};
class ARRAY {
  method make_ir_from_Match_tree() {
    self.map(sub ($e){$e.make_ir_from_Match_tree()})
  }
};
class STRING {
  method make_ir_from_Match_tree() {
    self
  }
};
class INTEGER {
  method make_ir_from_Match_tree() {
    self
  }
};
class FLOAT {
  method make_ir_from_Match_tree() {
    self
  }
};
class UNDEF {
  method make_ir_from_Match_tree() {
    self
  }
};

END_CODE


sub write_ast_handlers {
  my($def,$file)=@_;
  my $paragraphs = sub {
    my($text)=@_;
    $text =~ s/^[ ]*#.*\n//mg;
    $text =~ s/[ ]*#.*//g;
    $text =~ s/^([ \t]*\n)*//;
    my @x = split(/\n\n+/,$text);
    @x;
  };
  my @paragraphs = $paragraphs->($def);

  my $code = $header_code.unindent(<<'  END');
    package IRx1_Build2 {
      sub irbuild_ir ($x) {
        $x.make_ir_from_Match_tree()
      };
      sub maybe ($x) {
        if $x.WHAT eq "Array" && $x.elems == 0 { undef }
        else { $x }
      }

  END
  my $init = "";

  my %seen;
  for my $para (@paragraphs) {
    $para =~ /^(\w+(?::[\S]+)?)\n(.*)/s or die "bug\n$para";
    my($name,$body)=($1,$2);
    die "Saw an AST handler for '$name' twice!\n" if $seen{$name}++;

    $body =~ s/\bir\(/irbuild_ir\(/g;
    $body =~ s/(\$m(?:<\w+>)+)/irbuild_ir($1)/g;
    $body =~ s/\$o((?:<\w+>)+)/\$m$1/g;
    $body =~ s/<(\w+)>/.match_hash{'$1'}/g;
    $body =~ s/([A-Z]\w+\.new\w*)\(/IRx1::$1(\$m,/g;
    $body =~ s/\*text\*/(\$m.match_string)/g;
    if ($body =~ /\*1\*/) {
      $body =~ s/\*1\*/\$one/g;
      $body = unindent(<<'      END',"  ").$body;
        my $key;
        for $m.match_hash.keys {
          if $_ ne 'match' {
            if $key {
              die("Unexpectedly more than 1 field - dont know which to choose\n")
            }
            $key = $_;
          }
        }
        my $one = irbuild_ir($m.match_hash{$key});
      END
    }

    my $fname = $name;
    $fname =~ s/(\W)/"_".ord($1)/eg;
    my $qname = $name;
    $qname =~ s/([\\'])/\\$1/g;
    $code .= "\n".unindent(<<"    END","    ");
      my \$construct_$fname = sub (\$m) {
        $body;
      };
    END
    $init .= "".unindent(<<"    END","    ");
      \$.add_constructor('$qname', \$construct_$fname);
    END

  }
  $code .= unindent(<<"  END");
    method init {

  END
  $code .= unindent(<<"  END");
      $init
      self;
    }; # end init
  };

  END
  $code .= unindent(<<'  END');

  if not($*ast2ir_0) { $*ast2ir_0 = IRx1_Build2.new.init; }
  $*ast2ir_1 = IRx1_Build2.new.init;

  END
  open(F,">$file") or die $!; print F $code; close F;
}

#XXX doesn't actually work.
sub unindent {
  my($s,$leave_indent)=@_;
  $leave_indent ||= "";
  $s =~ /^( *)$leave_indent/;
  my $indent = $1;
  $s =~ s/^$indent//mg;
  $s;
}

write_ast_handlers($def,$output_file);
