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
my $result = $m<EXPR> || $m<control>;
if $o<EXPR> && ($o<mod_loop> || $o<mod_cond>) {
  temp $blackboard::statement_expr = $result;
  $result = $m<mod_loop> || $m<mod_cond>;
  if $o<mod_condloop> {
    $blackboard::statement_expr = $result;
    $result = $m<mod_condloop>;
  }
}
if $labels {
  Label.newp($labels,$result);
} else {
  $result;
}

EXPR
if $m<infix> {
  my $op = $m<infix><sym_name>;
  my $args = [$m<left>,$m<right>];
  if $op eq '=>' {
    if $args[2] { die "chained => unimplemented" }
    Pair.newp($args[0],$args[1])
  } else {
    Apply.newp("infix:"~$op,Capture.newp1($args))
  }
}
else {
  temp $blackboard::expect_term_base = $m<noun>;
  my $ops = [];
  if $o<pre>  { $ops.push($o<pre>.flatten) };
  if $o<post> { $ops.push($o<post>.flatten) };
  for $ops {
    $blackboard::expect_term_base = ir($_)
  }
  $blackboard::expect_term_base
}

pre
$m<prefix>

post
$m<postop> || $m<dotty>

postop
$m<postfix> || $m<postcircumfix>

prefix
my $op = *text*;
Apply.newp("prefix:"~$op,Capture.newp1([$blackboard::expect_term_base]))

postfix
my $op = *text*;
Apply.newp("postfix:"~$op,Capture.newp1([$blackboard::expect_term_base]))

postcircumfix
my $name = $m<sym_name>;
my $ident = "postcircumfix:"~$name;
my $args = $m<semilist>;
if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
Call.newp($blackboard::expect_term_base,$ident,Capture.newp1($args||[]))

semilist
$m<statement>

dotty
#temp $blackboard::dottyop = $m<sym_name>;
$m<dottyop>

dottyop
$m<methodop> || $m<postop>

methodop
Call.newp($blackboard::expect_term_base,$m<longname>,Capture.newp1($m<semilist>||[]))

term
my $text = $m<sym_name>;
if $text eq '*' {
  Apply.newp('whatever',Capture.newp1([]))
} else {
  Apply.newp($text,Capture.newp1([]))
}

noun
#Should just be *1*, but all versions of node contain a colonpair too.
$m<fatarrow> || $m<variable> || $m<package_declarator> || $m<scope_declarator> || $m<multi_declarator> || $m<routine_declarator> || $m<regex_declarator> || $m<type_declarator> || $m<circumfix> || $m<dotty> || $m<value> || $m<capterm> || $m<sigterm> || $m<term> || $m<statement_prefi> || $m<colonpair>

longname
$m<name>

name
my $parts = [$m<identifier>];
if $m<morename> { $parts.push($m<morename>.flatten) }
$parts.join("::");

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

fatarrow
Pair.newp($m<key>,$m<val>)


term:identifier
my $args = $m<args><semilist>;
if not($args) && $o<args><arglist>[0]<EXPR> {
  $args = [ irbuild_ir($o<args><arglist>[0]<EXPR>) ];
}
Apply.newp($m<identifier>,Capture.newp1($args||[]))


statement_control:use
Use.newp('use',$m<module_name>,$m<EXPR>)

module_name:depreciated
*text*

module_name:normal
*text*

role_name
*text*


circumfix
my $s = *text*;
my $name = substr($s,0,1)~' '~substr($s,-1,1); # XXX :(
my $args = $m<kludge_name>;
if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
Apply.newp("circumfix:"~$name,Capture.newp1($args||[]))


quote
my $nibs = $m<nibble><nibbles>;
my $args = $nibs.map(sub($x){if $x.WHAT eq 'Str' {Buf.newp($x);} else {$x}});
Apply.newp('infix:~',Capture.newp1($args||[]))

nibbles:\
my $which = $m<item><sym_name>;
if $which eq 'n' { "\n" }
if $which eq 't' { "\t" }
else { $which }


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

scoped
temp $blackboard::typenames = $m<fulltypename>;
$m<declarator>

declarator
$m<variable_declarator> || $m<signature> || $m<plurality_declarator> || $m<routine_declarator>  || $m<type_declarator>

variable_declarator
my $scope = $blackboard::scope; temp $blackboard::scope;
my $typenames = $blackboard::typenames; temp $blackboard::typenames = undef;
VarDecl.newp($scope,$typenames,undef,$m<variable>,undef,$m<traits>,'=',$m<default_value>)
#XXX default_value is going to take some non-local work.

variable
my $tw = $m<twigil>;
if $o<postcircumfix> {
  if $tw eq "." {
    my $slf = Apply.newp('self',Capture.newp1([]));
    my $args = $m<postcircumfix><kludge_name>;
    if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
    Call.newp($slf,$m<desigilname>,Capture.newp1($args||[]))
  } else {
    my $v = Var.newp($m<sigil>,$tw,$m<desigilname>);
    temp $blackboard::expect_term_base = $v;
    $m<postcircumfix>;
  }
} else {
  Var.newp($m<sigil>,$tw,$m<desigilname>);
}

sigil
*text*

twigil
*text*

special_variable
my $v = *text*;
my $s = substr($v,0,1);
my $n = substr($v,1,$v.chars);
Var.newp($s,undef,$n)


statement_control:BEGIN
ClosureTrait.newp('BEGIN',$m<block>)

statement_control:for
For.newp($m<EXPR>,$m<xblock>)

statement_mod_loop:for
For.newp($m<modifier_expr>,$blackboard::statement_expr)

statement_control:while
Loop.newp($m<EXPR>,$m<block>,undef,undef)

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
my $els = $m<else>;
if $els { $els = $els[0] }
Cond.newp([[$m<if_expr>,$m<if_block>]].push($m<elsif>.flatten),$els,undef)

elsif
[$m<elsif_expr>,$m<elsif_block>]

if__else
*1*

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
if $o<signature> {
  SubDecl.newp(undef,undef,undef,undef,$m<signature>,undef,$m<block>)
} else {
  $m<block>
}

block
Block.newp($m<statementlist>)

xblock
$m<pblock>

plurality_declarator:multi
temp $blackboard::plurality = 'multi';
$m<pluralized> || $m<routine_def>

routine_declarator:routine_def
my $scope = $blackboard::scope; temp $blackboard::scope;
my $plurality = $blackboard::plurality; temp $blackboard::plurality;
my $ident = "";
if $o<ident> { $ident = $m<ident>  };
if ($o<ident> && not($scope)) { $scope = "our" };
my $sig = Signature.newp([],undef);
if $m<multisig> { $sig = $m<multisig>.[0] };
SubDecl.newp($scope,undef,$plurality,$ident,$sig,$m<trait>,$m<block>)

# routine_def is the same as routine_declarator:routine_def
# This is a workaround for STD.pm not recognizing  multi f(){} .
routine_def
my $scope = $blackboard::scope; temp $blackboard::scope;
my $plurality = $blackboard::plurality; temp $blackboard::plurality;
my $ident = "";
if $o<ident> { $ident = $m<ident>  };
if ($o<ident> && not($scope)) { $scope = "our" };
my $sig = Signature.newp([],undef);
if $m<multisig> { $sig = $m<multisig>.[0] };
SubDecl.newp($scope,undef,$plurality,$ident,$sig,$m<trait>,$m<block>)

routine_declarator:method_def
my $plurality = $blackboard::plurality; temp $blackboard::plurality;
my $multisig = $m<multisig>;
if not($multisig) { $multisig = [Signature.newp([],undef)]; }
MethodDecl.newp(undef,undef,$plurality,$m<ident>,$multisig.[0],$m<trait>,$m<block>,undef,undef)

signature
Signature.newp($m<parsep>,undef)

parameter
Parameter.newp($m<type_constraint>,$m<quantchar>,$m<param_var>,undef,undef,undef,undef)

param_var
ParamVar.newp($m<sigil>,$m<twigil>,$m<ident>)

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

colonpair
*1*

colonpair__false
Pair.newp($m<ident>,NumInt.newp(0))

colonpair__value
my $value;
if $o<postcircumfix> {
  $value = $m<postcircumfix><kludge_name>;
} else {
  $value = NumInt.newp(1);
}
Pair.newp($m<ident>,$value)

quotepair
*1*

quotepair__false
Pair.newp($m<ident>,NumInt.newp(0))

quotepair__value
my $value;
if $o<postcircumfix> {
  $value = $m<postcircumfix><kludge_name>;
} else {
  $value = NumInt.newp(1);
}
Pair.newp($m<ident>,$value)

quotepair__nth
Pair.newp('nth',$m<n>)


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
PackageDecl.newp(undef,undef,$blackboard::package_declarator,$m<module_name>.[0],$m<traits>,$m<block>)

fulltypename
$m<typename>.join("::")

typename
*text* # $m<name>

trait_verb:is
Trait.newp('is',$m<ident>)

trait_verb:does
Trait.newp('does',$m<role_name>)


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


quote:regex
my $s = $m<text> || $m<quotesnabber><text>;
Rx.newp($s,$m<quotepair>)

regex_declarator:regex_def
RegexDef.newp($m<ident>,$m<regex_block>)

regex_block
$m<regex>

regex
Regex.newp($m<pattern>)

regex_first
RxFirst.newp($m<patterns>)

regex_every
RxEvery.newp($m<patterns>)

regex_submatch
RxSubmatch.newp($m<patterns>)

regex_any
RxAny.newp($m<patterns>)

regex_all
RxAll.newp($m<patterns>)

regex_sequence
RxSequence.newp($m<patterns>)

regex_quantified_atom
RxQuantifiedAtom.newp($m<regex_atom>,$m<regex_quantifier>)

regex_quantifier
*text*

regex_atom
if $m<char> { RxLiteral.newp($m<char>,"'") } else { *1* }

regex_metachar:regex_backslash
RxBackslash.newp(*text*)

regex_metachar:regex_mod_internal
RxModInternal.newp(*text*)

regex_assertion:ident
RxAssertion.newp($m<ident>)

regex_metachar:capture
RxCapture.newp($m<regex><pattern>)

regex_metachar:group
RxGroup.newp($m<regex><pattern>)

regex_metachar:block
RxBlock.newp($m<block>)

regex_metachar:var
RxBind.newp($m<variable>,$m<binding>)

regex_metachar:q
RxLiteral.newp($m<text>,"'")

regex_metachar:qq
RxLiteral.newp($m<text>,'"')

regex_metachar
RxSymbol.newp(*text*)



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
    my $rule = $m.rule;
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
    self.map(sub($e){$e.make_ir_from_Match_tree()})
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
    $body =~ s/<(\w+)>/.hash{'$1'}/g;
    $body =~ s/([A-Z]\w+\.new\w*)\(/IRx1::$1(\$m,/g;
    $body =~ s/\*text\*/(\$m.match_string)/g;
    if ($body =~ /\*1\*/) {
      $body =~ s/\*1\*/\$one/g;
      $body = unindent(<<'      END',"  ").$body;
        my $key;
        for $m.hash.keys {
          if $_ ne 'match' {
            if $key {
              die("Unexpectedly more than 1 field - dont know which to choose\n")
            }
            $key = $_;
          }
        }
        my $one = irbuild_ir($m.hash{$key});
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
