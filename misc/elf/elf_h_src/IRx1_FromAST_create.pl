#!/usr/bin/perl
use strict;
use warnings;

my $output_file = "./IRx1_FromAST.pm";
my $def = <<'END_DEF';
comp_unit
CompUnit.newp($m<statementlist>,undef)

statement
my $labels = $m<label>;
my $result = $m<expr> || $m<control>;
if $o<expr> && ($o<mod_loop> || $o<mod_cond>) {
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

expect_infix
if $m<infix> {
  if $m<infix_postfix_meta_operator> {
    die "Unimplemented infix_postfix_meta_operator";
  }
  my $op = $m<infix><sym>;
  if $op eq '=>' {
    my $args = $m<args>;
    if $args[2] { die "chained => unimplemented" }
    Pair.newp($args[0],$args[1])
  } else {
    Apply.newp("infix:"~$op,Capture.newp1($m<args>||[]))
  }
} else {
  die "Unimplemented infix_prefix_meta_operator or infix_circumfix_meta_operator";
}

fatarrow
Pair.newp($m<key>,$m<val>)

expect_term
temp $blackboard::expect_term_base = $m<noun>;
my $ops = [];
if $o<pre>  { $ops.push($o<pre>.flatten) };
if $o<post> { $ops.push($o<post>.flatten) };
for $ops {
  $blackboard::expect_term_base = ir($_)
}
$blackboard::expect_term_base

term:expect_term
$m<noun>

post
if $o<args> {
  $m<args>[0]
} else {
  $m<dotty> or $m<postop>
}

pre
if $o<args> {
  $m<args>[0]
} elsif $o<prefix> {
  $m<prefix>
} else {
  die "pre without a prefix is unimplemented";
}

dotty:methodop
Call.newp($blackboard::expect_term_base,$m<ident>,Capture.newp1($m<semilist>||[]))

dotty:.^!
Call.newp($blackboard::expect_term_base,'^!'~$m<methodop><ident>,Capture.newp1($m<methodop><semilist>||[])) # XXX ^! should be expanded.

dotty:postcircumfix
my $s = *text*;
my $name = substr($s,0,1)~' '~substr($s,-1,1); # XXX :(
my $ident = "postcircumfix:"~$name;
my $args = $m<kludge_name>;
if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
Call.newp($blackboard::expect_term_base,$ident,Capture.newp1($args||[]))

postcircumfix
my $s = *text*;
my $name = substr($s,0,1)~' '~substr($s,-1,1); # XXX :(
my $ident = "postcircumfix:"~$name;
my $args = $m<kludge_name>;
if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
Call.newp($blackboard::expect_term_base,$ident,Capture.newp1($args||[]))

postfix
my $op = *text*;
Apply.newp("postfix:"~$op,Capture.newp1([$blackboard::expect_term_base]))

prefix
my $op = *text*;
Apply.newp("prefix:"~$op,Capture.newp1([$blackboard::expect_term_base]))

infix
my $op = *text*;
Apply.newp("infix:"~$op,Capture.newp1([$m<left>,$m<right>]))

term
my $text = *text*;
if $text eq 'self' {
  Apply.newp('self',Capture.newp1([]))
} elsif $text eq '*' {
  Apply.newp('whatever',Capture.newp1([]))
} else {
  die "AST term partially unimplemented.\n";
}

integer
NumInt.newp(*text*,10)

subcall
my $t = $m<subshortname><twigil>;
if $t && $t eq '.' {
  Call.newp(Apply.newp('self',Capture.newp1([])),$m<subshortname><desigilname><ident>,Capture.newp1($m<semilist>||[]))
} else {
  Apply.newp($m<subshortname>,Capture.newp1($m<semilist>||[]))
}

name
*text*

subshortname
if $o<category> {
  my $cat = $o<category>.match_string;
  my $op;
  if $o<colonpair>[0]<structural> {
      $op = $o<colonpair>[0]<structural><kludge_name>;
  } else {
      $op = $o<colonpair>[0]<value><postcircumfix><kludge_name>;
  }
  #XXX The next line is a workaround for an unexamined bug, perhaps in STD_red.
  # multi infix:«<=» ($a,$b) {}  yields a ['<','='] .
  if $op.WHAT eq 'Array' { $op = $op.join("") }
  $cat~':'~$op;
} else {
  *text*
}

statement_control:use
Use.newp('use',$m<module_name>,$m<EXPR>)

module_name:depreciated
*text*

module_name:normal
*text*

role_name
*text*


statement_control:BEGIN
ClosureTrait.newp('BEGIN',$m<block>)

term:listop
my $not_really_an_arglist = $m<arglist>;
if $m<arglist> {
  Apply.newp($m<ident>,Capture.newp1([$not_really_an_arglist])) #XXX
} else {
  Apply.newp($m<ident>,Capture.newp1([]))
}

quote:q
my $s = $m<text>;
$s = $s.re_gsub_pat('\\\\([\\\\\'])','$1');
Buf.newp($s)

quote:qq
my $s = $m<text>;
$s = $s.re_gsub('(?<!\\\\)\\\\n',"\n");
$s = $s.re_gsub('(?<!\\\\)\\\\t',"\t");
$s = $s.re_gsub_pat('\\\\(.)','$1');
Buf.newp($s)

quote:regex
my $s = $m<text> || $m<quotesnabber><text>;
Rx.newp($s,$m<quotepair>)

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
$m<variable_decl> || $m<signature> || $m<plurality_declarator> || $m<routine_declarator>  || $m<type_declarator>

variable_decl
my $scope = $blackboard::scope; temp $blackboard::scope;
my $typenames = $blackboard::typenames; temp $blackboard::typenames = undef;
VarDecl.newp($scope,$typenames,undef,$m<variable>,undef,$m<traits>,'=',$m<default_value>)


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

circumfix
my $s = *text*;
my $name = substr($s,0,1)~' '~substr($s,-1,1); # XXX :(
my $args = $m<kludge_name>;
if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
Apply.newp("circumfix:"~$name,Capture.newp1($args||[]))


statement_control:for
For.newp($m<expr>,$m<block>)

statement_mod_loop:for
For.newp($m<modifier_expr>,$blackboard::statement_expr)

statement_control:while
Loop.newp($m<expr>,$m<block>,undef,undef)

statement_mod_loop:while
Loop.newp($m<modifier_expr>,$blackboard::statement_expr,undef,undef)

statement_control:until
my $test = Apply.newp("not",Capture.newp1([$m<expr>]));
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
Cond.newp([[$m<expr>,$m<block>]],undef,1)

statement_mod_cond:unless
Cond.newp([[$m<modifier_expr>,$blackboard::statement_expr]],undef,1)


statement_control:given
Given.newp($m<expr>,$m<block>)

statement_mod_loop:given
Given.newp($m<modifier_expr>,$blackboard::statement_expr)

statement_control:when
When.newp($m<expr>,$m<block>)

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
} elsif $o<block><statementlist>[0]<expr> and $o<block><statementlist>[0]<expr><sym> and $o<block><statementlist>[0]<expr><sym> eq "," { # XXX Not p6.  Remove once off elf_e, and Match updated.
  Hash.newp($m<block><statementlist>)
} elsif $o<block><statementlist>[0]<expr> and $o<block><statementlist>[0]<expr><sym> and $o<block><statementlist>[0]<expr><sym> eq "=>" {
  Hash.newp($m<block><statementlist>)
} elsif not($m<lambda>) and not($m<signature>) {
  $m<block>
} else {
  die "AST handler circumfix:pblock partially unimplemented";
}

regex_declarator:regex_def
RegexDef.newp('regex',$m<ident>,undef,undef,$m<regex_block>)

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

class IRx1_Build {
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
    if ($constructor) {
      $constructor.($m);
    } else {
      die "Unknown rule: "~$rule~"\nIt needs to be added to ast_handlers.\n";
    }
  };
};
class Match {
  method make_ir_from_Match_tree() {
    $main::irbuilder.make_ir_from_Match_tree(self)
  }
};
class Array {
  method make_ir_from_Match_tree() {
    self.map(sub ($e){$e.make_ir_from_Match_tree()})
  }
};
class Str {
  method make_ir_from_Match_tree() {
    self
  }
};
class Int {
  method make_ir_from_Match_tree() {
    self
  }
};
class Num {
  method make_ir_from_Match_tree() {
    self
  }
};
class Undef {
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
    package IRx1_Build {
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
    $code .= "\n".unindent(<<"    END","    ");
      my \$construct_$fname = sub (\$m) {
        $body;
      };
    END
    $init .= "".unindent(<<"    END","    ");
      \$.add_constructor('$name', \$construct_$fname);
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

  if not($*ast2ir_0) { $*ast2ir_0 = IRx1_Build.new.init; }
  $*ast2ir_1 = IRx1_Build.new.init;

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
