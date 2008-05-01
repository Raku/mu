#!/usr/bin/perl
use strict;
use warnings;

my $output_file = "./IRx1_FromAST.pm";
my $def = <<'END_DEF';
comp_unit
CompUnit.newp($m<statementlist>)

statement
*1*

expect_term
my $^blackboard::expect_term_base = $m<noun>;
my $post = $m.{'hash'}{'post'} || [];
for $post {
  $^blackboard::expect_term_base = ir($_)
}
$^blackboard::expect_term_base

post
$m<dotty> or $m<postop>

dotty:methodop
Call.newp($^blackboard::expect_term_base,$m<ident>,Capture.newp($m<semilist>))

dotty:postcircumfix
my $s = *text*;
my $name = substr($s,0,1)~' '~substr($s,-1,1); # XXX :(
my $ident = "postcircumfix:"~$name;
my $args = $m<kludge_name>;
if $args && ($args.ref eq 'SCALAR')  { $args = [$args] }
Call.newp($^blackboard::expect_term_base,$ident,Capture.newp($args))

postcircumfix
my $s = *text*;
my $name = substr($s,0,1)~' '~substr($s,-1,1); # XXX :(
my $ident = "postcircumfix:"~$name;
my $args = $m<kludge_name>;
if $args && ($args.ref eq 'SCALAR')  { $args = [$args] }
Call.newp($^blackboard::expect_term_base,$ident,Capture.newp($args))

term:expect_term
$m<noun>

term
if *text* eq 'self' {
  Apply.newp('self',Capture.newp([]))
} else {
  die "AST term partially unimplemented.\n";
}

integer
NumInt.newp(*text*)

subcall
my $t = $m<subshortname><twigil>;
if $t && $t eq '.' {
  Call.newp(Apply.newp('self',Capture.newp([])),$m<subshortname><desigilname><ident>,Capture.newp($m<semilist>))
} else {
  Apply.newp($m<subshortname>,Capture.newp($m<semilist>))
}

name
*text* # $m<ident>

statement_control:use
Use.newp('use',$m<module_name>)

module_name:depreciated
*text*

module_name:normal
*text*

statement_control:BEGIN
ClosureTrait.newp('BEGIN',$m<block>)

term:listop
my $not_really_an_arglist = $m<arglist>;
if $m<arglist> {
  Apply.newp($m<ident>,Capture.newp([$not_really_an_arglist])) #XXX
} else {
  Apply.newp($m<ident>,Capture.newp([]))
}

quote:q
Buf.newp($m<text>)

quote:qq
my $s = $m<text>;
$s =~ s/(?<!\\)\\n/\n/g;
$s =~ s/(?<!\\)\\t/\t/g;
Buf.newp($s)

quote:regex
my $s = $m<text>;
Rx.newp($s)

infix
#XXX Work around YAML::Syck::Load bug.
# str: "="  is becoming str: "str". :/
my $op = *text*;
if $op eq 'str' { $op = '=' };
Apply.newp("infix:"~$op,Capture.newp([$m<left>,$m<right>]))

scope_declarator:my
my $vd = $m<scoped>;
VarDecl.newp('my',undef,undef,$vd.[0],undef,undef,'=',$vd.[1])

scope_declarator:has
my $vd = $m<scoped>;
VarDecl.newp('has',undef,undef,$vd.[0],undef,undef,'=',$vd.[1])

scope_declarator:our
my $vd = $m<scoped>;
VarDecl.newp('our',undef,undef,$vd.[0],undef,undef,'=',$vd.[1])

scoped
*1*

variable_decl
[$m<variable>,$m<default_value>]

variable
Var.newp($m<sigil>,$m<twigil>,$m<desigilname>)

sigil
*text*

twigil
*text*

circumfix
my $s = *text*;
my $name = substr($s,0,1)~' '~substr($s,-1,1); # XXX :(
my $args = $m<kludge_name>;
if $args && ($args.ref eq 'SCALAR')  { $args = [$args] }
Apply.newp("circumfix:"~$name,Capture.newp($args))

statement_control:for
For.newp($m<expr>,$m<block>)

statement_control:while
Loop.newp($m<expr>,$m<block>)

statement_control:if
my $els = $m<else>;
if $els { $els = $els[0] }
Cond.newp([[$m<if_expr>,$m<if_block>]].push($m<elsif>.flatten),$els)

elsif
[$m<elsif_expr>,$m<elsif_block>]

if__else
*1*


statement_prefix:gather
Apply.newp("statement_prefix:gather",$m<statement>)


pblock
*1*

block
Block.newp($m<statementlist>)

plurality_declarator:multi
my $^blackboard::plurality = 'multi';
$m<pluralized>

routine_declarator:routine_def
my $plurality = $^blackboard::plurality; my $^blackboard::plurality;
my $ident = "";
if $m<ident> { $ident = $m<ident>.[0] };
my $sig = Signature.newp([],undef);
if $m<multisig> { $sig = $m<multisig>.[0] };
SubDecl.newp(undef,undef,$plurality,$ident,$sig,undef,$m<block>)

routine_declarator:method_def
my $plurality = $^blackboard::plurality; my $^blackboard::plurality;
MethodDecl.newp(undef,undef,$plurality,$m<ident>,$m<multisig>.[0],undef,$m<block>)

signature
Signature.newp($m<parsep>,undef)

parameter
Parameter.newp($m<type_constraint>,$m<quantchar>,$m<param_var>)

param_var
ParamVar.newp($m<sigil>,$m<twigil>,$m<ident>)

package_declarator:class
my $^blackboard::package_declarator = 'class';
$m<package_def>

package_declarator:module
my $^blackboard::package_declarator = 'module';
$m<package_def>

package_declarator:package
my $^blackboard::package_declarator = 'package';
$m<package_def>

package_declarator:grammar
my $^blackboard::package_declarator = 'grammar';
$m<package_def>

package_def
PackageDecl.newp(undef,undef,$^blackboard::package_declarator,$m<module_name>.[0],$m<traits>,$m<block>)

fulltypename
$m<typename>.join("::")

typename
*text* # $m<name>

trait_verb:is
Trait.newp('is',$m<ident>)

circumfix:pblock
if not($m<lambda>) and not($m<signature>) {
  Hash.newp($m<block><statementlist>)
} else {
  die "AST handler circumfix:pblock partially unimplemented";
}

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

class IRx1_Build {
  has $.constructors;
  $main::irbuilder = IRx1_Build.new;
  method add_constructor($k,$constructor) {
    if $.constructors {}else{
      my $h = {};
      $.constructors = $h;
    }
    $.constructors{$k} = $constructor;
  };
  method make_ir_from_Match_tree($m) {
    my $rule = $m.rule;
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
class ARRAY {
  method make_ir_from_Match_tree() {
    self.map(sub($e){$e.make_ir_from_Match_tree()})
  }
};
class SCALAR {
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
    $text =~ s/^\s*#.*\n//mg;
    $text =~ s/[ ]*#.*//g;
    $text =~ s/^([ \t]*\n)*//;
    my @x = split(/\n\n+/,$text);
    @x;
  };
  my @paragraphs = $paragraphs->($def);

  my $code = $header_code.unindent(<<'  END');
    sub irbuild_ir ($x) { # Deleteme when elf_d need not be supported.
      $x.make_ir_from_Match_tree()
    };
    package IRx1_Build {
      sub irbuild_ir ($x) {
        $x.make_ir_from_Match_tree()
      };
  END

  my %seen;
  for my $para (@paragraphs) {
    $para =~ /^([\w:]+)\n(.*)/s or die "bug";
    my($name,$body)=($1,$2);
    die "Saw an AST handler for '$name' twice!\n" if $seen{$name}++;

    $body =~ s{\s*=~\s*s/((?:[^\\\/]|\\.)*)/((?:[^\\\/]|\\.)*)/g;}{.re_gsub(/$1/,"$2");}g;

    $body =~ s/\bir\(/irbuild_ir\(/g;
    $body =~ s/(\$m(?:<\w+>)+)/irbuild_ir($1)/g;
    $body =~ s/<(\w+)>/.{'hash'}{'$1'}/g;
    $body =~ s/([A-Z]\w+\.new\w*)\(/IRx1::$1(\$m,/g;
    $body =~ s/\*text\*/(\$m.match_string)/g;
    if ($body =~ /\*1\*/) {
      $body =~ s/\*1\*/\$one/g;
      $body = unindent(<<'      END',"  ").$body;
        my $key;
        for $m.{'hash'}.keys {
          if $_ ne 'match' {
            if $key {
              die("Unexpectedly more than 1 field - dont know which to choose\n")
            }
            $key = $_;
          }
        }
        my $one = irbuild_ir($m.{'hash'}{$key});
      END
    }

    $code .= "\n".unindent(<<"    END","    ");
      \$main::irbuilder.add_constructor('$name', sub (\$m) {
        $body;
      });
    END

  }
  $code .= unindent(<<"  END");
  }
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
