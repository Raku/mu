# Warning: This file is mechanically written.  Your changes will be overwritten.

class IRx1_Build {
  has $.constructors;
  $main::irbuilder = IRx1_Build.new;
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

package IRx1_Build {
  sub irbuild_ir ($x) {
    $x.make_ir_from_Match_tree()
  };

    $main::irbuilder.add_constructor('comp_unit', sub ($m) {
      IRx1::CompUnit.newp($m,irbuild_ir($m.{'hash'}{'statementlist'}));
    });

    $main::irbuilder.add_constructor('statement', sub ($m) {
      my $labels = irbuild_ir($m.{'hash'}{'label'});
my $result = irbuild_ir($m.{'hash'}{'expr'}) || irbuild_ir($m.{'hash'}{'control'});
if $m.{'hash'}{'expr'} && ($m.{'hash'}{'mod_loop'} || $m.{'hash'}{'mod_cond'}) {
temp $blackboard::statement_expr = $result;
$result = irbuild_ir($m.{'hash'}{'mod_loop'}) || irbuild_ir($m.{'hash'}{'mod_cond'});
if $m.{'hash'}{'mod_condloop'} {
  $blackboard::statement_expr = $result;
  $result = irbuild_ir($m.{'hash'}{'mod_condloop'});
}
}
if $labels {
IRx1::Label.newp($m,$labels,$result);
} else {
$result;
};
    });

    $main::irbuilder.add_constructor('expect_infix', sub ($m) {
      if irbuild_ir($m.{'hash'}{'infix'}) {
if irbuild_ir($m.{'hash'}{'infix_postfix_meta_operator'}) {
  die "Unimplemented infix_postfix_meta_operator";
}
my $op = irbuild_ir($m.{'hash'}{'infix'}.{'hash'}{'sym'});
if $op eq '=>' {
  my $args = irbuild_ir($m.{'hash'}{'args'});
  if $args[2] { die "chained => unimplemented" }
  IRx1::Pair.newp($m,$args[0],$args[1])
} else {
  IRx1::Apply.newp($m,"infix:"~$op,IRx1::Capture.newp($m,irbuild_ir($m.{'hash'}{'args'})||[]))
}
} else {
die "Unimplemented infix_prefix_meta_operator or infix_circumfix_meta_operator";
};
    });

    $main::irbuilder.add_constructor('fatarrow', sub ($m) {
      IRx1::Pair.newp($m,irbuild_ir($m.{'hash'}{'key'}),irbuild_ir($m.{'hash'}{'val'}));
    });

    $main::irbuilder.add_constructor('expect_term', sub ($m) {
      temp $blackboard::expect_term_base = irbuild_ir($m.{'hash'}{'noun'});
my $ops = [];
if $m.{'hash'}{'pre'}  { $ops.push($m.{'hash'}{'pre'}.flatten) };
if $m.{'hash'}{'post'} { $ops.push($m.{'hash'}{'post'}.flatten) };
for $ops {
$blackboard::expect_term_base = irbuild_ir($_)
}
$blackboard::expect_term_base;
    });

    $main::irbuilder.add_constructor('term:expect_term', sub ($m) {
      irbuild_ir($m.{'hash'}{'noun'});
    });

    $main::irbuilder.add_constructor('post', sub ($m) {
      if $m.{'hash'}{'args'} {
irbuild_ir($m.{'hash'}{'args'})[0]
} else {
irbuild_ir($m.{'hash'}{'dotty'}) or irbuild_ir($m.{'hash'}{'postop'})
};
    });

    $main::irbuilder.add_constructor('pre', sub ($m) {
      if $m.{'hash'}{'args'} {
irbuild_ir($m.{'hash'}{'args'})[0]
} elsif $m.{'hash'}{'prefix'} {
irbuild_ir($m.{'hash'}{'prefix'})
} else {
die "pre without a prefix is unimplemented";
};
    });

    $main::irbuilder.add_constructor('dotty:methodop', sub ($m) {
      IRx1::Call.newp($m,$blackboard::expect_term_base,irbuild_ir($m.{'hash'}{'ident'}),IRx1::Capture.newp($m,irbuild_ir($m.{'hash'}{'semilist'})||[]));
    });

    $main::irbuilder.add_constructor('dotty:.^!', sub ($m) {
      IRx1::Call.newp($m,$blackboard::expect_term_base,'^!'~irbuild_ir($m.{'hash'}{'methodop'}.{'hash'}{'ident'}),IRx1::Capture.newp($m,irbuild_ir($m.{'hash'}{'methodop'}.{'hash'}{'semilist'})||[]));
    });

    $main::irbuilder.add_constructor('dotty:postcircumfix', sub ($m) {
      my $s = ($m.match_string);
my $name = substr($s,0,1)~' '~substr($s,-1,1);
my $ident = "postcircumfix:"~$name;
my $args = irbuild_ir($m.{'hash'}{'kludge_name'});
if $args && ($args.ref ne 'ARRAY')  { $args = [$args] }
IRx1::Call.newp($m,$blackboard::expect_term_base,$ident,IRx1::Capture.newp($m,$args||[]));
    });

    $main::irbuilder.add_constructor('postcircumfix', sub ($m) {
      my $s = ($m.match_string);
my $name = substr($s,0,1)~' '~substr($s,-1,1);
my $ident = "postcircumfix:"~$name;
my $args = irbuild_ir($m.{'hash'}{'kludge_name'});
if $args && ($args.ref ne 'ARRAY')  { $args = [$args] }
IRx1::Call.newp($m,$blackboard::expect_term_base,$ident,IRx1::Capture.newp($m,$args||[]));
    });

    $main::irbuilder.add_constructor('postfix', sub ($m) {
      my $op = ($m.match_string);
IRx1::Apply.newp($m,"postfix:"~$op,IRx1::Capture.newp($m,[$blackboard::expect_term_base]));
    });

    $main::irbuilder.add_constructor('prefix', sub ($m) {
      my $op = ($m.match_string);
IRx1::Apply.newp($m,"prefix:"~$op,IRx1::Capture.newp($m,[$blackboard::expect_term_base]));
    });

    $main::irbuilder.add_constructor('infix', sub ($m) {
      my $op = ($m.match_string);
IRx1::Apply.newp($m,"infix:"~$op,IRx1::Capture.newp($m,[irbuild_ir($m.{'hash'}{'left'}),irbuild_ir($m.{'hash'}{'right'})]));
    });

    $main::irbuilder.add_constructor('term', sub ($m) {
      my $text = ($m.match_string);
if $text eq 'self' {
IRx1::Apply.newp($m,'self',IRx1::Capture.newp($m,[]))
} elsif $text eq '*' {
IRx1::Apply.newp($m,'whatever',IRx1::Capture.newp($m,[]))
} else {
die "AST term partially unimplemented.\n";
};
    });

    $main::irbuilder.add_constructor('integer', sub ($m) {
      IRx1::NumInt.newp($m,($m.match_string));
    });

    $main::irbuilder.add_constructor('subcall', sub ($m) {
      my $t = irbuild_ir($m.{'hash'}{'subshortname'}.{'hash'}{'twigil'});
if $t && $t eq '.' {
IRx1::Call.newp($m,IRx1::Apply.newp($m,'self',IRx1::Capture.newp($m,[])),irbuild_ir($m.{'hash'}{'subshortname'}.{'hash'}{'desigilname'}.{'hash'}{'ident'}),IRx1::Capture.newp($m,irbuild_ir($m.{'hash'}{'semilist'})||[]))
} else {
IRx1::Apply.newp($m,irbuild_ir($m.{'hash'}{'subshortname'}),IRx1::Capture.newp($m,irbuild_ir($m.{'hash'}{'semilist'})||[]))
};
    });

    $main::irbuilder.add_constructor('name', sub ($m) {
      ($m.match_string);
    });

    $main::irbuilder.add_constructor('subshortname', sub ($m) {
      if $m.{'hash'}{'category'} {
my $cat = $m.{'hash'}{'category'}.match_string;
my $op = $m.{'hash'}{'colonpair'}[0].{'hash'}{'structural'}.{'hash'}{'kludge_name'};
if $op.WHAT eq 'Array' { $op = $op.join("") }
$cat~':'~$op;
} else {
($m.match_string)
};
    });

    $main::irbuilder.add_constructor('statement_control:use', sub ($m) {
      IRx1::Use.newp($m,'use',irbuild_ir($m.{'hash'}{'module_name'}),irbuild_ir($m.{'hash'}{'EXPR'}));
    });

    $main::irbuilder.add_constructor('module_name:depreciated', sub ($m) {
      ($m.match_string);
    });

    $main::irbuilder.add_constructor('module_name:normal', sub ($m) {
      ($m.match_string);
    });

    $main::irbuilder.add_constructor('role_name', sub ($m) {
      ($m.match_string);
    });

    $main::irbuilder.add_constructor('statement_control:BEGIN', sub ($m) {
      IRx1::ClosureTrait.newp($m,'BEGIN',irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('term:listop', sub ($m) {
      my $not_really_an_arglist = irbuild_ir($m.{'hash'}{'arglist'});
if irbuild_ir($m.{'hash'}{'arglist'}) {
IRx1::Apply.newp($m,irbuild_ir($m.{'hash'}{'ident'}),IRx1::Capture.newp($m,[$not_really_an_arglist]))
} else {
IRx1::Apply.newp($m,irbuild_ir($m.{'hash'}{'ident'}),IRx1::Capture.newp($m,[]))
};
    });

    $main::irbuilder.add_constructor('quote:q', sub ($m) {
      my $s = irbuild_ir($m.{'hash'}{'text'});
$s.re_sub_g('\\\\([\\\\\'])','$1');
IRx1::Buf.newp($m,$s);
    });

    $main::irbuilder.add_constructor('quote:qq', sub ($m) {
      my $s = irbuild_ir($m.{'hash'}{'text'});
$s.re_gsub(rx:P5/(?<!\\)\\n/,"\n");
$s.re_gsub(rx:P5/(?<!\\)\\t/,"\t");
$s.re_sub_g('\\\\(.)','$1');
IRx1::Buf.newp($m,$s);
    });

    $main::irbuilder.add_constructor('quote:regex', sub ($m) {
      my $s = irbuild_ir($m.{'hash'}{'text'}) || irbuild_ir($m.{'hash'}{'quotesnabber'}.{'hash'}{'text'});
IRx1::Rx.newp($m,$s,irbuild_ir($m.{'hash'}{'quotepair'}));
    });

    $main::irbuilder.add_constructor('scope_declarator:my', sub ($m) {
      temp $blackboard::scope = 'my';
irbuild_ir($m.{'hash'}{'scoped'});
    });

    $main::irbuilder.add_constructor('scope_declarator:has', sub ($m) {
      temp $blackboard::scope = 'has';
irbuild_ir($m.{'hash'}{'scoped'});
    });

    $main::irbuilder.add_constructor('scope_declarator:our', sub ($m) {
      temp $blackboard::scope = 'our';
irbuild_ir($m.{'hash'}{'scoped'});
    });

    $main::irbuilder.add_constructor('scope_declarator:temp', sub ($m) {
      temp $blackboard::scope = 'temp';
irbuild_ir($m.{'hash'}{'scoped'});
    });

    $main::irbuilder.add_constructor('scoped', sub ($m) {
      temp $blackboard::typenames = irbuild_ir($m.{'hash'}{'fulltypename'});
irbuild_ir($m.{'hash'}{'variable_decl'}) || irbuild_ir($m.{'hash'}{'signature'}) || irbuild_ir($m.{'hash'}{'plurality_declarator'}) || irbuild_ir($m.{'hash'}{'routine_declarator'})  || irbuild_ir($m.{'hash'}{'type_declarator'});
    });

    $main::irbuilder.add_constructor('variable_decl', sub ($m) {
      my $scope = $blackboard::scope; temp $blackboard::scope;
my $typenames = $blackboard::typenames; temp $blackboard::typenames = undef;
IRx1::VarDecl.newp($m,$scope,$typenames,undef,irbuild_ir($m.{'hash'}{'variable'}),undef,irbuild_ir($m.{'hash'}{'traits'}),'=',irbuild_ir($m.{'hash'}{'default_value'}));
    });

    $main::irbuilder.add_constructor('variable', sub ($m) {
      my $tw = irbuild_ir($m.{'hash'}{'twigil'});
if $m.{'hash'}{'postcircumfix'} {
if $tw eq "." {
  my $slf = IRx1::Apply.newp($m,'self',IRx1::Capture.newp($m,[]));
  my $args = irbuild_ir($m.{'hash'}{'postcircumfix'}.{'hash'}{'kludge_name'});
  if $args && ($args.ref ne 'ARRAY')  { $args = [$args] }
  IRx1::Call.newp($m,$slf,irbuild_ir($m.{'hash'}{'desigilname'}),IRx1::Capture.newp($m,$args||[]))
} else {
  my $v = IRx1::Var.newp($m,irbuild_ir($m.{'hash'}{'sigil'}),$tw,irbuild_ir($m.{'hash'}{'desigilname'}));
  temp $blackboard::expect_term_base = $v;
  irbuild_ir($m.{'hash'}{'postcircumfix'});
}
} else {
IRx1::Var.newp($m,irbuild_ir($m.{'hash'}{'sigil'}),$tw,irbuild_ir($m.{'hash'}{'desigilname'}));
};
    });

    $main::irbuilder.add_constructor('sigil', sub ($m) {
      ($m.match_string);
    });

    $main::irbuilder.add_constructor('twigil', sub ($m) {
      ($m.match_string);
    });

    $main::irbuilder.add_constructor('special_variable', sub ($m) {
      my $v = ($m.match_string);
my $s = substr($v,0,1);
my $n = substr($v,1,$v.length);
IRx1::Var.newp($m,$s,undef,$n);
    });

    $main::irbuilder.add_constructor('circumfix', sub ($m) {
      my $s = ($m.match_string);
my $name = substr($s,0,1)~' '~substr($s,-1,1);
my $args = irbuild_ir($m.{'hash'}{'kludge_name'});
if $args && ($args.ref ne 'ARRAY')  { $args = [$args] }
IRx1::Apply.newp($m,"circumfix:"~$name,IRx1::Capture.newp($m,$args||[]));
    });

    $main::irbuilder.add_constructor('statement_control:for', sub ($m) {
      IRx1::For.newp($m,irbuild_ir($m.{'hash'}{'expr'}),irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('statement_mod_loop:for', sub ($m) {
      IRx1::For.newp($m,irbuild_ir($m.{'hash'}{'modifier_expr'}),$blackboard::statement_expr);
    });

    $main::irbuilder.add_constructor('statement_control:while', sub ($m) {
      IRx1::Loop.newp($m,irbuild_ir($m.{'hash'}{'expr'}),irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('statement_mod_loop:while', sub ($m) {
      IRx1::Loop.newp($m,irbuild_ir($m.{'hash'}{'modifier_expr'}),$blackboard::statement_expr);
    });

    $main::irbuilder.add_constructor('statement_control:until', sub ($m) {
      my $test = IRx1::Apply.newp($m,"not",IRx1::Capture.newp($m,[irbuild_ir($m.{'hash'}{'expr'})]));
IRx1::Loop.newp($m,$test,irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('statement_mod_loop:until', sub ($m) {
      my $test = IRx1::Apply.newp($m,"not",IRx1::Capture.newp($m,[irbuild_ir($m.{'hash'}{'modifier_expr'})]));
IRx1::Loop.newp($m,$test,$blackboard::statement_expr);
    });

    $main::irbuilder.add_constructor('statement_control:loop', sub ($m) {
      my $e1 = irbuild_ir($m.{'hash'}{'loop_eee'}.{'hash'}{'loop_e1'});
my $e2 = irbuild_ir($m.{'hash'}{'loop_eee'}.{'hash'}{'loop_e2'});
my $e3 = irbuild_ir($m.{'hash'}{'loop_eee'}.{'hash'}{'loop_e3'});
my $block = irbuild_ir($m.{'hash'}{'loop_block'});
my $body = IRx1::Loop.newp($m,$e2,IRx1::Block.newp($m,[$block,$e3]));
IRx1::Block.newp($m,[$e1,$body]);
    });

    $main::irbuilder.add_constructor('statement_control:if', sub ($m) {
      my $els = irbuild_ir($m.{'hash'}{'else'});
if $els { $els = $els[0] }
IRx1::Cond.newp($m,[[irbuild_ir($m.{'hash'}{'if_expr'}),irbuild_ir($m.{'hash'}{'if_block'})]].push(irbuild_ir($m.{'hash'}{'elsif'}).flatten),$els);
    });

    $main::irbuilder.add_constructor('elsif', sub ($m) {
      [irbuild_ir($m.{'hash'}{'elsif_expr'}),irbuild_ir($m.{'hash'}{'elsif_block'})];
    });

    $main::irbuilder.add_constructor('if__else', sub ($m) {
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
$one;
    });

    $main::irbuilder.add_constructor('statement_mod_cond:if', sub ($m) {
      IRx1::Cond.newp($m,[[irbuild_ir($m.{'hash'}{'modifier_expr'}),$blackboard::statement_expr]],undef);
    });

    $main::irbuilder.add_constructor('statement_control:unless', sub ($m) {
      IRx1::Cond.newp($m,[[irbuild_ir($m.{'hash'}{'expr'}),irbuild_ir($m.{'hash'}{'block'})]],undef,1);
    });

    $main::irbuilder.add_constructor('statement_mod_cond:unless', sub ($m) {
      IRx1::Cond.newp($m,[[irbuild_ir($m.{'hash'}{'modifier_expr'}),$blackboard::statement_expr]],undef,1);
    });

    $main::irbuilder.add_constructor('statement_control:given', sub ($m) {
      IRx1::Given.newp($m,irbuild_ir($m.{'hash'}{'expr'}),irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('statement_mod_loop:given', sub ($m) {
      IRx1::Given.newp($m,irbuild_ir($m.{'hash'}{'modifier_expr'}),$blackboard::statement_expr);
    });

    $main::irbuilder.add_constructor('statement_control:when', sub ($m) {
      IRx1::When.newp($m,irbuild_ir($m.{'hash'}{'expr'}),irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('statement_mod_cond:when', sub ($m) {
      IRx1::When.newp($m,irbuild_ir($m.{'hash'}{'modifier_expr'}),$blackboard::statement_expr);
    });

    $main::irbuilder.add_constructor('statement_control:default', sub ($m) {
      IRx1::When.newp($m,undef,irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('statement_prefix:do', sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:do",IRx1::Capture.newp($m,[irbuild_ir($m.{'hash'}{'statement'})]));
    });

    $main::irbuilder.add_constructor('statement_prefix:try', sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:try",IRx1::Capture.newp($m,[irbuild_ir($m.{'hash'}{'statement'})]));
    });

    $main::irbuilder.add_constructor('statement_prefix:gather', sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:gather",IRx1::Capture.newp($m,[irbuild_ir($m.{'hash'}{'statement'})]));
    });

    $main::irbuilder.add_constructor('statement_prefix:contend', sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:contend",IRx1::Capture.newp($m,[irbuild_ir($m.{'hash'}{'statement'})]));
    });

    $main::irbuilder.add_constructor('statement_prefix:async', sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:async",IRx1::Capture.newp($m,[irbuild_ir($m.{'hash'}{'statement'})]));
    });

    $main::irbuilder.add_constructor('statement_prefix:lazy', sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:lazy",IRx1::Capture.newp($m,[irbuild_ir($m.{'hash'}{'statement'})]));
    });

    $main::irbuilder.add_constructor('pblock', sub ($m) {
      if $m.{'hash'}{'signature'} {
IRx1::SubDecl.newp($m,undef,undef,undef,undef,irbuild_ir($m.{'hash'}{'signature'}),undef,irbuild_ir($m.{'hash'}{'block'}))
} else {
irbuild_ir($m.{'hash'}{'block'})
};
    });

    $main::irbuilder.add_constructor('block', sub ($m) {
      IRx1::Block.newp($m,irbuild_ir($m.{'hash'}{'statementlist'}));
    });

    $main::irbuilder.add_constructor('plurality_declarator:multi', sub ($m) {
      temp $blackboard::plurality = 'multi';
irbuild_ir($m.{'hash'}{'pluralized'}) || irbuild_ir($m.{'hash'}{'routine_def'});
    });

    $main::irbuilder.add_constructor('routine_declarator:routine_def', sub ($m) {
      my $scope = $blackboard::scope; temp $blackboard::scope;
my $plurality = $blackboard::plurality; temp $blackboard::plurality;
my $ident = "";
if $m.{'hash'}{'ident'} { $ident = irbuild_ir($m.{'hash'}{'ident'})  };
my $sig = IRx1::Signature.newp($m,[],undef);
if irbuild_ir($m.{'hash'}{'multisig'}) { $sig = irbuild_ir($m.{'hash'}{'multisig'}).[0] };
IRx1::SubDecl.newp($m,$scope,undef,$plurality,$ident,$sig,irbuild_ir($m.{'hash'}{'trait'}),irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('routine_def', sub ($m) {
      my $scope = $blackboard::scope; temp $blackboard::scope;
my $plurality = $blackboard::plurality; temp $blackboard::plurality;
my $ident = "";
if $m.{'hash'}{'ident'} { $ident = irbuild_ir($m.{'hash'}{'ident'})  };
my $sig = IRx1::Signature.newp($m,[],undef);
if irbuild_ir($m.{'hash'}{'multisig'}) { $sig = irbuild_ir($m.{'hash'}{'multisig'}).[0] };
IRx1::SubDecl.newp($m,$scope,undef,$plurality,$ident,$sig,irbuild_ir($m.{'hash'}{'trait'}),irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('routine_declarator:method_def', sub ($m) {
      my $plurality = $blackboard::plurality; temp $blackboard::plurality;
my $multisig = irbuild_ir($m.{'hash'}{'multisig'});
if not($multisig) { $multisig = [IRx1::Signature.newp($m,[],undef)]; }
IRx1::MethodDecl.newp($m,undef,undef,$plurality,irbuild_ir($m.{'hash'}{'ident'}),$multisig.[0],irbuild_ir($m.{'hash'}{'trait'}),irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('signature', sub ($m) {
      IRx1::Signature.newp($m,irbuild_ir($m.{'hash'}{'parsep'}),undef);
    });

    $main::irbuilder.add_constructor('parameter', sub ($m) {
      IRx1::Parameter.newp($m,irbuild_ir($m.{'hash'}{'type_constraint'}),irbuild_ir($m.{'hash'}{'quantchar'}),irbuild_ir($m.{'hash'}{'param_var'}));
    });

    $main::irbuilder.add_constructor('param_var', sub ($m) {
      IRx1::ParamVar.newp($m,irbuild_ir($m.{'hash'}{'sigil'}),irbuild_ir($m.{'hash'}{'twigil'}),irbuild_ir($m.{'hash'}{'ident'}));
    });

    $main::irbuilder.add_constructor('capture', sub ($m) {
      if not($m.{'hash'}{'EXPR'}) {
IRx1::Capture.newp($m,[])
}
elsif $m.{'hash'}{'EXPR'}.{'hash'}{'noun'} {
IRx1::Capture.newp($m,[irbuild_ir($m.{'hash'}{'EXPR'}.{'hash'}{'noun'})])
}
elsif $m.{'hash'}{'EXPR'}.{'hash'}{'sym'} && $m.{'hash'}{'EXPR'}.{'hash'}{'sym'} eq ':' {
my $args = irbuild_ir($m.{'hash'}{'EXPR'}.{'hash'}{'args'});
my $inv = $args.shift;
IRx1::Capture.newp($m,$args||[],$inv)
}
elsif $m.{'hash'}{'EXPR'}.{'hash'}{'sym'} && $m.{'hash'}{'EXPR'}.{'hash'}{'sym'} eq ',' {
my $args = $m.{'hash'}{'EXPR'}.{'hash'}{'args'};
my $arg0 = $args && $args[0];
my $inv = undef;
if $arg0 && $arg0.{'hash'}{'sym'} && $arg0.{'hash'}{'sym'} eq ':' {
  $args.shift;
  $inv = $arg0.{'hash'}{'args'}[0];
  if $arg0.{'hash'}{'args'}[1] {
    $args.unshift($arg0.{'hash'}{'args'}[1]);
  }
}
IRx1::Capture.newp($m,irbuild_ir($args)||[],irbuild_ir($inv))
}
else { die "capture AST form not recognized" };
    });

    $main::irbuilder.add_constructor('colonpair', sub ($m) {
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
$one;
    });

    $main::irbuilder.add_constructor('colonpair__false', sub ($m) {
      IRx1::Pair.newp($m,irbuild_ir($m.{'hash'}{'ident'}),IRx1::NumInt.newp($m,0));
    });

    $main::irbuilder.add_constructor('colonpair__value', sub ($m) {
      my $value;
if $m.{'hash'}{'postcircumfix'} {
$value = irbuild_ir($m.{'hash'}{'postcircumfix'}.{'hash'}{'kludge_name'});
} else {
$value = IRx1::NumInt.newp($m,1);
}
IRx1::Pair.newp($m,irbuild_ir($m.{'hash'}{'ident'}),$value);
    });

    $main::irbuilder.add_constructor('quotepair', sub ($m) {
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
$one;
    });

    $main::irbuilder.add_constructor('quotepair__false', sub ($m) {
      IRx1::Pair.newp($m,irbuild_ir($m.{'hash'}{'ident'}),IRx1::NumInt.newp($m,0));
    });

    $main::irbuilder.add_constructor('quotepair__value', sub ($m) {
      my $value;
if $m.{'hash'}{'postcircumfix'} {
$value = irbuild_ir($m.{'hash'}{'postcircumfix'}.{'hash'}{'kludge_name'});
} else {
$value = IRx1::NumInt.newp($m,1);
}
IRx1::Pair.newp($m,irbuild_ir($m.{'hash'}{'ident'}),$value);
    });

    $main::irbuilder.add_constructor('quotepair__nth', sub ($m) {
      IRx1::Pair.newp($m,'nth',irbuild_ir($m.{'hash'}{'n'}));
    });

    $main::irbuilder.add_constructor('package_declarator:role', sub ($m) {
      temp $blackboard::package_declarator = 'role';
irbuild_ir($m.{'hash'}{'package_def'});
    });

    $main::irbuilder.add_constructor('package_declarator:class', sub ($m) {
      temp $blackboard::package_declarator = 'class';
irbuild_ir($m.{'hash'}{'package_def'});
    });

    $main::irbuilder.add_constructor('package_declarator:module', sub ($m) {
      temp $blackboard::package_declarator = 'module';
irbuild_ir($m.{'hash'}{'package_def'});
    });

    $main::irbuilder.add_constructor('package_declarator:package', sub ($m) {
      temp $blackboard::package_declarator = 'package';
irbuild_ir($m.{'hash'}{'package_def'});
    });

    $main::irbuilder.add_constructor('package_declarator:grammar', sub ($m) {
      temp $blackboard::package_declarator = 'grammar';
irbuild_ir($m.{'hash'}{'package_def'});
    });

    $main::irbuilder.add_constructor('package_def', sub ($m) {
      IRx1::PackageDecl.newp($m,undef,undef,$blackboard::package_declarator,irbuild_ir($m.{'hash'}{'module_name'}).[0],irbuild_ir($m.{'hash'}{'traits'}),irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('fulltypename', sub ($m) {
      irbuild_ir($m.{'hash'}{'typename'}).join("::");
    });

    $main::irbuilder.add_constructor('typename', sub ($m) {
      ($m.match_string);
    });

    $main::irbuilder.add_constructor('trait_verb:is', sub ($m) {
      IRx1::Trait.newp($m,'is',irbuild_ir($m.{'hash'}{'ident'}));
    });

    $main::irbuilder.add_constructor('trait_verb:does', sub ($m) {
      IRx1::Trait.newp($m,'does',irbuild_ir($m.{'hash'}{'role_name'}));
    });

    $main::irbuilder.add_constructor('circumfix:pblock', sub ($m) {
      if $m.{'hash'}{'block'}.{'hash'}{'statementlist'}.elems == 0 or $m.{'hash'}{'block'}.{'hash'}{'statementlist'}[0].match_string.re_matchp('^:') {
IRx1::Hash.newp($m,irbuild_ir($m.{'hash'}{'block'}.{'hash'}{'statementlist'}))
} elsif $m.{'hash'}{'block'}.{'hash'}{'statementlist'}[0].{'hash'}{'expr'} and $m.{'hash'}{'block'}.{'hash'}{'statementlist'}[0].{'hash'}{'expr'}.{'hash'}{'sym'} and $m.{'hash'}{'block'}.{'hash'}{'statementlist'}[0].{'hash'}{'expr'}.{'hash'}{'sym'} eq "," {
IRx1::Hash.newp($m,irbuild_ir($m.{'hash'}{'block'}.{'hash'}{'statementlist'}))
} elsif $m.{'hash'}{'block'}.{'hash'}{'statementlist'}[0].{'hash'}{'expr'} and $m.{'hash'}{'block'}.{'hash'}{'statementlist'}[0].{'hash'}{'expr'}.{'hash'}{'sym'} and $m.{'hash'}{'block'}.{'hash'}{'statementlist'}[0].{'hash'}{'expr'}.{'hash'}{'sym'} eq "=>" {
IRx1::Hash.newp($m,irbuild_ir($m.{'hash'}{'block'}.{'hash'}{'statementlist'}))
} elsif not(irbuild_ir($m.{'hash'}{'lambda'})) and not(irbuild_ir($m.{'hash'}{'signature'})) {
irbuild_ir($m.{'hash'}{'block'})
} else {
die "AST handler circumfix:pblock partially unimplemented";
};
    });

    $main::irbuilder.add_constructor('regex_declarator:regex_def', sub ($m) {
      IRx1::RegexDef.newp($m,irbuild_ir($m.{'hash'}{'ident'}),irbuild_ir($m.{'hash'}{'regex_block'}));
    });

    $main::irbuilder.add_constructor('regex_block', sub ($m) {
      irbuild_ir($m.{'hash'}{'regex'});
    });

    $main::irbuilder.add_constructor('regex', sub ($m) {
      IRx1::Regex.newp($m,irbuild_ir($m.{'hash'}{'pattern'}));
    });

    $main::irbuilder.add_constructor('regex_first', sub ($m) {
      IRx1::RxFirst.newp($m,irbuild_ir($m.{'hash'}{'patterns'}));
    });

    $main::irbuilder.add_constructor('regex_every', sub ($m) {
      IRx1::RxEvery.newp($m,irbuild_ir($m.{'hash'}{'patterns'}));
    });

    $main::irbuilder.add_constructor('regex_submatch', sub ($m) {
      IRx1::RxSubmatch.newp($m,irbuild_ir($m.{'hash'}{'patterns'}));
    });

    $main::irbuilder.add_constructor('regex_any', sub ($m) {
      IRx1::RxAny.newp($m,irbuild_ir($m.{'hash'}{'patterns'}));
    });

    $main::irbuilder.add_constructor('regex_all', sub ($m) {
      IRx1::RxAll.newp($m,irbuild_ir($m.{'hash'}{'patterns'}));
    });

    $main::irbuilder.add_constructor('regex_sequence', sub ($m) {
      IRx1::RxSequence.newp($m,irbuild_ir($m.{'hash'}{'patterns'}));
    });

    $main::irbuilder.add_constructor('regex_quantified_atom', sub ($m) {
      IRx1::RxQuantifiedAtom.newp($m,irbuild_ir($m.{'hash'}{'regex_atom'}),irbuild_ir($m.{'hash'}{'regex_quantifier'}));
    });

    $main::irbuilder.add_constructor('regex_quantifier', sub ($m) {
      ($m.match_string);
    });

    $main::irbuilder.add_constructor('regex_atom', sub ($m) {
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
if irbuild_ir($m.{'hash'}{'char'}) { IRx1::RxLiteral.newp($m,irbuild_ir($m.{'hash'}{'char'}),"'") } else { $one };
    });

    $main::irbuilder.add_constructor('regex_metachar:regex_backslash', sub ($m) {
      IRx1::RxBackslash.newp($m,($m.match_string));
    });

    $main::irbuilder.add_constructor('regex_metachar:regex_mod_internal', sub ($m) {
      IRx1::RxModInternal.newp($m,($m.match_string));
    });

    $main::irbuilder.add_constructor('regex_assertion:ident', sub ($m) {
      IRx1::RxAssertion.newp($m,irbuild_ir($m.{'hash'}{'ident'}));
    });

    $main::irbuilder.add_constructor('regex_metachar:capture', sub ($m) {
      IRx1::RxCapture.newp($m,irbuild_ir($m.{'hash'}{'regex'}.{'hash'}{'pattern'}));
    });

    $main::irbuilder.add_constructor('regex_metachar:group', sub ($m) {
      IRx1::RxGroup.newp($m,irbuild_ir($m.{'hash'}{'regex'}.{'hash'}{'pattern'}));
    });

    $main::irbuilder.add_constructor('regex_metachar:block', sub ($m) {
      IRx1::RxBlock.newp($m,irbuild_ir($m.{'hash'}{'block'}));
    });

    $main::irbuilder.add_constructor('regex_metachar:var', sub ($m) {
      IRx1::RxBind.newp($m,irbuild_ir($m.{'hash'}{'variable'}),irbuild_ir($m.{'hash'}{'binding'}));
    });

    $main::irbuilder.add_constructor('regex_metachar:q', sub ($m) {
      IRx1::RxLiteral.newp($m,irbuild_ir($m.{'hash'}{'text'}),"'");
    });

    $main::irbuilder.add_constructor('regex_metachar:qq', sub ($m) {
      IRx1::RxLiteral.newp($m,irbuild_ir($m.{'hash'}{'text'}),'"');
    });

    $main::irbuilder.add_constructor('regex_metachar', sub ($m) {
      IRx1::RxSymbol.newp($m,($m.match_string));
    });
}
