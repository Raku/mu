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

package IRx1_Build {
  sub irbuild_ir ($x) {
    $x.make_ir_from_Match_tree()
  };


    my $construct_comp_unit = sub ($m) {
      IRx1::CompUnit.newp($m,irbuild_ir($m.match_hash{'statementlist'}),undef);
    };

    my $construct_statement = sub ($m) {
      my $labels = irbuild_ir($m.match_hash{'label'});
my $result = irbuild_ir($m.match_hash{'expr'}) || irbuild_ir($m.match_hash{'control'});
if $m.match_hash{'expr'} && ($m.match_hash{'mod_loop'} || $m.match_hash{'mod_cond'}) {
temp $blackboard::statement_expr = $result;
$result = irbuild_ir($m.match_hash{'mod_loop'}) || irbuild_ir($m.match_hash{'mod_cond'});
if $m.match_hash{'mod_condloop'} {
  $blackboard::statement_expr = $result;
  $result = irbuild_ir($m.match_hash{'mod_condloop'});
}
}
if $labels {
IRx1::Label.newp($m,$labels,$result);
} else {
$result;
};
    };

    my $construct_expect_infix = sub ($m) {
      if irbuild_ir($m.match_hash{'infix'}) {
if irbuild_ir($m.match_hash{'infix_postfix_meta_operator'}) {
  die "Unimplemented infix_postfix_meta_operator";
}
my $op = irbuild_ir($m.match_hash{'infix'}.match_hash{'sym'});
if $op eq '=>' {
  my $args = irbuild_ir($m.match_hash{'args'});
  if $args[2] { die "chained => unimplemented" }
  IRx1::Pair.newp($m,$args[0],$args[1])
} else {
  IRx1::Apply.newp($m,"infix:"~$op,IRx1::Capture.newp1($m,irbuild_ir($m.match_hash{'args'})||[]))
}
} else {
die "Unimplemented infix_prefix_meta_operator or infix_circumfix_meta_operator";
};
    };

    my $construct_fatarrow = sub ($m) {
      IRx1::Pair.newp($m,irbuild_ir($m.match_hash{'key'}),irbuild_ir($m.match_hash{'val'}));
    };

    my $construct_expect_term = sub ($m) {
      temp $blackboard::expect_term_base = irbuild_ir($m.match_hash{'noun'});
my $ops = [];
if $m.match_hash{'pre'}  { $ops.push($m.match_hash{'pre'}.flatten) };
if $m.match_hash{'post'} { $ops.push($m.match_hash{'post'}.flatten) };
for $ops {
$blackboard::expect_term_base = irbuild_ir($_)
}
$blackboard::expect_term_base;
    };

    my $construct_term_58expect_term = sub ($m) {
      irbuild_ir($m.match_hash{'noun'});
    };

    my $construct_post = sub ($m) {
      if $m.match_hash{'args'} {
irbuild_ir($m.match_hash{'args'}[0])
} else {
irbuild_ir($m.match_hash{'dotty'}) or irbuild_ir($m.match_hash{'postop'})
};
    };

    my $construct_pre = sub ($m) {
      if $m.match_hash{'args'} {
irbuild_ir($m.match_hash{'args'}[0])
} elsif $m.match_hash{'prefix'} {
irbuild_ir($m.match_hash{'prefix'})
} else {
die "pre without a prefix is unimplemented";
};
    };

    my $construct_dotty_58methodop = sub ($m) {
      IRx1::Call.newp($m,$blackboard::expect_term_base,irbuild_ir($m.match_hash{'ident'}),IRx1::Capture.newp1($m,irbuild_ir($m.match_hash{'semilist'})||[]));
    };

    my $construct_dotty_58_46_94_33 = sub ($m) {
      IRx1::Call.newp($m,$blackboard::expect_term_base,'^!'~irbuild_ir($m.match_hash{'methodop'}.match_hash{'ident'}),IRx1::Capture.newp1($m,irbuild_ir($m.match_hash{'methodop'}.match_hash{'semilist'})||[]));
    };

    my $construct_dotty_58postcircumfix = sub ($m) {
      my $s = ($m.match_string);
my $name = substr($s,0,1)~' '~substr($s,-1,1);
my $ident = "postcircumfix:"~$name;
my $args = irbuild_ir($m.match_hash{'kludge_name'});
if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
IRx1::Call.newp($m,$blackboard::expect_term_base,$ident,IRx1::Capture.newp1($m,$args||[]));
    };

    my $construct_postcircumfix = sub ($m) {
      my $s = ($m.match_string);
my $name = substr($s,0,1)~' '~substr($s,-1,1);
my $ident = "postcircumfix:"~$name;
my $args = irbuild_ir($m.match_hash{'kludge_name'});
if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
IRx1::Call.newp($m,$blackboard::expect_term_base,$ident,IRx1::Capture.newp1($m,$args||[]));
    };

    my $construct_postfix = sub ($m) {
      my $op = ($m.match_string);
IRx1::Apply.newp($m,"postfix:"~$op,IRx1::Capture.newp1($m,[$blackboard::expect_term_base]));
    };

    my $construct_prefix = sub ($m) {
      my $op = ($m.match_string);
IRx1::Apply.newp($m,"prefix:"~$op,IRx1::Capture.newp1($m,[$blackboard::expect_term_base]));
    };

    my $construct_infix = sub ($m) {
      my $op = ($m.match_string);
IRx1::Apply.newp($m,"infix:"~$op,IRx1::Capture.newp1($m,[irbuild_ir($m.match_hash{'left'}),irbuild_ir($m.match_hash{'right'})]));
    };

    my $construct_term = sub ($m) {
      my $text = ($m.match_string);
if $text eq 'self' {
IRx1::Apply.newp($m,'self',IRx1::Capture.newp1($m,[]))
} elsif $text eq '*' {
IRx1::Apply.newp($m,'whatever',IRx1::Capture.newp1($m,[]))
} else {
die "AST term partially unimplemented.\n";
};
    };

    my $construct_integer = sub ($m) {
      IRx1::NumInt.newp($m,($m.match_string),10);
    };

    my $construct_subcall = sub ($m) {
      my $t = irbuild_ir($m.match_hash{'subshortname'}.match_hash{'twigil'});
if $t && $t eq '.' {
IRx1::Call.newp($m,IRx1::Apply.newp($m,'self',IRx1::Capture.newp1($m,[])),irbuild_ir($m.match_hash{'subshortname'}.match_hash{'desigilname'}.match_hash{'ident'}),IRx1::Capture.newp1($m,irbuild_ir($m.match_hash{'semilist'})||[]))
} else {
IRx1::Apply.newp($m,irbuild_ir($m.match_hash{'subshortname'}),IRx1::Capture.newp1($m,irbuild_ir($m.match_hash{'semilist'})||[]))
};
    };

    my $construct_name = sub ($m) {
      ($m.match_string);
    };

    my $construct_subshortname = sub ($m) {
      if $m.match_hash{'category'} {
my $cat = $m.match_hash{'category'}.match_string;
my $op;
if $m.match_hash{'colonpair'}[0].match_hash{'structural'} {
    $op = $m.match_hash{'colonpair'}[0].match_hash{'structural'}.match_hash{'kludge_name'};
} else {
    $op = $m.match_hash{'colonpair'}[0].match_hash{'value'}.match_hash{'postcircumfix'}.match_hash{'kludge_name'};
}
if $op.WHAT eq 'Array' { $op = $op.join("") }
$cat~':'~$op;
} else {
($m.match_string)
};
    };

    my $construct_statement_control_58use = sub ($m) {
      IRx1::Use.newp($m,'use',irbuild_ir($m.match_hash{'module_name'}),irbuild_ir($m.match_hash{'EXPR'}));
    };

    my $construct_module_name_58depreciated = sub ($m) {
      ($m.match_string);
    };

    my $construct_module_name_58normal = sub ($m) {
      ($m.match_string);
    };

    my $construct_role_name = sub ($m) {
      ($m.match_string);
    };

    my $construct_statement_control_58BEGIN = sub ($m) {
      IRx1::ClosureTrait.newp($m,'BEGIN',irbuild_ir($m.match_hash{'block'}));
    };

    my $construct_term_58listop = sub ($m) {
      my $not_really_an_arglist = irbuild_ir($m.match_hash{'arglist'});
if irbuild_ir($m.match_hash{'arglist'}) {
IRx1::Apply.newp($m,irbuild_ir($m.match_hash{'ident'}),IRx1::Capture.newp1($m,[$not_really_an_arglist]))
} else {
IRx1::Apply.newp($m,irbuild_ir($m.match_hash{'ident'}),IRx1::Capture.newp1($m,[]))
};
    };

    my $construct_quote_58q = sub ($m) {
      my $s = irbuild_ir($m.match_hash{'text'});
$s = $s.re_gsub_pat('\\\\([\\\\\'])','$1');
IRx1::Buf.newp($m,$s);
    };

    my $construct_quote_58qq = sub ($m) {
      my $s = irbuild_ir($m.match_hash{'text'});
$s = $s.re_gsub('(?<!\\\\)\\\\n',"\n");
$s = $s.re_gsub('(?<!\\\\)\\\\t',"\t");
$s = $s.re_gsub_pat('\\\\(.)','$1');
IRx1::Buf.newp($m,$s);
    };

    my $construct_quote_58regex = sub ($m) {
      my $s = irbuild_ir($m.match_hash{'text'}) || irbuild_ir($m.match_hash{'quotesnabber'}.match_hash{'text'});
IRx1::Rx.newp($m,$s,irbuild_ir($m.match_hash{'quotepair'}));
    };

    my $construct_scope_declarator_58my = sub ($m) {
      temp $blackboard::scope = 'my';
irbuild_ir($m.match_hash{'scoped'});
    };

    my $construct_scope_declarator_58has = sub ($m) {
      temp $blackboard::scope = 'has';
irbuild_ir($m.match_hash{'scoped'});
    };

    my $construct_scope_declarator_58our = sub ($m) {
      temp $blackboard::scope = 'our';
irbuild_ir($m.match_hash{'scoped'});
    };

    my $construct_scope_declarator_58temp = sub ($m) {
      temp $blackboard::scope = 'temp';
irbuild_ir($m.match_hash{'scoped'});
    };

    my $construct_scoped = sub ($m) {
      temp $blackboard::typenames = irbuild_ir($m.match_hash{'fulltypename'});
irbuild_ir($m.match_hash{'variable_decl'}) || irbuild_ir($m.match_hash{'signature'}) || irbuild_ir($m.match_hash{'plurality_declarator'}) || irbuild_ir($m.match_hash{'routine_declarator'})  || irbuild_ir($m.match_hash{'type_declarator'});
    };

    my $construct_variable_decl = sub ($m) {
      my $scope = $blackboard::scope; temp $blackboard::scope;
my $typenames = $blackboard::typenames; temp $blackboard::typenames = undef;
IRx1::VarDecl.newp($m,$scope,$typenames,undef,irbuild_ir($m.match_hash{'variable'}),undef,irbuild_ir($m.match_hash{'traits'}),'=',irbuild_ir($m.match_hash{'default_value'}));
    };

    my $construct_variable = sub ($m) {
      my $tw = irbuild_ir($m.match_hash{'twigil'});
if $m.match_hash{'postcircumfix'} {
if $tw eq "." {
  my $slf = IRx1::Apply.newp($m,'self',IRx1::Capture.newp1($m,[]));
  my $args = irbuild_ir($m.match_hash{'postcircumfix'}.match_hash{'kludge_name'});
  if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
  IRx1::Call.newp($m,$slf,irbuild_ir($m.match_hash{'desigilname'}),IRx1::Capture.newp1($m,$args||[]))
} else {
  my $v = IRx1::Var.newp($m,irbuild_ir($m.match_hash{'sigil'}),$tw,irbuild_ir($m.match_hash{'desigilname'}));
  temp $blackboard::expect_term_base = $v;
  irbuild_ir($m.match_hash{'postcircumfix'});
}
} else {
IRx1::Var.newp($m,irbuild_ir($m.match_hash{'sigil'}),$tw,irbuild_ir($m.match_hash{'desigilname'}));
};
    };

    my $construct_sigil = sub ($m) {
      ($m.match_string);
    };

    my $construct_twigil = sub ($m) {
      ($m.match_string);
    };

    my $construct_special_variable = sub ($m) {
      my $v = ($m.match_string);
my $s = substr($v,0,1);
my $n = substr($v,1,$v.chars);
IRx1::Var.newp($m,$s,undef,$n);
    };

    my $construct_circumfix = sub ($m) {
      my $s = ($m.match_string);
my $name = substr($s,0,1)~' '~substr($s,-1,1);
my $args = irbuild_ir($m.match_hash{'kludge_name'});
if $args && ($args.WHAT ne 'Array')  { $args = [$args] }
IRx1::Apply.newp($m,"circumfix:"~$name,IRx1::Capture.newp1($m,$args||[]));
    };

    my $construct_statement_control_58for = sub ($m) {
      IRx1::For.newp($m,irbuild_ir($m.match_hash{'expr'}),irbuild_ir($m.match_hash{'block'}));
    };

    my $construct_statement_mod_loop_58for = sub ($m) {
      IRx1::For.newp($m,irbuild_ir($m.match_hash{'modifier_expr'}),$blackboard::statement_expr);
    };

    my $construct_statement_control_58while = sub ($m) {
      IRx1::Loop.newp($m,irbuild_ir($m.match_hash{'expr'}),irbuild_ir($m.match_hash{'block'}),undef,undef);
    };

    my $construct_statement_mod_loop_58while = sub ($m) {
      IRx1::Loop.newp($m,irbuild_ir($m.match_hash{'modifier_expr'}),$blackboard::statement_expr,undef,undef);
    };

    my $construct_statement_control_58until = sub ($m) {
      my $test = IRx1::Apply.newp($m,"not",IRx1::Capture.newp1($m,[irbuild_ir($m.match_hash{'expr'})]));
IRx1::Loop.newp($m,$test,irbuild_ir($m.match_hash{'block'}),undef,undef);
    };

    my $construct_statement_mod_loop_58until = sub ($m) {
      my $test = IRx1::Apply.newp($m,"not",IRx1::Capture.newp1($m,[irbuild_ir($m.match_hash{'modifier_expr'})]));
IRx1::Loop.newp($m,$test,$blackboard::statement_expr,undef,undef);
    };

    my $construct_statement_control_58loop = sub ($m) {
      my $e1 = irbuild_ir($m.match_hash{'loop_eee'}.match_hash{'loop_e1'});
my $e2 = irbuild_ir($m.match_hash{'loop_eee'}.match_hash{'loop_e2'});
my $e3 = irbuild_ir($m.match_hash{'loop_eee'}.match_hash{'loop_e3'});
my $block = irbuild_ir($m.match_hash{'loop_block'});
my $body = IRx1::Loop.newp($m,$e2,IRx1::Block.newp($m,[$block,$e3]),undef,undef);
IRx1::Block.newp($m,[$e1,$body]);
    };

    my $construct_statement_control_58if = sub ($m) {
      my $els = irbuild_ir($m.match_hash{'else'});
if $els { $els = $els[0] }
IRx1::Cond.newp($m,[[irbuild_ir($m.match_hash{'if_expr'}),irbuild_ir($m.match_hash{'if_block'})]].push(irbuild_ir($m.match_hash{'elsif'}).flatten),$els,undef);
    };

    my $construct_elsif = sub ($m) {
      [irbuild_ir($m.match_hash{'elsif_expr'}),irbuild_ir($m.match_hash{'elsif_block'})];
    };

    my $construct_if__else = sub ($m) {
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
$one;
    };

    my $construct_statement_mod_cond_58if = sub ($m) {
      IRx1::Cond.newp($m,[[irbuild_ir($m.match_hash{'modifier_expr'}),$blackboard::statement_expr]],undef,undef);
    };

    my $construct_statement_control_58unless = sub ($m) {
      IRx1::Cond.newp($m,[[irbuild_ir($m.match_hash{'expr'}),irbuild_ir($m.match_hash{'block'})]],undef,1);
    };

    my $construct_statement_mod_cond_58unless = sub ($m) {
      IRx1::Cond.newp($m,[[irbuild_ir($m.match_hash{'modifier_expr'}),$blackboard::statement_expr]],undef,1);
    };

    my $construct_statement_control_58given = sub ($m) {
      IRx1::Given.newp($m,irbuild_ir($m.match_hash{'expr'}),irbuild_ir($m.match_hash{'block'}));
    };

    my $construct_statement_mod_loop_58given = sub ($m) {
      IRx1::Given.newp($m,irbuild_ir($m.match_hash{'modifier_expr'}),$blackboard::statement_expr);
    };

    my $construct_statement_control_58when = sub ($m) {
      IRx1::When.newp($m,irbuild_ir($m.match_hash{'expr'}),irbuild_ir($m.match_hash{'block'}));
    };

    my $construct_statement_mod_cond_58when = sub ($m) {
      IRx1::When.newp($m,irbuild_ir($m.match_hash{'modifier_expr'}),$blackboard::statement_expr);
    };

    my $construct_statement_control_58default = sub ($m) {
      IRx1::When.newp($m,undef,irbuild_ir($m.match_hash{'block'}));
    };

    my $construct_statement_prefix_58do = sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:do",IRx1::Capture.newp1($m,[irbuild_ir($m.match_hash{'statement'})]));
    };

    my $construct_statement_prefix_58try = sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:try",IRx1::Capture.newp1($m,[irbuild_ir($m.match_hash{'statement'})]));
    };

    my $construct_statement_prefix_58gather = sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:gather",IRx1::Capture.newp1($m,[irbuild_ir($m.match_hash{'statement'})]));
    };

    my $construct_statement_prefix_58contend = sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:contend",IRx1::Capture.newp1($m,[irbuild_ir($m.match_hash{'statement'})]));
    };

    my $construct_statement_prefix_58async = sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:async",IRx1::Capture.newp1($m,[irbuild_ir($m.match_hash{'statement'})]));
    };

    my $construct_statement_prefix_58lazy = sub ($m) {
      IRx1::Apply.newp($m,"statement_prefix:lazy",IRx1::Capture.newp1($m,[irbuild_ir($m.match_hash{'statement'})]));
    };

    my $construct_pblock = sub ($m) {
      if $m.match_hash{'signature'} {
IRx1::SubDecl.newp($m,undef,undef,undef,undef,irbuild_ir($m.match_hash{'signature'}),undef,irbuild_ir($m.match_hash{'block'}))
} else {
irbuild_ir($m.match_hash{'block'})
};
    };

    my $construct_block = sub ($m) {
      IRx1::Block.newp($m,irbuild_ir($m.match_hash{'statementlist'}));
    };

    my $construct_plurality_declarator_58multi = sub ($m) {
      temp $blackboard::plurality = 'multi';
irbuild_ir($m.match_hash{'pluralized'}) || irbuild_ir($m.match_hash{'routine_def'});
    };

    my $construct_routine_declarator_58routine_def = sub ($m) {
      my $scope = $blackboard::scope; temp $blackboard::scope;
my $plurality = $blackboard::plurality; temp $blackboard::plurality;
my $ident = "";
if $m.match_hash{'ident'} { $ident = irbuild_ir($m.match_hash{'ident'})  };
if ($m.match_hash{'ident'} && not($scope)) { $scope = "our" };
my $sig = IRx1::Signature.newp($m,[],undef);
if irbuild_ir($m.match_hash{'multisig'}) { $sig = irbuild_ir($m.match_hash{'multisig'}).[0] };
IRx1::SubDecl.newp($m,$scope,undef,$plurality,$ident,$sig,irbuild_ir($m.match_hash{'trait'}),irbuild_ir($m.match_hash{'block'}));
    };

    my $construct_routine_def = sub ($m) {
      my $scope = $blackboard::scope; temp $blackboard::scope;
my $plurality = $blackboard::plurality; temp $blackboard::plurality;
my $ident = "";
if $m.match_hash{'ident'} { $ident = irbuild_ir($m.match_hash{'ident'})  };
if ($m.match_hash{'ident'} && not($scope)) { $scope = "our" };
my $sig = IRx1::Signature.newp($m,[],undef);
if irbuild_ir($m.match_hash{'multisig'}) { $sig = irbuild_ir($m.match_hash{'multisig'}).[0] };
IRx1::SubDecl.newp($m,$scope,undef,$plurality,$ident,$sig,irbuild_ir($m.match_hash{'trait'}),irbuild_ir($m.match_hash{'block'}));
    };

    my $construct_routine_declarator_58method_def = sub ($m) {
      my $plurality = $blackboard::plurality; temp $blackboard::plurality;
my $multisig = irbuild_ir($m.match_hash{'multisig'});
if not($multisig) { $multisig = [IRx1::Signature.newp($m,[],undef)]; }
IRx1::MethodDecl.newp($m,undef,undef,$plurality,irbuild_ir($m.match_hash{'ident'}),$multisig.[0],irbuild_ir($m.match_hash{'trait'}),irbuild_ir($m.match_hash{'block'}),undef,undef);
    };

    my $construct_signature = sub ($m) {
      IRx1::Signature.newp($m,irbuild_ir($m.match_hash{'parsep'}),undef);
    };

    my $construct_parameter = sub ($m) {
      IRx1::Parameter.newp($m,irbuild_ir($m.match_hash{'type_constraint'}),irbuild_ir($m.match_hash{'quantchar'}),irbuild_ir($m.match_hash{'param_var'}),undef,undef,undef,undef);
    };

    my $construct_param_var = sub ($m) {
      IRx1::ParamVar.newp($m,irbuild_ir($m.match_hash{'sigil'}),irbuild_ir($m.match_hash{'twigil'}),irbuild_ir($m.match_hash{'ident'}));
    };

    my $construct_capture = sub ($m) {
      if not($m.match_hash{'EXPR'}) {
IRx1::Capture.newp1($m,[])
}
elsif $m.match_hash{'EXPR'}.match_hash{'noun'} {
IRx1::Capture.newp1($m,[irbuild_ir($m.match_hash{'EXPR'}.match_hash{'noun'})])
}
elsif $m.match_hash{'EXPR'}.match_hash{'sym'} && $m.match_hash{'EXPR'}.match_hash{'sym'} eq ':' {
my $args = irbuild_ir($m.match_hash{'EXPR'}.match_hash{'args'});
my $inv = $args.shift;
IRx1::Capture.newp($m,$args||[],$inv)
}
elsif $m.match_hash{'EXPR'}.match_hash{'sym'} && $m.match_hash{'EXPR'}.match_hash{'sym'} eq ',' {
my $args = $m.match_hash{'EXPR'}.match_hash{'args'};
my $arg0 = $args && $args[0];
my $inv = undef;
if $arg0 && $arg0.match_hash{'sym'} && $arg0.match_hash{'sym'} eq ':' {
  $args.shift;
  $inv = $arg0.match_hash{'args'}[0];
  if $arg0.match_hash{'args'}[1] {
    $args.unshift($arg0.match_hash{'args'}[1]);
  }
}
IRx1::Capture.newp($m,irbuild_ir($args)||[],irbuild_ir($inv))
}
else { die "capture AST form not recognized" };
    };

    my $construct_colonpair = sub ($m) {
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
$one;
    };

    my $construct_colonpair__false = sub ($m) {
      IRx1::Pair.newp($m,irbuild_ir($m.match_hash{'ident'}),IRx1::NumInt.newp($m,0));
    };

    my $construct_colonpair__value = sub ($m) {
      my $value;
if $m.match_hash{'postcircumfix'} {
$value = irbuild_ir($m.match_hash{'postcircumfix'}.match_hash{'kludge_name'});
} else {
$value = IRx1::NumInt.newp($m,1);
}
IRx1::Pair.newp($m,irbuild_ir($m.match_hash{'ident'}),$value);
    };

    my $construct_quotepair = sub ($m) {
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
$one;
    };

    my $construct_quotepair__false = sub ($m) {
      IRx1::Pair.newp($m,irbuild_ir($m.match_hash{'ident'}),IRx1::NumInt.newp($m,0));
    };

    my $construct_quotepair__value = sub ($m) {
      my $value;
if $m.match_hash{'postcircumfix'} {
$value = irbuild_ir($m.match_hash{'postcircumfix'}.match_hash{'kludge_name'});
} else {
$value = IRx1::NumInt.newp($m,1);
}
IRx1::Pair.newp($m,irbuild_ir($m.match_hash{'ident'}),$value);
    };

    my $construct_quotepair__nth = sub ($m) {
      IRx1::Pair.newp($m,'nth',irbuild_ir($m.match_hash{'n'}));
    };

    my $construct_package_declarator_58role = sub ($m) {
      temp $blackboard::package_declarator = 'role';
irbuild_ir($m.match_hash{'package_def'});
    };

    my $construct_package_declarator_58class = sub ($m) {
      temp $blackboard::package_declarator = 'class';
irbuild_ir($m.match_hash{'package_def'});
    };

    my $construct_package_declarator_58module = sub ($m) {
      temp $blackboard::package_declarator = 'module';
irbuild_ir($m.match_hash{'package_def'});
    };

    my $construct_package_declarator_58package = sub ($m) {
      temp $blackboard::package_declarator = 'package';
irbuild_ir($m.match_hash{'package_def'});
    };

    my $construct_package_declarator_58grammar = sub ($m) {
      temp $blackboard::package_declarator = 'grammar';
irbuild_ir($m.match_hash{'package_def'});
    };

    my $construct_package_def = sub ($m) {
      IRx1::PackageDecl.newp($m,undef,undef,$blackboard::package_declarator,irbuild_ir($m.match_hash{'module_name'}).[0],irbuild_ir($m.match_hash{'traits'}),irbuild_ir($m.match_hash{'block'}));
    };

    my $construct_fulltypename = sub ($m) {
      irbuild_ir($m.match_hash{'typename'}).join("::");
    };

    my $construct_typename = sub ($m) {
      ($m.match_string);
    };

    my $construct_trait_verb_58is = sub ($m) {
      IRx1::Trait.newp($m,'is',irbuild_ir($m.match_hash{'ident'}));
    };

    my $construct_trait_verb_58does = sub ($m) {
      IRx1::Trait.newp($m,'does',irbuild_ir($m.match_hash{'role_name'}));
    };

    my $construct_circumfix_58pblock = sub ($m) {
      if $m.match_hash{'block'}.match_hash{'statementlist'}.elems == 0 or $m.match_hash{'block'}.match_hash{'statementlist'}[0].match_string.re_matchp('^:') {
IRx1::Hash.newp($m,irbuild_ir($m.match_hash{'block'}.match_hash{'statementlist'}))
} elsif $m.match_hash{'block'}.match_hash{'statementlist'}[0].match_hash{'expr'} and $m.match_hash{'block'}.match_hash{'statementlist'}[0].match_hash{'expr'}.match_hash{'sym'} and $m.match_hash{'block'}.match_hash{'statementlist'}[0].match_hash{'expr'}.match_hash{'sym'} eq "," {
IRx1::Hash.newp($m,irbuild_ir($m.match_hash{'block'}.match_hash{'statementlist'}))
} elsif $m.match_hash{'block'}.match_hash{'statementlist'}[0].match_hash{'expr'} and $m.match_hash{'block'}.match_hash{'statementlist'}[0].match_hash{'expr'}.match_hash{'sym'} and $m.match_hash{'block'}.match_hash{'statementlist'}[0].match_hash{'expr'}.match_hash{'sym'} eq "=>" {
IRx1::Hash.newp($m,irbuild_ir($m.match_hash{'block'}.match_hash{'statementlist'}))
} elsif not(irbuild_ir($m.match_hash{'lambda'})) and not(irbuild_ir($m.match_hash{'signature'})) {
irbuild_ir($m.match_hash{'block'})
} else {
die "AST handler circumfix:pblock partially unimplemented";
};
    };

    my $construct_regex_declarator_58regex_def = sub ($m) {
      IRx1::RegexDef.newp($m,'regex',irbuild_ir($m.match_hash{'ident'}),undef,undef,irbuild_ir($m.match_hash{'regex_block'}));
    };

    my $construct_regex_block = sub ($m) {
      irbuild_ir($m.match_hash{'regex'});
    };

    my $construct_regex = sub ($m) {
      IRx1::Regex.newp($m,irbuild_ir($m.match_hash{'pattern'}));
    };

    my $construct_regex_first = sub ($m) {
      IRx1::RxFirst.newp($m,irbuild_ir($m.match_hash{'patterns'}));
    };

    my $construct_regex_every = sub ($m) {
      IRx1::RxEvery.newp($m,irbuild_ir($m.match_hash{'patterns'}));
    };

    my $construct_regex_submatch = sub ($m) {
      IRx1::RxSubmatch.newp($m,irbuild_ir($m.match_hash{'patterns'}));
    };

    my $construct_regex_any = sub ($m) {
      IRx1::RxAny.newp($m,irbuild_ir($m.match_hash{'patterns'}));
    };

    my $construct_regex_all = sub ($m) {
      IRx1::RxAll.newp($m,irbuild_ir($m.match_hash{'patterns'}));
    };

    my $construct_regex_sequence = sub ($m) {
      IRx1::RxSequence.newp($m,irbuild_ir($m.match_hash{'patterns'}));
    };

    my $construct_regex_quantified_atom = sub ($m) {
      IRx1::RxQuantifiedAtom.newp($m,irbuild_ir($m.match_hash{'regex_atom'}),irbuild_ir($m.match_hash{'regex_quantifier'}));
    };

    my $construct_regex_quantifier = sub ($m) {
      ($m.match_string);
    };

    my $construct_regex_atom = sub ($m) {
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
if irbuild_ir($m.match_hash{'char'}) { IRx1::RxLiteral.newp($m,irbuild_ir($m.match_hash{'char'}),"'") } else { $one };
    };

    my $construct_regex_metachar_58regex_backslash = sub ($m) {
      IRx1::RxBackslash.newp($m,($m.match_string));
    };

    my $construct_regex_metachar_58regex_mod_internal = sub ($m) {
      IRx1::RxModInternal.newp($m,($m.match_string));
    };

    my $construct_regex_assertion_58ident = sub ($m) {
      IRx1::RxAssertion.newp($m,irbuild_ir($m.match_hash{'ident'}));
    };

    my $construct_regex_metachar_58capture = sub ($m) {
      IRx1::RxCapture.newp($m,irbuild_ir($m.match_hash{'regex'}.match_hash{'pattern'}));
    };

    my $construct_regex_metachar_58group = sub ($m) {
      IRx1::RxGroup.newp($m,irbuild_ir($m.match_hash{'regex'}.match_hash{'pattern'}));
    };

    my $construct_regex_metachar_58block = sub ($m) {
      IRx1::RxBlock.newp($m,irbuild_ir($m.match_hash{'block'}));
    };

    my $construct_regex_metachar_58var = sub ($m) {
      IRx1::RxBind.newp($m,irbuild_ir($m.match_hash{'variable'}),irbuild_ir($m.match_hash{'binding'}));
    };

    my $construct_regex_metachar_58q = sub ($m) {
      IRx1::RxLiteral.newp($m,irbuild_ir($m.match_hash{'text'}),"'");
    };

    my $construct_regex_metachar_58qq = sub ($m) {
      IRx1::RxLiteral.newp($m,irbuild_ir($m.match_hash{'text'}),'"');
    };

    my $construct_regex_metachar = sub ($m) {
      IRx1::RxSymbol.newp($m,($m.match_string));
    };
method init {

$.add_constructor('comp_unit', $construct_comp_unit);
    $.add_constructor('statement', $construct_statement);
    $.add_constructor('expect_infix', $construct_expect_infix);
    $.add_constructor('fatarrow', $construct_fatarrow);
    $.add_constructor('expect_term', $construct_expect_term);
    $.add_constructor('term:expect_term', $construct_term_58expect_term);
    $.add_constructor('post', $construct_post);
    $.add_constructor('pre', $construct_pre);
    $.add_constructor('dotty:methodop', $construct_dotty_58methodop);
    $.add_constructor('dotty:.^!', $construct_dotty_58_46_94_33);
    $.add_constructor('dotty:postcircumfix', $construct_dotty_58postcircumfix);
    $.add_constructor('postcircumfix', $construct_postcircumfix);
    $.add_constructor('postfix', $construct_postfix);
    $.add_constructor('prefix', $construct_prefix);
    $.add_constructor('infix', $construct_infix);
    $.add_constructor('term', $construct_term);
    $.add_constructor('integer', $construct_integer);
    $.add_constructor('subcall', $construct_subcall);
    $.add_constructor('name', $construct_name);
    $.add_constructor('subshortname', $construct_subshortname);
    $.add_constructor('statement_control:use', $construct_statement_control_58use);
    $.add_constructor('module_name:depreciated', $construct_module_name_58depreciated);
    $.add_constructor('module_name:normal', $construct_module_name_58normal);
    $.add_constructor('role_name', $construct_role_name);
    $.add_constructor('statement_control:BEGIN', $construct_statement_control_58BEGIN);
    $.add_constructor('term:listop', $construct_term_58listop);
    $.add_constructor('quote:q', $construct_quote_58q);
    $.add_constructor('quote:qq', $construct_quote_58qq);
    $.add_constructor('quote:regex', $construct_quote_58regex);
    $.add_constructor('scope_declarator:my', $construct_scope_declarator_58my);
    $.add_constructor('scope_declarator:has', $construct_scope_declarator_58has);
    $.add_constructor('scope_declarator:our', $construct_scope_declarator_58our);
    $.add_constructor('scope_declarator:temp', $construct_scope_declarator_58temp);
    $.add_constructor('scoped', $construct_scoped);
    $.add_constructor('variable_decl', $construct_variable_decl);
    $.add_constructor('variable', $construct_variable);
    $.add_constructor('sigil', $construct_sigil);
    $.add_constructor('twigil', $construct_twigil);
    $.add_constructor('special_variable', $construct_special_variable);
    $.add_constructor('circumfix', $construct_circumfix);
    $.add_constructor('statement_control:for', $construct_statement_control_58for);
    $.add_constructor('statement_mod_loop:for', $construct_statement_mod_loop_58for);
    $.add_constructor('statement_control:while', $construct_statement_control_58while);
    $.add_constructor('statement_mod_loop:while', $construct_statement_mod_loop_58while);
    $.add_constructor('statement_control:until', $construct_statement_control_58until);
    $.add_constructor('statement_mod_loop:until', $construct_statement_mod_loop_58until);
    $.add_constructor('statement_control:loop', $construct_statement_control_58loop);
    $.add_constructor('statement_control:if', $construct_statement_control_58if);
    $.add_constructor('elsif', $construct_elsif);
    $.add_constructor('if__else', $construct_if__else);
    $.add_constructor('statement_mod_cond:if', $construct_statement_mod_cond_58if);
    $.add_constructor('statement_control:unless', $construct_statement_control_58unless);
    $.add_constructor('statement_mod_cond:unless', $construct_statement_mod_cond_58unless);
    $.add_constructor('statement_control:given', $construct_statement_control_58given);
    $.add_constructor('statement_mod_loop:given', $construct_statement_mod_loop_58given);
    $.add_constructor('statement_control:when', $construct_statement_control_58when);
    $.add_constructor('statement_mod_cond:when', $construct_statement_mod_cond_58when);
    $.add_constructor('statement_control:default', $construct_statement_control_58default);
    $.add_constructor('statement_prefix:do', $construct_statement_prefix_58do);
    $.add_constructor('statement_prefix:try', $construct_statement_prefix_58try);
    $.add_constructor('statement_prefix:gather', $construct_statement_prefix_58gather);
    $.add_constructor('statement_prefix:contend', $construct_statement_prefix_58contend);
    $.add_constructor('statement_prefix:async', $construct_statement_prefix_58async);
    $.add_constructor('statement_prefix:lazy', $construct_statement_prefix_58lazy);
    $.add_constructor('pblock', $construct_pblock);
    $.add_constructor('block', $construct_block);
    $.add_constructor('plurality_declarator:multi', $construct_plurality_declarator_58multi);
    $.add_constructor('routine_declarator:routine_def', $construct_routine_declarator_58routine_def);
    $.add_constructor('routine_def', $construct_routine_def);
    $.add_constructor('routine_declarator:method_def', $construct_routine_declarator_58method_def);
    $.add_constructor('signature', $construct_signature);
    $.add_constructor('parameter', $construct_parameter);
    $.add_constructor('param_var', $construct_param_var);
    $.add_constructor('capture', $construct_capture);
    $.add_constructor('colonpair', $construct_colonpair);
    $.add_constructor('colonpair__false', $construct_colonpair__false);
    $.add_constructor('colonpair__value', $construct_colonpair__value);
    $.add_constructor('quotepair', $construct_quotepair);
    $.add_constructor('quotepair__false', $construct_quotepair__false);
    $.add_constructor('quotepair__value', $construct_quotepair__value);
    $.add_constructor('quotepair__nth', $construct_quotepair__nth);
    $.add_constructor('package_declarator:role', $construct_package_declarator_58role);
    $.add_constructor('package_declarator:class', $construct_package_declarator_58class);
    $.add_constructor('package_declarator:module', $construct_package_declarator_58module);
    $.add_constructor('package_declarator:package', $construct_package_declarator_58package);
    $.add_constructor('package_declarator:grammar', $construct_package_declarator_58grammar);
    $.add_constructor('package_def', $construct_package_def);
    $.add_constructor('fulltypename', $construct_fulltypename);
    $.add_constructor('typename', $construct_typename);
    $.add_constructor('trait_verb:is', $construct_trait_verb_58is);
    $.add_constructor('trait_verb:does', $construct_trait_verb_58does);
    $.add_constructor('circumfix:pblock', $construct_circumfix_58pblock);
    $.add_constructor('regex_declarator:regex_def', $construct_regex_declarator_58regex_def);
    $.add_constructor('regex_block', $construct_regex_block);
    $.add_constructor('regex', $construct_regex);
    $.add_constructor('regex_first', $construct_regex_first);
    $.add_constructor('regex_every', $construct_regex_every);
    $.add_constructor('regex_submatch', $construct_regex_submatch);
    $.add_constructor('regex_any', $construct_regex_any);
    $.add_constructor('regex_all', $construct_regex_all);
    $.add_constructor('regex_sequence', $construct_regex_sequence);
    $.add_constructor('regex_quantified_atom', $construct_regex_quantified_atom);
    $.add_constructor('regex_quantifier', $construct_regex_quantifier);
    $.add_constructor('regex_atom', $construct_regex_atom);
    $.add_constructor('regex_metachar:regex_backslash', $construct_regex_metachar_58regex_backslash);
    $.add_constructor('regex_metachar:regex_mod_internal', $construct_regex_metachar_58regex_mod_internal);
    $.add_constructor('regex_assertion:ident', $construct_regex_assertion_58ident);
    $.add_constructor('regex_metachar:capture', $construct_regex_metachar_58capture);
    $.add_constructor('regex_metachar:group', $construct_regex_metachar_58group);
    $.add_constructor('regex_metachar:block', $construct_regex_metachar_58block);
    $.add_constructor('regex_metachar:var', $construct_regex_metachar_58var);
    $.add_constructor('regex_metachar:q', $construct_regex_metachar_58q);
    $.add_constructor('regex_metachar:qq', $construct_regex_metachar_58qq);
    $.add_constructor('regex_metachar', $construct_regex_metachar);

      self;
    }; # end init
  };


  if not($*ast2ir_0) { $*ast2ir_0 = IRx1_Build.new.init; }
  $*ast2ir_1 = IRx1_Build.new.init;

