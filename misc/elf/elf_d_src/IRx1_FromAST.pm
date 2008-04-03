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
    if($constructor) {
      $constructor.($m);
    } else {
      die "Unknown rule: $rule\nIt needs to be added to ast_handlers.\n";
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

sub irbuild_ir ($x) {
  $x.make_ir_from_Match_tree()
};

package IRx1_Build {
    $main::irbuilder.add_constructor('comp_unit', sub ($m) {
    IRx1::CompUnit.newp($m,irbuild_ir($m.{'hash'}{'statementlist'}));
    });
    $main::irbuilder.add_constructor('statement', sub ($m) {
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
    $main::irbuilder.add_constructor('expect_term', sub ($m) {
    my $^blackboard::expect_term_base = irbuild_ir($m.{'hash'}{'noun'});
my $post = $m.{'hash'}{'post'} || [];
for $post {
$^blackboard::expect_term_base = irbuild_ir($_)
}
$^blackboard::expect_term_base;
    });
    $main::irbuilder.add_constructor('post', sub ($m) {
    irbuild_ir($m.{'hash'}{'dotty'}) or irbuild_ir($m.{'hash'}{'postop'});
    });
    $main::irbuilder.add_constructor('dotty:methodop', sub ($m) {
    IRx1::Call.newp($m,$^blackboard::expect_term_base,irbuild_ir($m.{'hash'}{'ident'}),IRx1::Capture.newp($m,irbuild_ir($m.{'hash'}{'semilist'})));
    });
    $main::irbuilder.add_constructor('dotty:postcircumfix', sub ($m) {
    my $s = ($m.match_string);
my $name = substr($s,0,1)~' '~substr($s,-1,1);
my $ident = "postcircumfix:"~$name;
my $args = irbuild_ir($m.{'hash'}{'kludge_name'});
if $args && ($args.ref eq 'SCALAR')  { $args = [$args] }
IRx1::Call.newp($m,$^blackboard::expect_term_base,$ident,IRx1::Capture.newp($m,$args));
    });
    $main::irbuilder.add_constructor('postcircumfix', sub ($m) {
    my $s = ($m.match_string);
my $name = substr($s,0,1)~' '~substr($s,-1,1);
my $ident = "postcircumfix:"~$name;
my $args = irbuild_ir($m.{'hash'}{'kludge_name'});
if $args && ($args.ref eq 'SCALAR')  { $args = [$args] }
IRx1::Call.newp($m,$^blackboard::expect_term_base,$ident,IRx1::Capture.newp($m,$args));
    });
    $main::irbuilder.add_constructor('term:expect_term', sub ($m) {
    irbuild_ir($m.{'hash'}{'noun'});
    });
    $main::irbuilder.add_constructor('term', sub ($m) {
    if ($m.match_string) eq 'self' {
IRx1::Apply.newp($m,'self',IRx1::Capture.newp($m,[]))
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
IRx1::Call.newp($m,IRx1::Apply.newp($m,'self',IRx1::Capture.newp($m,[])),irbuild_ir($m.{'hash'}{'subshortname'}.{'hash'}{'desigilname'}.{'hash'}{'ident'}),IRx1::Capture.newp($m,irbuild_ir($m.{'hash'}{'semilist'})))
} else {
IRx1::Apply.newp($m,irbuild_ir($m.{'hash'}{'subshortname'}),IRx1::Capture.newp($m,irbuild_ir($m.{'hash'}{'semilist'})))
};
    });
    $main::irbuilder.add_constructor('name', sub ($m) {
    ($m.match_string);
    });
    $main::irbuilder.add_constructor('statement_control:use', sub ($m) {
    IRx1::Use.newp($m,'use',irbuild_ir($m.{'hash'}{'module_name'}));
    });
    $main::irbuilder.add_constructor('module_name:depreciated', sub ($m) {
    ($m.match_string);
    });
    $main::irbuilder.add_constructor('module_name:normal', sub ($m) {
    ($m.match_string);
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
    IRx1::Buf.newp($m,irbuild_ir($m.{'hash'}{'text'}));
    });
    $main::irbuilder.add_constructor('quote:qq', sub ($m) {
    my $s = irbuild_ir($m.{'hash'}{'text'});
$s.re_gsub(/(?<!\\)\\n/,"\n");
$s.re_gsub(/(?<!\\)\\t/,"\t");
IRx1::Buf.newp($m,$s);
    });
    $main::irbuilder.add_constructor('quote:regex', sub ($m) {
    my $s = irbuild_ir($m.{'hash'}{'text'});
IRx1::Rx.newp($m,$s);
    });
    $main::irbuilder.add_constructor('infix', sub ($m) {
    my $op = ($m.match_string);
if $op eq 'str' { $op = '=' };
IRx1::Apply.newp($m,"infix:"~$op,IRx1::Capture.newp($m,[irbuild_ir($m.{'hash'}{'left'}),irbuild_ir($m.{'hash'}{'right'})]));
    });
    $main::irbuilder.add_constructor('scope_declarator:my', sub ($m) {
    my $vd = irbuild_ir($m.{'hash'}{'scoped'});
IRx1::VarDecl.newp($m,'my',undef,undef,$vd.[0],undef,undef,'=',$vd.[1]);
    });
    $main::irbuilder.add_constructor('scope_declarator:has', sub ($m) {
    my $vd = irbuild_ir($m.{'hash'}{'scoped'});
IRx1::VarDecl.newp($m,'has',undef,undef,$vd.[0],undef,undef,'=',$vd.[1]);
    });
    $main::irbuilder.add_constructor('scope_declarator:our', sub ($m) {
    my $vd = irbuild_ir($m.{'hash'}{'scoped'});
IRx1::VarDecl.newp($m,'our',undef,undef,$vd.[0],undef,undef,'=',$vd.[1]);
    });
    $main::irbuilder.add_constructor('scoped', sub ($m) {
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
    $main::irbuilder.add_constructor('variable_decl', sub ($m) {
    [irbuild_ir($m.{'hash'}{'variable'}),irbuild_ir($m.{'hash'}{'default_value'})];
    });
    $main::irbuilder.add_constructor('variable', sub ($m) {
    IRx1::Var.newp($m,irbuild_ir($m.{'hash'}{'sigil'}),irbuild_ir($m.{'hash'}{'twigil'}),irbuild_ir($m.{'hash'}{'desigilname'}));
    });
    $main::irbuilder.add_constructor('sigil', sub ($m) {
    ($m.match_string);
    });
    $main::irbuilder.add_constructor('twigil', sub ($m) {
    ($m.match_string);
    });
    $main::irbuilder.add_constructor('circumfix', sub ($m) {
    my $s = ($m.match_string);
my $name = substr($s,0,1)~' '~substr($s,-1,1);
my $args = irbuild_ir($m.{'hash'}{'kludge_name'});
if $args && ($args.ref eq 'SCALAR')  { $args = [$args] }
IRx1::Apply.newp($m,"circumfix:"~$name,IRx1::Capture.newp($m,$args));
    });
    $main::irbuilder.add_constructor('statement_control:for', sub ($m) {
    IRx1::For.newp($m,irbuild_ir($m.{'hash'}{'expr'}),irbuild_ir($m.{'hash'}{'block'}));
    });
    $main::irbuilder.add_constructor('statement_control:while', sub ($m) {
    IRx1::Loop.newp($m,irbuild_ir($m.{'hash'}{'expr'}),irbuild_ir($m.{'hash'}{'block'}));
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
    $main::irbuilder.add_constructor('pblock', sub ($m) {
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
    $main::irbuilder.add_constructor('block', sub ($m) {
    IRx1::Block.newp($m,irbuild_ir($m.{'hash'}{'statementlist'}));
    });
    $main::irbuilder.add_constructor('routine_declarator:routine_def', sub ($m) {
    my $ident = "";
if irbuild_ir($m.{'hash'}{'ident'}) { $ident = irbuild_ir($m.{'hash'}{'ident'}).[0] };
my $sig = IRx1::Signature.newp($m,[],undef);
if irbuild_ir($m.{'hash'}{'multisig'}) { $sig = irbuild_ir($m.{'hash'}{'multisig'}).[0] };
IRx1::SubDecl.newp($m,undef,undef,undef,$ident,$sig,undef,irbuild_ir($m.{'hash'}{'block'}));
    });
    $main::irbuilder.add_constructor('routine_declarator:method_def', sub ($m) {
    IRx1::MethodDecl.newp($m,undef,undef,undef,irbuild_ir($m.{'hash'}{'ident'}),irbuild_ir($m.{'hash'}{'multisig'}).[0],undef,irbuild_ir($m.{'hash'}{'block'}));
    });
    $main::irbuilder.add_constructor('signature', sub ($m) {
    IRx1::Signature.newp($m,irbuild_ir($m.{'hash'}{'parsep'}),undef);
    });
    $main::irbuilder.add_constructor('parameter', sub ($m) {
    IRx1::Parameter.newp($m,undef,undef,undef,irbuild_ir($m.{'hash'}{'param_var'}));
    });
    $main::irbuilder.add_constructor('param_var', sub ($m) {
    IRx1::ParamVar.newp($m,irbuild_ir($m.{'hash'}{'sigil'}),irbuild_ir($m.{'hash'}{'twigil'}),irbuild_ir($m.{'hash'}{'ident'}));
    });
    $main::irbuilder.add_constructor('package_declarator:class', sub ($m) {
    my $^blackboard::package_declarator = 'class';
irbuild_ir($m.{'hash'}{'package_def'});
    });
    $main::irbuilder.add_constructor('package_declarator:module', sub ($m) {
    my $^blackboard::package_declarator = 'module';
irbuild_ir($m.{'hash'}{'package_def'});
    });
    $main::irbuilder.add_constructor('package_declarator:package', sub ($m) {
    my $^blackboard::package_declarator = 'package';
irbuild_ir($m.{'hash'}{'package_def'});
    });
    $main::irbuilder.add_constructor('package_def', sub ($m) {
    IRx1::PackageDecl.newp($m,undef,undef,$^blackboard::package_declarator,irbuild_ir($m.{'hash'}{'module_name'}).[0],irbuild_ir($m.{'hash'}{'traits'}),irbuild_ir($m.{'hash'}{'block'}));
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
    $main::irbuilder.add_constructor('circumfix:pblock', sub ($m) {
    if not(irbuild_ir($m.{'hash'}{'lambda'})) and not(irbuild_ir($m.{'hash'}{'signature'})) {
IRx1::Hash.newp($m,irbuild_ir($m.{'hash'}{'block'}.{'hash'}{'statementlist'}))
} else {
die "AST handler circumfix:pblock partially unimplemented";
}
;
    });
}
