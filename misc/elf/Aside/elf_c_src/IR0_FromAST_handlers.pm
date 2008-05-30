#line 2 ast_handlers.pl
# Warning: This file is mechanically written.  Your changes will be overwritten.
package IRBuild {
    $main::irbuilder.add_constructor('comp_unit', sub ($m) {
    IR0::CompUnit.newp($m,irbuild_ir($m.{'hash'}{'statementlist'}));
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
for ($post) {
$^blackboard::expect_term_base = irbuild_ir($_)
}
$^blackboard::expect_term_base;
    });
    $main::irbuilder.add_constructor('post', sub ($m) {
    irbuild_ir($m.{'hash'}{'dotty'}) or irbuild_ir($m.{'hash'}{'postop'});
    });
    $main::irbuilder.add_constructor('dotty:methodop', sub ($m) {
    IR0::Call.newp($m,$^blackboard::expect_term_base,undef,irbuild_ir($m.{'hash'}{'ident'}),irbuild_ir($m.{'hash'}{'semilist'}));
    });
    $main::irbuilder.add_constructor('dotty:postcircumfix', sub ($m) {
    my $s = ($m.match_string);
my $name = substr($s,0,1)~' '~substr($s,-1,1);
my $ident = "postcircumfix:"~$name;
IR0::Call.newp($m,$^blackboard::expect_term_base,undef,$ident,irbuild_ir($m.{'hash'}{'kludge_name'}));
    });
    $main::irbuilder.add_constructor('postcircumfix', sub ($m) {
    my $s = ($m.match_string);
my $name = substr($s,0,1)~' '~substr($s,-1,1);
my $ident = "postcircumfix:"~$name;
IR0::Call.newp($m,$^blackboard::expect_term_base,undef,$ident,irbuild_ir($m.{'hash'}{'kludge_name'}));
    });
    $main::irbuilder.add_constructor('term:expect_term', sub ($m) {
    irbuild_ir($m.{'hash'}{'noun'});
    });
    $main::irbuilder.add_constructor('term', sub ($m) {
    if (($m.match_string) eq 'self') {
IR0::Apply.newp($m,'self',[])
} else {
die "AST term partially unimplemented.\n";
};
    });
    $main::irbuilder.add_constructor('integer', sub ($m) {
    IR0::Val_Int.newp($m,($m.match_string));
    });
    $main::irbuilder.add_constructor('subcall', sub ($m) {
    my $t = irbuild_ir($m.{'hash'}{'subshortname'}.{'hash'}{'twigil'});
if($t && $t eq '~') {
IR0::Call.newp($m,IR0::Apply.newp($m,'self',[]),undef,irbuild_ir($m.{'hash'}{'subshortname'}.{'hash'}{'desigilname'}.{'hash'}{'ident'}),irbuild_ir($m.{'hash'}{'semilist'}))
} else {
IR0::Apply.newp($m,irbuild_ir($m.{'hash'}{'subshortname'}),irbuild_ir($m.{'hash'}{'semilist'}))
};
    });
    $main::irbuilder.add_constructor('name', sub ($m) {
    ($m.match_string);
    });
    $main::irbuilder.add_constructor('statement_control:use', sub ($m) {
    IR0::Use.newp($m,irbuild_ir($m.{'hash'}{'module_name'}));
    });
    $main::irbuilder.add_constructor('module_name:depreciated', sub ($m) {
    ($m.match_string);
    });
    $main::irbuilder.add_constructor('module_name:normal', sub ($m) {
    ($m.match_string);
    });
    $main::irbuilder.add_constructor('term:listop', sub ($m) {
    my $not_really_an_arglist = irbuild_ir($m.{'hash'}{'arglist'});
if(irbuild_ir($m.{'hash'}{'arglist'})) {
IR0::Apply.newp($m,irbuild_ir($m.{'hash'}{'ident'}),[$not_really_an_arglist])
} else {
IR0::Apply.newp($m,irbuild_ir($m.{'hash'}{'ident'}),[])
};
    });
    $main::irbuilder.add_constructor('quote:q', sub ($m) {
    IR0::Val_Buf.newp($m,irbuild_ir($m.{'hash'}{'text'}));
    });
    $main::irbuilder.add_constructor('quote:qq', sub ($m) {
    my $s = irbuild_ir($m.{'hash'}{'text'});
$s.re_gsub(/(?<!\\)\\n/,"\n");
$s.re_gsub(/(?<!\\)\\t/,"\t");
IR0::Val_Buf.newp($m,$s);
    });
    $main::irbuilder.add_constructor('quote:regex', sub ($m) {
    my $s = irbuild_ir($m.{'hash'}{'text'});
IR0::Val_Rx.newp($m,$s);
    });
    $main::irbuilder.add_constructor('infix', sub ($m) {
    my $op = ($m.match_string);
if($op eq 'str') { $op = '=' };
IR0::Apply.newp($m,"infix:"~$op,[irbuild_ir($m.{'hash'}{'left'}),irbuild_ir($m.{'hash'}{'right'})]);
    });
    $main::irbuilder.add_constructor('scope_declarator:my', sub ($m) {
    my $vd = irbuild_ir($m.{'hash'}{'scoped'});
IR0::Decl.newp($m,'my',undef,$vd.[0],$vd.[1]);
    });
    $main::irbuilder.add_constructor('scope_declarator:has', sub ($m) {
    my $vd = irbuild_ir($m.{'hash'}{'scoped'});
IR0::Decl.newp($m,'has',undef,$vd.[0],$vd.[1]);
    });
    $main::irbuilder.add_constructor('scope_declarator:our', sub ($m) {
    my $vd = irbuild_ir($m.{'hash'}{'scoped'});
IR0::Decl.newp($m,'our',undef,$vd.[0],$vd.[1]);
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
    IR0::Var.newp($m,irbuild_ir($m.{'hash'}{'sigil'}),irbuild_ir($m.{'hash'}{'twigil'}),irbuild_ir($m.{'hash'}{'desigilname'}));
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
IR0::Apply.newp($m,"circumfix:"~$name,irbuild_ir($m.{'hash'}{'kludge_name'}));
    });
    $main::irbuilder.add_constructor('statement_control:for', sub ($m) {
    IR0::For.newp($m,irbuild_ir($m.{'hash'}{'expr'}),irbuild_ir($m.{'hash'}{'block'}));
    });
    $main::irbuilder.add_constructor('statement_control:while', sub ($m) {
    IR0::While.newp($m,irbuild_ir($m.{'hash'}{'expr'}),irbuild_ir($m.{'hash'}{'block'}));
    });
    $main::irbuilder.add_constructor('statement_control:if', sub ($m) {
    IR0::If.newp($m,irbuild_ir($m.{'hash'}{'if_expr'}),irbuild_ir($m.{'hash'}{'if_block'}),irbuild_ir($m.{'hash'}{'elsif'}),irbuild_ir($m.{'hash'}{'else'}));
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
    IR0::Block.newp($m,irbuild_ir($m.{'hash'}{'statementlist'}));
    });
    $main::irbuilder.add_constructor('routine_declarator:routine_def', sub ($m) {
    my $ident = "";
if(irbuild_ir($m.{'hash'}{'ident'})) { $ident = irbuild_ir($m.{'hash'}{'ident'}).[0] };
my $sig = IR0::Sig.newp($m,undef,[]);
if(irbuild_ir($m.{'hash'}{'multisig'})) { $sig = irbuild_ir($m.{'hash'}{'multisig'}).[0] };
IR0::Sub.newp($m,$ident,$sig,irbuild_ir($m.{'hash'}{'block'}));
    });
    $main::irbuilder.add_constructor('routine_declarator:method_def', sub ($m) {
    IR0::Method.newp($m,irbuild_ir($m.{'hash'}{'ident'}),irbuild_ir($m.{'hash'}{'multisig'}).[0],irbuild_ir($m.{'hash'}{'block'}));
    });
    $main::irbuilder.add_constructor('signature', sub ($m) {
    IR0::Sig.newp($m,undef,irbuild_ir($m.{'hash'}{'parsep'}));
    });
    $main::irbuilder.add_constructor('parameter', sub ($m) {
    IR0::Lit_SigArgument.newp($m,irbuild_ir($m.{'hash'}{'param_var'}));
    });
    $main::irbuilder.add_constructor('param_var', sub ($m) {
    IR0::Var.newp($m,irbuild_ir($m.{'hash'}{'sigil'}),irbuild_ir($m.{'hash'}{'twigil'}),irbuild_ir($m.{'hash'}{'ident'}));
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
    IR0::PackageDeclarator.newp($m,$^blackboard::package_declarator,irbuild_ir($m.{'hash'}{'module_name'}).[0],irbuild_ir($m.{'hash'}{'traits'}),irbuild_ir($m.{'hash'}{'block'}));
    });
    $main::irbuilder.add_constructor('fulltypename', sub ($m) {
    join("::",irbuild_ir($m.{'hash'}{'typename'}).flatten);
    });
    $main::irbuilder.add_constructor('typename', sub ($m) {
    ($m.match_string);
    });
    $main::irbuilder.add_constructor('trait_verb:is', sub ($m) {
    IR0::Trait.newp($m,'is',irbuild_ir($m.{'hash'}{'ident'}));
    });
    $main::irbuilder.add_constructor('circumfix:pblock', sub ($m) {
    if (not irbuild_ir($m.{'hash'}{'lambda'}) and not irbuild_ir($m.{'hash'}{'signature'})) {
IR0::Lit_Hash.newp($m,irbuild_ir($m.{'hash'}{'block'}.{'hash'}{'statementlist'}))
} else {
die "AST handler circumfix:pblock partially unimplemented";
}
;
    });
}
