#line 2 ast_handlers.pl
# Warning: This file is mechanically written.  Your changes will be overwritten.
{ package IRBuild;
    $IRBuild::constructors{'comp_unit'} = sub {
      my($m)=@_;
    IR::CompUnit->new($m,ir($m->{hash}{statementlist}));
    };
    $IRBuild::constructors{'statement'} = sub {
      my($m)=@_;
      my @keys = map{$_ eq "match" ? () : ($_)} keys %{$m->{hash}};
die("Unexpectedly more than 1 field - dont know which to choose\n".
    $m->match_describe."\n") if(@keys > 1);
my $one = ir($m->{hash}{$keys[0]});
$one;
    };
    $IRBuild::constructors{'expect_term'} = sub {
      my($m)=@_;
    local $blackboard::expect_term_base = ir($m->{hash}{noun});
for my $post (@{$m->{hash}{post}||[]}) {
$blackboard::expect_term_base = ir($post)
}
$blackboard::expect_term_base;
    };
    $IRBuild::constructors{'post'} = sub {
      my($m)=@_;
      my @keys = map{$_ eq "match" ? () : ($_)} keys %{$m->{hash}};
die("Unexpectedly more than 1 field - dont know which to choose\n".
    $m->match_describe."\n") if(@keys > 1);
my $one = ir($m->{hash}{$keys[0]});
$one;
    };
    $IRBuild::constructors{'dotty:methodop'} = sub {
      my($m)=@_;
    IR::Call->new($m,$blackboard::expect_term_base,undef,ir($m->{hash}{ident}),ir($m->{hash}{semilist}));
    };
    $IRBuild::constructors{'dotty:postcircumfix'} = sub {
      my($m)=@_;
    my $s = ($m->match_string);
my $name = substr($s,0,1).' '.substr($s,-1,1);
my $ident = "postcircumfix:".$name;
IR::Call->new($m,$blackboard::expect_term_base,undef,$ident,ir($m->{hash}{kludge_name}));
    };
    $IRBuild::constructors{'term:expect_term'} = sub {
      my($m)=@_;
    ir($m->{hash}{noun});
    };
    $IRBuild::constructors{'term'} = sub {
      my($m)=@_;
    if(($m->match_string) eq 'self') {
IR::Apply->new($m,'self',[])
} else {
die "AST term partially unimplemented.\n";
};
    };
    $IRBuild::constructors{'integer'} = sub {
      my($m)=@_;
    IR::Val_Int->new($m,($m->match_string));
    };
    $IRBuild::constructors{'subcall'} = sub {
      my($m)=@_;
    IR::Apply->new($m,ir($m->{hash}{subshortname}),ir($m->{hash}{semilist}));
    };
    $IRBuild::constructors{'name'} = sub {
      my($m)=@_;
    ir($m->{hash}{ident});
    };
    $IRBuild::constructors{'statement_control:use'} = sub {
      my($m)=@_;
    IR::Use->new($m,ir($m->{hash}{module_name}));
    };
    $IRBuild::constructors{'module_name:depreciated'} = sub {
      my($m)=@_;
    ($m->match_string);
    };
    $IRBuild::constructors{'module_name:normal'} = sub {
      my($m)=@_;
    ($m->match_string);
    };
    $IRBuild::constructors{'term:listop'} = sub {
      my($m)=@_;
    IR::Apply->new($m,ir($m->{hash}{ident}),[ir($m->{hash}{arglist})]);
    };
    $IRBuild::constructors{'quote:q'} = sub {
      my($m)=@_;
    IR::Val_Buf->new($m,ir($m->{hash}{text}));
    };
    $IRBuild::constructors{'quote:qq'} = sub {
      my($m)=@_;
    my $s = ir($m->{hash}{text});
$s =~ s/(?<!\\)\\n/\n/g;
$s =~ s/(?<!\\)\\t/\t/g;
IR::Val_Buf->new($m,$s);
    };
    $IRBuild::constructors{'infix'} = sub {
      my($m)=@_;
    my $op = ($m->match_string);
$op = '=' if $op eq 'str';
IR::Apply->new($m,"infix:".$op,[ir($m->{hash}{left}),ir($m->{hash}{right})]);
    };
    $IRBuild::constructors{'scope_declarator:my'} = sub {
      my($m)=@_;
    my $vd = ir($m->{hash}{scoped});
IR::Decl->new($m,'my',undef,$vd->[0],$vd->[1]);
    };
    $IRBuild::constructors{'scope_declarator:has'} = sub {
      my($m)=@_;
    my $vd = ir($m->{hash}{scoped});
IR::Decl->new($m,'has',undef,$vd->[0],$vd->[1]);
    };
    $IRBuild::constructors{'scoped'} = sub {
      my($m)=@_;
      my @keys = map{$_ eq "match" ? () : ($_)} keys %{$m->{hash}};
die("Unexpectedly more than 1 field - dont know which to choose\n".
    $m->match_describe."\n") if(@keys > 1);
my $one = ir($m->{hash}{$keys[0]});
$one;
    };
    $IRBuild::constructors{'variable_decl'} = sub {
      my($m)=@_;
    [ir($m->{hash}{variable}),ir($m->{hash}{default_value})];
    };
    $IRBuild::constructors{'variable'} = sub {
      my($m)=@_;
    IR::Var->new($m,ir($m->{hash}{sigil}),ir($m->{hash}{twigil}),ir($m->{hash}{desigilname}));
    };
    $IRBuild::constructors{'sigil'} = sub {
      my($m)=@_;
    ($m->match_string);
    };
    $IRBuild::constructors{'twigil'} = sub {
      my($m)=@_;
    ($m->match_string);
    };
    $IRBuild::constructors{'circumfix'} = sub {
      my($m)=@_;
    my $s = ($m->match_string);
my $name = substr($s,0,1).' '.substr($s,-1,1);
IR::Apply->new($m,"circumfix:".$name,ir($m->{hash}{kludge_name}));
    };
    $IRBuild::constructors{'statement_control:if'} = sub {
      my($m)=@_;
    IR::If->new($m,ir($m->{hash}{if_expr}),ir($m->{hash}{if_block}),ir($m->{hash}{elsif}),ir($m->{hash}{else}));
    };
    $IRBuild::constructors{'elsif'} = sub {
      my($m)=@_;
    [ir($m->{hash}{elsif_expr}),ir($m->{hash}{elsif_block})];
    };
    $IRBuild::constructors{'if__else'} = sub {
      my($m)=@_;
      my @keys = map{$_ eq "match" ? () : ($_)} keys %{$m->{hash}};
die("Unexpectedly more than 1 field - dont know which to choose\n".
    $m->match_describe."\n") if(@keys > 1);
my $one = ir($m->{hash}{$keys[0]});
$one;
    };
    $IRBuild::constructors{'pblock'} = sub {
      my($m)=@_;
      my @keys = map{$_ eq "match" ? () : ($_)} keys %{$m->{hash}};
die("Unexpectedly more than 1 field - dont know which to choose\n".
    $m->match_describe."\n") if(@keys > 1);
my $one = ir($m->{hash}{$keys[0]});
$one;
    };
    $IRBuild::constructors{'block'} = sub {
      my($m)=@_;
    IR::Block->new($m,ir($m->{hash}{statementlist}));
    };
    $IRBuild::constructors{'routine_declarator:routine_def'} = sub {
      my($m)=@_;
    my $ident = ir($m->{hash}{ident}) ? ir($m->{hash}{ident})->[0] : "";
my $sig = ir($m->{hash}{multisig}) ? ir($m->{hash}{multisig})->[0] : IR::Sig->new($m,undef,[]);
IR::Sub->new($m,$ident,$sig,ir($m->{hash}{block}));
    };
    $IRBuild::constructors{'routine_declarator:method_def'} = sub {
      my($m)=@_;
    IR::Method->new($m,ir($m->{hash}{ident}),ir($m->{hash}{multisig})->[0],ir($m->{hash}{block}));
    };
    $IRBuild::constructors{'signature'} = sub {
      my($m)=@_;
    IR::Sig->new($m,undef,ir($m->{hash}{parsep}));
    };
    $IRBuild::constructors{'parameter'} = sub {
      my($m)=@_;
    IR::Lit_SigArgument->new($m,ir($m->{hash}{param_var}));
    };
    $IRBuild::constructors{'param_var'} = sub {
      my($m)=@_;
    IR::Var->new($m,ir($m->{hash}{sigil}),ir($m->{hash}{twigil}),ir($m->{hash}{ident}));
    };
    $IRBuild::constructors{'package_declarator:class'} = sub {
      my($m)=@_;
    local $blackboard::package_declarator = 'class';
ir($m->{hash}{package_def});
    };
    $IRBuild::constructors{'package_declarator:module'} = sub {
      my($m)=@_;
    local $blackboard::package_declarator = 'module';
ir($m->{hash}{package_def});
    };
    $IRBuild::constructors{'package_declarator:package'} = sub {
      my($m)=@_;
    local $blackboard::package_declarator = 'package';
ir($m->{hash}{package_def});
    };
    $IRBuild::constructors{'package_def'} = sub {
      my($m)=@_;
    IR::PackageDeclarator->new($m,$blackboard::package_declarator,ir($m->{hash}{module_name})->[0],ir($m->{hash}{traits}),ir($m->{hash}{block}));
    };
    $IRBuild::constructors{'fulltypename'} = sub {
      my($m)=@_;
    join("::",@{ir($m->{hash}{typename})});
    };
    $IRBuild::constructors{'typename'} = sub {
      my($m)=@_;
    ir($m->{hash}{name});
    };
    $IRBuild::constructors{'trait_verb:is'} = sub {
      my($m)=@_;
    IR::Trait->new($m,'is',ir($m->{hash}{ident}));
    };
    $IRBuild::constructors{'circumfix:pblock'} = sub {
      my($m)=@_;
    if(not ir($m->{hash}{lambda}) and not ir($m->{hash}{signature})) {
IR::Lit_Hash->new($m,ir($m->{hash}{block}->{hash}{statementlist}))
} else {
die "AST handler circumfix:pblock partially unimplemented";
}
;
    };
}
