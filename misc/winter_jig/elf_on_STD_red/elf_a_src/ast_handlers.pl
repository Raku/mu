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
    ir($m->{hash}{noun});
    };
    $IRBuild::constructors{'term:expect_term'} = sub {
      my($m)=@_;
    ir($m->{hash}{noun});
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
    IR::Val_Buf->new($m,ir($m->{hash}{text}));
    };
    $IRBuild::constructors{'infix'} = sub {
      my($m)=@_;
    IR::Apply->new($m,"infix:".($m->match_string),[ir($m->{hash}{left}),ir($m->{hash}{right})]);
    };
    $IRBuild::constructors{'scope_declarator:my'} = sub {
      my($m)=@_;
    my $vd = ir($m->{hash}{scoped});
IR::Decl->new($m,'my',undef,$vd->[0],$vd->[1]);
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
    IR::If->new($m,[ir($m->{hash}{if_expr}),ir($m->{hash}{if_block}),@{ir($m->{hash}{elsif})}],ir($m->{hash}{else}));
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
    IR::Block->new($m,ir($m->{hash}{statementlist}))
;
    };
}
