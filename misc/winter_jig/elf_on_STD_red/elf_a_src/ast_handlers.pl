#line 2 ast_handlers.pl
# Warning: This file is mechanically written.  Your changes will be overwritten.
{ package IRBuild;
    $IRBuild::constructors{comp_unit} = sub {
      my($m)=@_;
    IR::CompUnit->new($m,ir($m->{hash}{statementlist}));
    };
    $IRBuild::constructors{statement} = sub {
      my($m)=@_;
      my @keys = map{$_ eq "match" ? () : ($_)} keys %{$m->{hash}};
die("Unexpectedly more than 1 field - dont know which to choose\n".
    $m->match_describe."\n") if(@keys > 1);
my $one = ir($m->{hash}{$keys[0]});
$one;
    };
    $IRBuild::constructors{expect_term} = sub {
      my($m)=@_;
    ir($m->{hash}{noun});
    };
    $IRBuild::constructors{integer} = sub {
      my($m)=@_;
    IR::Val_Int->new($m,($m->match_string));
    };
    $IRBuild::constructors{subcall} = sub {
      my($m)=@_;
    IR::Apply->new($m,ir($m->{hash}{subshortname}),ir($m->{hash}{semilist}));
    };
    $IRBuild::constructors{name} = sub {
      my($m)=@_;
    ir($m->{hash}{ident});
    };
}
