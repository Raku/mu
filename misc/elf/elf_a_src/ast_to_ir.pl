#line 2 ir_build.pl
{ package IRBuild;
  sub make_ir_from_Match_tree {
    my($this,$tree)=@_;
    if(not ref($tree)) {
      $tree;
    } else {
      my $ref = ref($tree);
      if($ref eq 'Match') {
        my $m = $tree;
        my $rule = $m->{rule};
        if(not $rule) {die "Bug.  Found an unlabeled Match:\n$m->match_describe";}
        my $constructor = $IRBuild::constructors{$rule};
        if($constructor) {
          #print STDERR "Calling constructor for $rule with @{[$m->match_describe]}.\n";
          $constructor->($m);
        } else {
          die "Unknown rule: $rule\nIt needs to be added to ast_handlers.\n";
        }
      }
      elsif($ref eq 'ARRAY') {
        [map{$this->make_ir_from_Match_tree($_)} @$tree]
      }
      elsif($ref eq 'HASH') {
        my %h;
        for my $k (keys %$tree) {
          my $v = $tree->{$k};
          $h{$k} = $this->make_ir_from_Match_tree($v);
        }
        \%h;
      }
      else {
        die "bug? $ref $tree";
      }
    }
  }
  sub ir {
    my($tree)=@_;
    IRBuild->make_ir_from_Match_tree($tree);
  }
}
