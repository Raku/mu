#line 2 ir_build.pl
class IRBuild {
  has $.constructors;
  $main::irbuilder = IRBuild.new;
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
sub irbuild_ir ($x) {
  $x.make_ir_from_Match_tree()
}
