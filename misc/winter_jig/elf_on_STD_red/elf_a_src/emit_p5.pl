#line 2 emit_p5.pl
# Warning: This file is mechanically written.  Your changes will be overwritten.
{ package IR;
  sub emit_p5_for {
    my($this,$tree)=@_;
    if(not ref($tree)) {
      $tree;
    } else {
      my $ref = ref($tree);
      if($ref eq 'ARRAY') {
        [map{$this->emit_p5_for($_)} @$tree]
      }
      elsif($ref eq 'HASH') {
        my %h;
        for my $k (keys %$tree) {
          my $v = $tree->{$k};
          $h{$k} = $this->emit_p5_for($v);
        }
        \%h;
      }
      else {
        $tree->emit_p5
      }
    }
  }
}
{ package IR::All;
  sub emit_p5 {
    my $name = $_[0]->node_name;
    print STDERR "ERROR: emit_p5 is not defined for $name.\n";
    "***<$name>***";
  }
}
{ package IR::CompUnit; sub emit_p5 {
    my($n)=@_;
    'package Main;
use Perl6::Say;
'.join(";\n",@{IR->emit_p5_for($n->{statements})})
  }
}
{ package IR::Val_Int; sub emit_p5 {
    my($n)=@_;
    IR->emit_p5_for($n->{text})
  }
}
{ package IR::Apply; sub emit_p5 {
    my($n)=@_;
    IR->emit_p5_for($n->{code}).'('.join(",",@{IR->emit_p5_for($n->{arguments})}).')'

  }
}
