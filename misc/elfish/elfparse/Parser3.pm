
class Parser3 is Parser {

  method parse($p6_code,$claim_as_filename) {
    my $msg = "Parse error in: "~$claim_as_filename~"\n";
    my $tree = STD.parse6($p6_code);
    if not $tree { die($msg~"Parse failed.\n") }
    $._condition_tree($tree);
    $tree;
  };

  method _condition_tree ($x) {
    my $what = $x.WHAT;
    if $what eq 'Match' {
      my $m = $x;
      my $sym = $m<sym>;
      if defined $sym { $m<sym_name> = $m<sym>.Str }
      my $a = $m.match_array.concat($m.match_hash.values);
      for $a { $._condition_tree($_) }
    }
    elsif $what eq 'Array' {
      for $x { $._condition_tree($_) }
    }
    else {}
  }

};

if not($*parser0) { $*parser0 = Parser3.new('is_for_active_runtime',1) }
$*parser1 = Parser3.new;
