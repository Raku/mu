
class Parser3 is Parser {

  method parse($p6_code,$claim_as_filename) {
    my $msg = "Parse error in: "~$claim_as_filename~"\n";
    my $tree = STD.parse6($p6_code);
    if not $tree { die($msg~"Parse failed.\n") }
    $tree;
  };

};

if not($*parser0) { $*parser0 = Parser3.new('is_for_active_runtime',1) }
$*parser1 = Parser3.new;
