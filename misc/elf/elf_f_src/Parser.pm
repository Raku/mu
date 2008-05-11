
class Parser {
  has $.is_for_active_runtime; # Unused.

  method parse($p6_code,$claim_as_filename) {
    unslurp($p6_code,"deleteme.p6");
    my $parser = self.parser_name();
    my $msg = "Parse error in: "~$claim_as_filename~"\n";
    my $cmd = $parser ~ " --error-message='"~$msg~"' -q --format=p5a deleteme.p6 > deleteme.dump";
    system($cmd) == 0 or die("Parse failed.\n");
    my $dump5 = slurp("deleteme.dump");
    my $tree = eval_perl5("package Fastdump;"~$dump5);
    $tree;
  };
  method parser_name() {
    parser_name()
  };

};
eval_perl5('
{ package Fastdump;
  sub match {my($r,$s,$f,$t,$h)=@_; Match->make_from_rsfth($r,$s,$f,$t,$h)}
}');

if not($*parser0) { $*parser0 = Parser.new('is_for_active_runtime',1) }
$*parser1 = Parser.new;
