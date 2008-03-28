

class Program {
  method print_usage_and_die() {
    say "\nelf_b [-e P6_CODE]\n";
    exit(2);
  };
  method main ($args) {
    if $args.elems == 0 {
      self.print_usage_and_die;
    }
    my $p6_code;
    while $args.elems {
      my $arg = $args.shift;
      if $arg eq '-e' {
        $p6_code = $args.shift;
      }
      elsif file_exists($arg) {
        $p6_code = slurp($arg)
      }
      else {
        self.print_usage_and_die;
      }
    }
    #say $p6_code;
    unslurp($p6_code,"deleteme.p6");
    my $parser = "../../STD_red/STD_red_run";
    my $cmd = $parser ~ " -q --format=p5a deleteme.p6 > deleteme.dump";
    system($cmd) == 0 or die("Parse failed.\n");
    my $dump5 = slurp("deleteme.dump");
    #say $dump5;
    my $tree = eval_perl5("package Fastdump;"~$dump5);
    say $tree.match_describe;
    my $ir = $tree.make_ir_from_Match_tree();
    #say eval_perl5('sub{use Data::Dumper; Data::Dumper::Dumper($_[0])}').($ir);
    say $ir.ir0_describe;
    say $ir.callback(SimpleEmit5.new)
  };
};
Program.new().main(@*ARGS);

package Fastdump {
  sub match ($r,$s,$f,$t,$h){Match.make($r,$s,$f,$t,$h)}
}
