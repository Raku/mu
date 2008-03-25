

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
    my $cmd = $parser ~ " -q --dump5 deleteme.p6 > deleteme.dump";
    system($cmd) == 0 or die("Parse failed.\n");
    my $dump5 = slurp("deleteme.dump");
    #say $dump5;
    my $tree = eval_perl5("package Fastdump;"~$dump5);
    say $tree.match_describe;
  };
};
Program.new().main(@*ARGS);

package Fastdump {
  sub match ($r,$s,$f,$t,$h){Match.make($r,$s,$f,$t,$h)}
}
