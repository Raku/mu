

class Program {
  method print_usage_and_die() {
    say "
Usage: [-v] [-c|-x|-xe] [-o OUTPUT_FILE] [ P6_FILE | -e P6_CODE ]

default Run code.
 -c     Compile code.
 -x     Compile code, and include prelude, creating an executable.
 -xe    Compile code, and include prelude, and run it.

";
    exit(2);
  };
  method main ($args) {
    if $args.elems == 0 {
      self.print_usage_and_die;
    }
    my $output_file;
    my $dont_eval;
    my $run_externally;
    my $include_prelude;
    my $verbose;
    my $p6_code;
    my $p5_code;
    while $args.elems {
      my $arg = $args.shift;
      if $arg eq '-v' {
        $verbose = 1
      }
      elsif $arg eq '-c' {
        $dont_eval = 1;
      }
      elsif $arg eq '-x' {
        $dont_eval = 1;
        $include_prelude = 1;
      }
      elsif $arg eq '-xe' {
        $dont_eval = 1;
        $include_prelude = 1;
        $run_externally = 1;
      }
      elsif $arg eq '-o' {
        $output_file = $args.shift || self.print_usage_and_die;
      }
      elsif $arg eq '-e' {
        $p6_code = $args.shift || self.print_usage_and_die;
        $p5_code = $p5_code ~ self.compile($p6_code,$verbose);
        $p5_code = $p5_code ~ "\n;\n";
      }
      elsif file_exists($arg) {
        $p6_code = slurp($arg);
        $p5_code = $p5_code ~ self.compile($p6_code,$verbose);
        $p5_code = $p5_code ~ "\n;\n";
      }
      elsif $arg eq '--' {
        last;
      }
      else {
        self.print_usage_and_die;
      }
    }
    my $prelude = "";
    if $include_prelude {
      $prelude = self.prelude;
    }
    $p5_code = $prelude~"\n"~$p5_code;
    $p5_code = "#!/usr/bin/perl -w\n"~$p5_code;
    if(not($dont_eval)) {
      eval_perl5($p5_code);
      #if($@) { #XXX how to easily check result?
      #  #XXX... provide $code.
      #  die $@;
      #}
    }
    elsif($run_externally) {
      $p5_code = $p5_code~"\n";
      if(not($output_file)) {
        #XXX how to use tempfile?
        $output_file = "deleteme_exe";
      }
      unslurp($p5_code,$output_file);
      exec("perl",$output_file,$args);
    }
    else {
      if(not $output_file) {
        say $p5_code;
      } else {
        $p5_code = $p5_code~"\n";
        unslurp($p5_code,$output_file);
      }
    }
  };
  method compile($p6_code,$verbose) {
    #say $p6_code;
    unslurp($p6_code,"deleteme.p6");
    my $parser = parser_name();
    my $cmd = $parser ~ " -q --format=p5a deleteme.p6 > deleteme.dump";
    system($cmd) == 0 or die("Parse failed.\n");
    my $dump5 = slurp("deleteme.dump");
    #say $dump5;
    my $tree = eval_perl5("package Fastdump;"~$dump5);
    if $verbose { say $tree.match_describe; }
    my $ir = $tree.make_ir_from_Match_tree();
    #say eval_perl5('sub{use Data::Dumper; Data::Dumper::Dumper($_[0])}').($ir);
    if $verbose { say $ir.ir0_describe; }
    my $p5 = $ir.callback(SimpleEmit5.new);
    if $verbose { say $p5; }
    $p5;
  };
};
Program.new().main(@*ARGS);

package Fastdump {
  sub match ($r,$s,$f,$t,$h){Match.make($r,$s,$f,$t,$h)}
}
