

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
    my $verbose;
    my $include_prelude;
    my $tasks = [];
    while $args.elems {
      my $arg = $args.shift;
      if $arg eq '-v' {
        $verbose = 1;
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
        my $p6_code = $args.shift || self.print_usage_and_die;
        $tasks.push(CompilerTask.new('filename',"-e",'code',$p6_code));
      }
      elsif file_exists($arg) {
        $tasks.push(CompilerTask.new('filename',$arg));
      }
      elsif $arg eq '--' {
        last;
      }
      else {
        self.print_usage_and_die;
      }
    }
    my $compiler = Compiler.new;
    $compiler.verbose($verbose); # XXX emitter workaround re Moose.
    $compiler.include_prelude($include_prelude); # XXX emitter workaround re Moose.
    my $p5_code = $compiler.do_tasks($tasks);
    if(not($dont_eval)) {
      eval_perl5($p5_code);
      #if($@) { #XXX how to easily check result?
      #  #XXX... provide $code.
      #  die $@;
      #}
    }
    elsif($run_externally) {
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
        unslurp($p5_code,$output_file);
      }
    }
  };
};

class CompilerTask {
  has $.filename;
  has $.code;
  has $.module;
};

class Compiler {
  has $.units;
  has $.verbose;
  has $.include_prelude;
  has $.extra_tasks;
  method do_tasks($tasks) {
    my $p5_code = "";
    $.extra_tasks = [];
    for $tasks {
      my $p6_code = $_.code;
      if not(defined($p6_code)) { $p6_code = slurp($_.filename); }
      my $p5 = self.compile($p6_code);
      for $.extra_tasks {
      };
      $.extra_tasks = [];
      $p5_code = $p5_code ~ $p5 ~ "\n;\n";
    }
    if $.include_prelude {
      $p5_code = self.prelude~"\n"~$p5_code;
    }
    $p5_code = "#!/usr/bin/perl -w\n"~$p5_code~"\n";
    $p5_code;
  };
  method compile($p6_code) {
    #say $p6_code;
    unslurp($p6_code,"deleteme.p6");
    my $parser = parser_name();
    my $cmd = $parser ~ " -q --format=p5a deleteme.p6 > deleteme.dump";
    system($cmd) == 0 or die("Parse failed.\n");
    my $dump5 = slurp("deleteme.dump");
    #say $dump5;
    my $tree = eval_perl5("package Fastdump;"~$dump5);
    if $.verbose { say $tree.match_describe; }
    my $ir = $tree.make_ir_from_Match_tree();
    #say eval_perl5('sub{use Data::Dumper; Data::Dumper::Dumper($_[0])}').($ir);
    if $.verbose { say $ir.ir0_describe; }
    my $p5 = $ir.callback(SimpleEmit5.new(self));
    if $.verbose { say $p5; }
    $p5;
  };
  method prelude() {
    SimpleEmit5.prelude()
  };
  method dont_use($module) {
    undef;
  };
};
package Fastdump {
  sub match ($r,$s,$f,$t,$h){Match.make($r,$s,$f,$t,$h)}
};

Program.new().main(@*ARGS);
