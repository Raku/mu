

class Program {
  method print_usage_and_die() {
    say "
Usage: [-v] [-c|-x|-xe] [-o OUTPUT_FILE] [-I dir] [ P6_FILE | -e P6_CODE ]+

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
    my $include_prelude = 1;
    my $verbose;
    my $sources = [];
    while $args.elems {
      my $arg = $args.shift;
      if $arg eq '-v' {
        $verbose = 1;
      }
      elsif $arg eq '-c' {
        $dont_eval = 1;
        $include_prelude = 0;
      }
      elsif $arg eq '-x' {
        $dont_eval = 1;
      }
      elsif $arg eq '-xe' {
        $dont_eval = 1;
        $run_externally = 1;
      }
      elsif $arg eq '-o' {
        $output_file = $args.shift || self.print_usage_and_die;
      }
      elsif $arg eq '-e' {
        my $p6_code = $args.shift || self.print_usage_and_die;
        $sources.push(CompilerSource.new('filename',"-e",'code',$p6_code));
      }
      elsif file_exists($arg) {
        $sources.push(CompilerSource.new('filename',$arg));
      }
      elsif $arg eq '-I' {
        my $dir = $args.shift || self.print_usage_and_die;
        @*INC.push($dir);
      }
      elsif $arg eq '--' {
        last;
      }
      else {
        self.print_usage_and_die;
      }
    }
    if(not($dont_eval)) {
      $*compiler0.eval_sources($sources,$verbose);
    }
    elsif($run_externally) {
      if(not($output_file)) {
        #XXX how to use tempfile?
        $output_file = "deleteme_exe";
      }
      my $p5_code = $*compiler1.compile_sources($sources,$verbose,$include_prelude);
      unslurp($p5_code,$output_file);
      exec("perl",$output_file,$args);
    }
    else {
      my $p5_code = $*compiler1.compile_sources($sources,$verbose,$include_prelude);
      if(not $output_file) {
        say $p5_code;
      } else {
        unslurp($p5_code,$output_file);
      }
    }
  };
};

class CompilerSource {
  has $.filename;
  has $.code;
  has $.module;
};

class Compiler {
  method eval_perl6($code) {
    my $compiler = Compiler.new('is_compiler_for_runtime',1);
    $compiler.eval_sources([CompilerSource.new('code',$code)],0);
  };
  method eval_file($file) {
    self.eval_perl6(slurp($file));
  };

  has $.is_compiler_for_runtime;
  has $.verbose;
  has $.is_eval;
  method eval_sources($sources,$verbose) {
    $.verbose = $verbose;
    $.is_eval = 1;
    my $p5_code = self.do_tasks($sources);
    eval_perl5($p5_code);
  };
  method compile_sources($sources,$verbose,$include_prelude) {
    $.verbose = $verbose;
    $.is_eval = 0;
    my $p5_code = self.do_tasks($sources);
    if $include_prelude {
      $p5_code = self.prelude~"\n"~$p5_code;
    }
    $p5_code;
  };

  has $.todo;
  method do_tasks($tasks) {
    my $p5_code = "";
    $.todo = $tasks.copy;
    while $.todo.elems {
      my $task = $.todo.shift;
      my $p6_code = $task.code;
      if not(defined($p6_code)) { $p6_code = slurp($task.filename); }
      my $p5 = self.compile($p6_code);
      $p5_code = $p5_code ~ $p5 ~ "\n;\n";
    }
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
    my $p5 = $ir.callback(SimpleEmit5.new('compiler',self));
    if $.verbose { say $p5; }
    $p5;
  };
  method prelude() {
    SimpleEmit5.prelude()
  };
  method hook_for_use($module) {
    if $.is_eval {
      require($module);
    } else {
      my $filename = find_required_module($module) ||
          die("Didnt find "~$module~" in ( "~@*INC.join(" ")~" ).\n");
      $.todo.push(CompilerSource.new('filename',$filename));
    }
    1; # true -> Don't emit use().
  };
};
package Fastdump {
  sub match ($r,$s,$f,$t,$h){Match.make($r,$s,$f,$t,$h)}
};

if not($*compiler0) { $*compiler0 = Compiler.new('is_compiler_for_runtime',1) }
$*compiler1 = Compiler.new;

Program.new().main(@*ARGS);
