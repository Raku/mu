use Prelude;
use Match;
use IRx1_Nodes;
use IRx1_FromAST;
use IRx1_Analysis;
use EmitSimpleP5;
use EmitFasterP5;
use PrimitivesP5;
use Parser;
use Compiler;

class Program {
  method print_usage_and_die() {
    say "
Usage: [-v] [-o OUTPUT_FILE] [-I dir]
         [ P6_FILE | -e P6_CODE ]+ [ -- ARGS* ]

Unlike p5, multiple P6_FILE's and -e P6_CODE's and can be mixed.
Use -- to stop.

 -v                verbose.
";
    exit(2);
  };
  method main ($args) {
    $*compiler0 = Compiler.new('emitter',EmitFasterP5.new(),'parser',Parser.new('is_for_active_runtime',1),'is_for_active_runtime',1);
    $*compiler1 = Compiler.new('emitter',EmitFasterP5.new(),'parser',Parser.new('is_for_active_runtime',0),'is_for_active_runtime',0);

    if $args.elems == 0 {
      self.print_usage_and_die;
    }

    my $verbose;
    my $output_file;
    my $sources = [];

    while $args.elems {
      my $arg = $args.shift;
      if $arg eq '-v' {
        $verbose = 1;
      }
      elsif $arg eq '-o' {
        $output_file = $args.shift || self.print_usage_and_die;
      }
      elsif $arg eq '-e' {
        my $p6_code = $args.shift || self.print_usage_and_die;
        $sources.push([$p6_code,'-e',$verbose]);
      }
      elsif file_exists($arg) {
        $sources.push([slurp($arg),$arg,$verbose]);
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
    if ($output_file) {
        $*compiler1.compile_executable($sources,$output_file);
    } else {
        $*compiler1.compile_executable($sources,"tmp_perl_code");
        system("perl tmp_perl_code");
    }
  }
};
Program.new().main(@*ARGS);
