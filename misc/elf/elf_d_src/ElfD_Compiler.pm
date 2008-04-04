
class Compiler {

  has $.is_for_active_runtime;

  method eval_perl6($code) {
    self.eval_fragment($code,'-e',0);
  };
  method eval_file($file) {
    self.eval_fragment(slurp($file),$file,0);
  };

  method eval_fragment($code,$filename,$verbose) {
    my $p5 = self.compile_fragment($code,$filename,$verbose);
    eval_perl5($p5);
  };
  method compile_fragment($code,$filename,$verbose) {
    my $tree;
    if $.is_for_active_runtime {
      $tree = $*parser0.parse($code,$filename);
    } else {
      $tree = $*parser1.parse($code,$filename);
    }
    if $verbose { say $tree.match_describe; }
    my $ir = $tree.make_ir_from_Match_tree();
    #say eval_perl5('sub{use Data::Dumper; Data::Dumper::Dumper($_[0])}').($ir);
    if $verbose { say $ir.irx1_describe; }
    my $p5;
    if $.is_for_active_runtime {
      $p5 = $ir.callback($*emitter0.new_emitter('compiler',self));
    } else {
      $p5 = $ir.callback($*emitter1.new_emitter('compiler',self));
    }
    if $verbose { say $p5; }
    $p5;
  };

  has $.todo;
  method compile_executable($sources,$output_file) {
    $.todo = [];
    my $p5 = self.prelude ~ "\n";
    for $sources {
      my $code = $_.[0];
      my $file = $_.[1];
      my $verbose = $_.[2];
      my $more_p5 = self.compile_fragment($code,$file,$verbose);
      while $.todo.elems > 0 {
        my $filename = $.todo.shift;
        my $module_p5 = self.compile_fragment(slurp($filename),$filename,$verbose);
        $p5 = $p5 ~ $module_p5 ~ "\n;\n";
      }
      $p5 = $p5 ~ $more_p5 ~ "\n;\n";
    }
    if $output_file eq '-' {
      say $p5;
    } else {
      unslurp($p5,$output_file);
    };
    ['perl',$output_file];
  };

  method prelude() {
    if $.is_for_active_runtime {
      $*emitter0.prelude
    } else {
      $*emitter1.prelude
    }
  };
  method hook_for_use($module) {
    if $.is_for_active_runtime {
      require($module);
    } else {
      my $filename = find_required_module($module) ||
          die("Didnt find "~$module~" in ( "~@*INC.join(" ")~" ).\n");
      $.todo.push($filename);
    }
    1; # true -> Don't emit use().
  };
};

if not($*compiler0) { $*compiler0 = Compiler.new('is_for_active_runtime',1) }
$*compiler1 = Compiler.new;


