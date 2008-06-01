
class Compiler {

  has $.is_for_active_runtime;
  has $.parser;
  has $.emitter;

  method eval_perl6($code,$env) {
    self.eval_fragment($code,'-e',0,$env);
  };
  method eval_file($file) {
    self.eval_fragment(slurp($file),$file,0);
  };

  method eval_fragment($code,$filename,$verbose,$env) {
    my $p5 = self.compile_fragment($code,$filename,$verbose);
    eval_perl5($p5,$env);
  };
  method compile_fragment_cache_get($code,$filename) { undef };
  method compile_fragment_cache_set($code,$filename,$value) { };

  sub dump_IRx1($ast) {
    if $ast.isa("IRx1::Base") {
      my $field_names = $ast.field_names;
      my $field_values = $ast.field_values;
      my $i=0;
      my $str = $ast.node_name ~ '(';
      while $i < $field_names.elems {
        if $i != 0 {
          $str = $str ~ ',';
        }
        $str = $str ~ $field_names[$i] ~ '=>' ~ dump_IRx1($field_values[$i]);
        $i++;
      }
      $str = $str ~ ')';
      return $str;
    } elsif $ast.WHAT eq "Array" {
      return '[' ~ $ast.map(sub ($e) {dump_IRx1($e)}).join(',') ~ ']';
    } elsif $ast.WHAT eq "Str" {
      #TODO: '
      return "'" ~ $ast ~ "'"
    } elsif $ast.WHAT eq "Undef" {
      return 'undef'
    } else {
      return $ast
    }
  }
  method compile_fragment($code,$filename,$verbose) {
    my $tree;
    my $cached = self.compile_fragment_cache_get($code,$filename);
    if $cached {
      $cached
    }
    else {
      $tree = $.parser.parse($code,$filename);
      if $verbose { say $tree.match_describe; }
      my $ir = $tree.make_ir_from_Match_tree();
      if $verbose { say $.emitter.tidy(dump_IRx1($ir)) }

      my $p5;
      $p5 = ($.emitter.prelude_lexical ~
        $ir.callback($.emitter.new_emitter('compiler',self,'filename',$filename)));

      if $verbose {
        say $.emitter.tidy($p5);
      }
      self.compile_fragment_cache_set($code,$filename,$p5);
      $p5;
    }
  };

  has $.todo = [];
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
      $.emitter.prelude
  };
  method hook_for_use_lib($expr) {
    @*INC.unshift($expr);
    if $.is_for_active_runtime { 1 } else { 0 }
  }
  method hook_for_use($module,$expr) {
    if $.is_for_active_runtime {
      require($module);
      import($module,$expr);
    } else {
      my $filename = find_required_module($module) ||
          die("Didnt find "~$module~" in ( "~@*INC.join(" ")~" ).\n");
      $.todo.push($filename);
    }
    1; # true -> Don't emit use().
  };
};


