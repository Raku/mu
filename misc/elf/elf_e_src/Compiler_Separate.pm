
class CompilerSeperate is Compiler {
  sub make_needed_dirs($filename) {
    my $components = $filename.split('/');
    $components.pop();
    my $path = '';
    for $components {
      $path = $path ~ $_~'/';
      mkdir($path);
    }
  };
  method hook_for_use($module) {
    if $.is_for_active_runtime {
      require($module);
    } else {
      my $filename = find_required_module($module) ||
          die("Didnt find "~$module~" in ( "~@*INC.join(" ")~" ).\n");
      my $module_p5 = self.compile_fragment(slurp($filename),$filename,0);
      make_needed_dirs('builddir/'~$filename);
      unslurp($module_p5,'builddir/'~$filename);
      say "module: "~$module~" filename: "~$filename;
    }
    0; # false -> emit use().
  };
};

$*compiler1 = CompilerSeperate.new;


