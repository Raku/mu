
class Program {
  method print_usage_and_die() {
    say "
Usage: [-v] [-s0|-s|-x|-xr] [-o OUTPUT_FILE] [-I dir]
         [ P6_FILE | -e P6_CODE ]+ [ -- ARGS* ]

Unlike p5, multiple P6_FILE's and -e P6_CODE's and can be mixed.
Use -- to stop.

 -v                verbose.

 default  Compile0 and eval.
 -s0      Compile0 and show the resulting framgent.
 -s       Compile1 and show the resulting fragment.
 -x       Compile1 to an executable form.
 -xr      Compile1 to an executable form, and run.

One can also do
 [ P6_FILE | -e P6_CODE ]+  these are evaled,
 -x
 [ P6_FILE | -e P6_CODE ]+  these are compiled.

";
    exit(2);
  };
  method main ($args) {
    if $args.elems == 0 {
      self.print_usage_and_die;
    }

    my $verbose;
    my $mode = 'r';
    my $output_file;
    my $incs = [];
    my $output = sub ($text) {
      if $output_file {
        unslurp($text,$output_file);
      } else {
        say $text;
      }
    };
    my $sources = [];
    my $handle = sub ($filename,$code) {
      if $mode eq 'r' {
        $*compiler0.eval_fragment($code,$filename,$verbose,undef);
      }
      elsif $mode eq 's0' || $mode eq 's1' {
        my $comp;
        if $mode eq 's0' {
          $comp = $*compiler0;
        } else {
          $comp = $*compiler1;
        }
        $output.($comp.compile_fragment($code,$filename,$verbose));
      }
      else {
        $sources.push([$code,$filename,$verbose]);
      }
    };
    my $at_end = sub () {
      if $mode eq 'x' && $sources.elems != 0 {
        if not($output_file) {
          $output_file = '-';
        }
        my $exec_args = $*compiler1.compile_executable($sources,$output_file);
      }
      elsif $mode eq 'xr' && $sources.elems != 0 {
        if not($output_file) {
          $output_file = "./deleteme_exe";
        }
        my $exec_args = $*compiler1.compile_executable($sources,$output_file);
        say "# "~$exec_args.join(" ")~" "~$args.join(" ");
        exec($exec_args.flatten,$args.flatten);
      }
    };
    while $args.elems {
      my $arg = $args.shift;
      if $arg eq '-v' {
        $verbose = 1;
      }
      elsif $arg eq '-s0' {
        $mode = 's0';
      }
      elsif $arg eq '-s' {
        $mode = 's1';
      }
      elsif $arg eq '-x' {
        $mode = 'x';
      }
      elsif $arg eq '-xr' {
        $mode = 'xr';
      }
      elsif $arg eq '-o' {
        $output_file = $args.shift || self.print_usage_and_die;
      }
      elsif $arg eq '-e' {
        @*INC.unshift($incs.flatten); $incs = [];
        my $p6_code = $args.shift || self.print_usage_and_die;
        $handle.('-e',$p6_code);
      }
      elsif file_exists($arg) {
        @*INC.unshift($incs.flatten); $incs = [];
        $handle.($arg,slurp($arg));
      }
      elsif $arg eq '-I' {
        my $dir = $args.shift || self.print_usage_and_die;
        $incs.push($dir);
      }
      elsif $arg eq '--' {
        last;
      }
      else {
        self.print_usage_and_die;
      }
    }
    $at_end.();
  }
};

