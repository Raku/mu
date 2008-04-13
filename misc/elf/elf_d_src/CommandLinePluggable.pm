
class Program {
  method print_usage_and_die() {
say "
Usage: elf [switches] [--] [programfile] [arguments]
  -e program      one line of program (multiple -e are allowed, omit programfile)
  -c              parse the file or -e, but do not run it
  -Bbackend       execute using the compiler backend
  -Cbackend       compile using the compiler backend
                  (valid backends are: nder -B)
  -o              output to file if using -B, required by some backends
  --do            visitors to use

  -h or --help    give this message
";
    exit(2);
  };
  my %C;
  %C<perl5> = sub($code,$filename,$verbose) {
    say $*compiler1.compile_fragment($code,$filename,$verbose);
  };
  %C<ast> = sub {
    say "emitting ast";
  };
  method main ($args) {
    if $args.elems == 0 {
        self.print_usage_and_die;
    };
    my $code = [];
    my $mode = 'perl5';
    while $args.elems {
        my $arg = $args.shift;
        if $arg eq '-e' {
            $code.push($args.shift || self.print_usage_and_die);
        };
        if $arg eq '-Cperl5' {
            $mode = 'perl5';
        }
        if $arg eq '-Cast' {
            $mode = 'ast';
        }
    };
    if $args.elems != 0 and $code.elems == 0 {
        self.print_usage_and_die;
    };
    %C{$mode}.($code.join(';'),'-e',0);
  }
};

Program.new().main(@*ARGS);
