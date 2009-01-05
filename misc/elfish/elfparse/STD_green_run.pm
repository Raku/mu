#!/usr/bin/env elfblue
class STD {
  regex unimplemented_rule { <fail> }
  _inline_p5('
  my %unimp_rules_seen;
  sub AUTOLOAD {
    my $r = $AUTOLOAD;
    print STDERR "FAKING unimplemented rule: $r\n"
      if !$unimp_rules_seen{$r}++;
    return __PACKAGE__->unimplemented_rule("");
  }
');
}

class STDGreenRun {
  method print_usage_and_exit () {
    say '
Usage: [-q] [--format=p5a] [--start RULE] [ FILENAME | -e CODE ]

';
    exit(1);
  }
  method main () {
    if @*ARGS.elems == 0 {
      $.print_usage_and_exit()
    }
    my $q = 0;
    my $format;
    my $start;
    my $code;
    my $filename;
    while @*ARGS.elems > 0 {
      my $arg = @*ARGS.shift;
      if $arg eq '--help' {
        $.print_usage_and_exit()
      }
      elsif $arg eq '-q' {
        $q = 1;
      }
      elsif $arg eq '--format=p5a' {
        $format = 'p5a';
      }
      elsif $arg eq '--start' {
        $start = @*ARGS.shift || $.print_usage_and_exit();
      }
      elsif $arg eq '-e' {
        $code = @*ARGS.shift || $.print_usage_and_exit();
      }
      else {
        $filename = @*ARGS.shift;
        $code = slurp($filename);
      }
    }

    my $tree;
    if not($start) { $start = 'TOP' }
    my $src = 'grammar STD { /^<'~$start~'>/ }';
    my $rx = eval($src);
    $tree = $rx.match($code);
    if $tree && $tree.match_hash{$start} { $tree = $tree.match_hash{$start} }
    if not($format) {
      say $tree.match_describe;
    } else {
      say $tree.to_dump0;
    }
  }
}
STDGreenRun.main()

