#!/usr/bin/env elfblue

class STDGreenRun {
  method print_usage_and_exit () {
    say '
Usage: [-q] [--format=p5a] [--start=RULE] [ FILENAME | -e CODE ]

';
    exit(1);
  }
  our $whole_file;
  method main () {
    if @*ARGS.elems == 0 {
      $.print_usage_and_exit()
    }
    my $q = 0;
    my $format;
    my $start;
    my $code;
    my $filename;
    my $g;
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
      elsif ($g = $arg.re_groups('--start=(\w+)')) {
        $start = $g[0];
      }
      elsif $arg eq '-e' {
        $code = @*ARGS.shift || $.print_usage_and_exit();
      }
      else {
        $filename = $arg;
        $code = slurp($filename);
        if not(defined($code)) { die "File not found: "~$filename~"\n"; }
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
      $whole_file = $code;
      say $tree.to_dump0;
    }
  }
}

class Match {
  method condition_sym ($sym) {
    my $g;
    if ($sym.WHAT eq 'Array') { join(" ",$sym) }
    elsif not($sym.re_matchp('\Asym\b')) { $sym }
    elsif ($g = $sym.re_groups('\Asym<\s*(.+?)\s*>\z')) { $g[0] }
    elsif ($g = $sym.re_groups('\Asym«\s*(.+?)\s*»\z')) { $g[0] }
    elsif ($g = $sym.re_groups('\Asym\[\'(.+?)\']\z')) { $g[0] }
    else { die "bogus sym: "~$sym~"\n" }
  }
  method to_dump0 {
    my $rule = $.match_rule;
    if not(defined($rule)) {
      $rule = "unknown"
    }

    my $f = $.match_from;
    my $t = $.match_to;
    my $str = substr($STDGreenRun::whole_file,$f,$t-$f);

    my $sym = self{'sym'};
    if ($sym) {
      my $normalized = $.condition_sym($sym);
      self{'sym_name'} = $normalized;
      $rule = $rule ~ ':'~$normalized if ($sym ne $rule &&
                                          $rule ne 'EXPR' &&
                                          $rule ne 'infixish');
    }
    my $rule_str = $.match_rule.to_dump0;
    my $s = $str.to_dump0;
    my $mh = $.match_hash;
    my $h = $mh.keys.map(sub ($k){
      if not($k.re_matchp('^[a-zA-Z]')) { "" }
      elsif $k.re_matchp('\A(O)\z') { "" }
      else {
        my $v = $mh{$k};
        my $vs = 'undef'; if defined($v) { $vs = $v.to_dump0 }
        "\n "~$k~' => '~$vs~','
      }
    }).join("");
    'match('~$rule_str~','~$s~','~$f~','~$t~',{'~$h~'})'
  }
}
class Array {
  method to_dump0 {
    '['~$.map(sub ($e){$e.to_dump0}).join(',')~']'
  }
}
class Hash {
  method to_dump0 {
    '{'~$.keys.map(sub ($k){$k~' => '~self{$k}.to_dump0}).join(',')~'}'
  }
}
class Any {
  method to_dump0 {
    my $s = self.Str;
    $s = $s.re_gsub_pat('([\\\\\'])','\\$1');
    "'"~$s~"'";
  }
}

STDGreenRun.main();
