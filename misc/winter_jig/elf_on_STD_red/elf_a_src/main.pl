#line 2 main.pl
{ package Program;
  use YAML::Syck;

  sub new {
    my($cls)=@_;
    bless {},$cls;
  }
  sub print_usage_and_exit {
    my $usage = "
Usage: [-v] [-c|-x|-xe] [-o OUTPUT_FILE] [ P6_FILE | -e P6_CODE ]

default Run code.
 -c     Compile code.
 -x     Compile code, and include prelude, creating an executable.
 -xe    Compile code, and include prelude, and run it.

";
    print STDERR $usage;
    exit(2);
  }
  sub main {
    my($self,$argv)=@_;
    $self->print_usage_and_exit() if !@$argv;
    my($output_file,$dont_eval,$run_externally,$include_prelude,$verbose);
    my $p5_code = "";
    while(my $arg = shift(@$argv)) {
      if($arg eq '-v') {
        $verbose = 1;
      }
      elsif($arg eq '-c') {
        $dont_eval = 1;
      }
      elsif($arg eq '-x') {
        $dont_eval = 1;
        $include_prelude = 1;
      }
      elsif($arg eq '-xe') {
        $dont_eval = 1;
        $include_prelude = 1;
        $run_externally = 1;
      }
      elsif($arg eq '-o') {
        $output_file = shift(@$argv) || $self->print_usage_and_exit();
      }
      elsif($arg eq '-e') {
        my $p6_code = shift(@$argv) || $self->print_usage_and_exit();
        $p5_code .= $self->compile($p6_code,$verbose);
        $p5_code .= "\n;\n";
      }
      elsif(-f $arg) {
        my $p6_code = `cat $arg`;
        $p5_code .= $self->compile($p6_code,$verbose);
        $p5_code .= "\n;\n";
      }
      elsif($arg eq '--') {
        last;
      }
      else {
        $self->print_usage_and_exit();
      }
    }
    my $prelude = "";
    $prelude = $self->prelude if $include_prelude || !$dont_eval;
    $p5_code = $prelude."\n".$p5_code;
    $p5_code = "#!/usr/bin/perl -w\n".$p5_code;
    if(!$dont_eval) {
      eval($p5_code);
      if($@) {
        #XXX... provide $code.
        die $@;
      }
    }
    elsif($run_externally) {
      if(not $output_file) {
        use File::Temp qw/ tempfile /;
        my($fh,$fname) = tempfile();
        close $fh;
        $output_file = $fname;
      }
      open(F,">$output_file") or die $!;
      print F $p5_code; close F;
      exec("perl",$output_file,@ARGV);
    }
    else {
      if(not $output_file) {
        print $p5_code,"\n";
      } else {
        open(F,">$output_file") or die $!;
        print F $p5_code,"\n"; close F;
      }
    }
  }
  sub compile {
    my($self,$p6_code,$verbose)=@_;
    #my $yaml = $self->parse(undef,$p6_code);
    #print $yaml if $verbose;
    #my $tree = YAML::Syck::Load($yaml);
    my $dump5 = $self->parse(undef,$p6_code);
    print $dump5 if $verbose;
    my $tree = eval('package Fastdump;'.$dump5);
    if(!$tree) {
      die "Flawed dump: $@";
    }
    #print Data::Dumper::Dumper($tree);
    print $tree->match_describe(1),"\n" if $verbose;
    my $ir = IRBuild->make_ir_from_Match_tree($tree);
    print "\n",$ir->describe,"\n" if $verbose;
    my $p5 = IR->emit_p5_for($ir);
    print "\n",$p5,"\n\n" if $verbose;
    $p5;
  }
  sub parse {
    my($self,$p6_file,$p6_code)=@_;
    $p6_code ||= `cat $p6_file`;
    my $std_red_from_root = "misc/STD_red/STD_red_run";
    my $std_red_from_src = "../../../$std_red_from_root";
    my $std_red;
    if(-f $std_red_from_src) {
      $std_red = $std_red_from_src;
    } else {
      die "The environment variable PUGS_ROOT must be defined.\n"
          if !exists($ENV{PUGS_ROOT});
      $std_red = $ENV{PUGS_ROOT}."/".$std_red_from_root;
    }
    my $file = $p6_file;
    if(!$file) {
      use File::Temp qw/ tempfile /;
      my($fh,$fname) = tempfile();
      print $fh $p6_code;
      close($fh);
      $file = $fname;
    }
    my $cmd = "$std_red -q --dump5 $file";
    my $out = `$cmd` or die "Parse failed. $!\n";
    $out;
  }
}
Program->new()->main(\@ARGV);

{ package Fastdump;
  sub match {my($r,$s,$f,$t,$h)=@_;Match->new_set($r,$s,$f,$t,$h)}
}
