#line 2 main.pl
{ package Program;
  use YAML::Syck;

  sub new {
    my($cls)=@_;
    bless {},$cls;
  }
  sub print_usage_and_exit {
    my $usage = "
Usage: [-c] [-o OUTPUT_FILE] [ P6_FILE | -e P6_CODE ]

";
    print STDERR $usage;
    exit(2);
  }
  sub main {
    my($self,$argv)=@_;
    $self->print_usage_and_exit() if !@$argv;
    my($p6_code,$output_file,$compile);
    while(my $arg = shift(@$argv)) {
      if($arg eq '-c') {
        $compile = 1;
      }
      elsif($arg eq '-o') {
        $output_file = shift(@$argv) || $self->print_usage_and_exit();
      }
      elsif($arg eq '-e') {
        $p6_code = shift(@$argv) || $self->print_usage_and_exit();
      }
      elsif(-f $arg) {
        $p6_code = `cat $arg`;
      }
      else {
        $self->print_usage_and_exit();
      }
    }
    my $yaml = $self->parse(undef,$p6_code);
    print $yaml;
    my $tree = YAML::Syck::Load($yaml);
    if(!$tree) {
      exit(1);
    }
    print $tree->match_describe(1),"\n";
    my $ir = IRBuild->make_ir_from_Match_tree($tree);
    print YAML::Syck::Dump($ir);
    my $p5 = IR->emit_p5_for($ir);
    print "\n",$p5,"\n\n";
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
    my $cmd = "$std_red -q --yaml $file";
    my $yaml = `$cmd` or die "Parse failed. $!\n";
    $yaml;
  }
}
Program->new()->main(\@ARGV);
