#!/usr/bin/perl
use strict; use warnings;
my $gray = "";
sub g ($) { $gray .= join("\n",@_)."\n"; }
sub gs ($) { $gray .= join("",@_).""; }

g <<'END';
class PerlGray {
  has $.ws_from;
  has $.ws_to;

END

    def_precedence( hyper           => { transparent=>1                         });
    def_precedence( term            => { prec=>"z="                             });
    def_precedence( methodcall      => { prec=>"y="                             });
    def_precedence( autoincrement   => { prec=>"x="                             });
    def_precedence( exponentiation  => { prec=>"w=", assoc=>'right', assign=>1 });
    def_precedence( symbolic_unary  => { prec=>"v="                             });
    def_precedence( multiplicative  => { prec=>"u=", assoc=>'left',  assign=>1 });
    def_precedence( additive        => { prec=>"t=", assoc=>'left',  assign=>1 });
    def_precedence( replication     => { prec=>"s=", assoc=>'left',  assign=>1 });
    def_precedence( concatenation   => { prec=>"r=", assoc=>'left',  assign=>1 });
    def_precedence( junctive_and    => { prec=>"q=", assoc=>'list',  assign=>1 });
    def_precedence( junctive_or     => { prec=>"p=", assoc=>'list',  assign=>1 });
    def_precedence( named_unary     => { prec=>"o=",                            });
    def_precedence( nonchaining     => { prec=>"n=", assoc=>'non'               });
    def_precedence( chaining        => { prec=>"m=", assoc=>'chain', bool=>1   });
    def_precedence( tight_and       => { prec=>"l=", assoc=>'left',  assign=>1 });
    def_precedence( tight_or        => { prec=>"k=", assoc=>'left',  assign=>1 });
    def_precedence( conditional     => { prec=>"j=", assoc=>'right',            });
    def_precedence( item_assignment => { prec=>"i=", assoc=>'right'             });
    def_precedence( loose_unary     => { prec=>"h=",                            });
    def_precedence( comma           => { prec=>"g=", assoc=>'list',             });
    def_precedence( list_infix      => { prec=>"f=", assoc=>'list',  assign=>1 });
    def_precedence( list_assignment => { prec=>"i=", 'sub'=>"e=", assoc=>'right' });
    def_precedence( list_prefix     => { prec=>"e=",                            });
    def_precedence( loose_and       => { prec=>"d=", assoc=>'left',  assign=>1 });
    def_precedence( loose_or        => { prec=>"c=", assoc=>'left',  assign=>1 });
    def_precedence( LOOSEST         => { prec=>"a=!",                           });
    def_precedence( terminator      => { prec=>"a=", assoc=>'list'              });
sub def_precedence {
  my($name,$info)=@_;
}


#    SLOOSEST = HLOOSEST[prec]

g <<'END';
END

g <<'END';
  method init($code) {
  }
}
END

sub STD_gray_run {
  <<'END';

class STDGrayRun {
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
    $code = $.remove_t_pod_hacks($code);

    my $tree;
    my $pn = PerlGray.new;
    $pn.init($code);
    my $^unitstopper = '_EOS';
    my $^stop = 'if you see this dont stop';
    if $start {
      $tree = eval($pn~'.'~$start)
    } else {
      $tree = $pn._UNIT;
    }
    if not($format) {
      say $tree.match_describe;
    } else {
      say $tree.to_dump0;
    }
  }
  method remove_t_pod_hacks ($code) is p5 {'
    my $whiteout = sub {my($s)=@_; $s =~ tr/ \n/ /c; $s };
    $code =~ s/(\n=kwid.*?\n=cut[^\n]*)/$whiteout->($1)/seg;
    $code =~ s/(\n=pod.*?\n=cut[^\n]*)/$whiteout->($1)/seg;
    $code =~ s/(\n=head1.*?\n=cut[^\n]*)/$whiteout->($1)/seg;
    $code;
  '}
}
STDGrayRun.main()

END
}

sub main {
  open(F,">STD_gray_run") or die;
  print F "#!/usr/bin/env elf_f\n";
  print F  $gray.STD_gray_run();
  close F;
  system("./STD_gray_run",@ARGV);
}
main();
