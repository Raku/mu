#!/usr/bin/perl
# A script to help create and run elf_a .
use strict;
use warnings;
require 'elf_a_src/ir_nodes_ref.pl';
require 'elf_a_src/util.pl';

sub main {
  IRNodesRef->load_ir_node_config("./elf_a_src/ir_nodes.config");
  write_ir_nodes();
  write_ast_handlers();
  write_emit_p5();

  my $files = join(" ",qw(
    head.pl Match.pm
    ir_nodes.pl ast_to_ir.pl ast_handlers.pl
    emit_p5.pl main.pl ));
  my $cmd = "(cd elf_a_src/; cat $files) > ./elf_a";
  system($cmd) == 0 or die $!;

  exec("./elf_a",@ARGV);
  die "Exec failed $!";
}
main();

sub write_ir_nodes {
  my $file = "./elf_a_src/ir_nodes.pl";
  my $code = "#line 2 ir_nodes.pl\n".unindent(<<'  END');
    # Warning: This file is mechanically written.  Your changes will be overwritten.
    { package IR::All;
      use Data::Dumper;
      sub describe_anything {
        my($this,$x)=@_;
        my $ref = ref($x);
        if($ref) {
          if(UNIVERSAL::can($x,'describe')) {
            $x->describe

          } elsif($ref eq 'ARRAY') {
            '['.join(",",map{$this->describe_anything($_)} @$x).']'
          } else {
            die "bug";
          }
        } else {
          local $Data::Dumper::Terse = 1;
          my $s = Dumper($x); $s =~ s/\n$//; $s;
        }
      }
    }
  END

  for my $node (IRNodesRef->nodes) {
    my($name,@fields)=($node->name,$node->fields);
    my @all = $node->all_fields;
    
    my $params = join(',',map{"\$$_"}@all);
    my $all_indexes = join(',',map{"'$_'"}@all);
    my $sig = @all == 1 ? '$' : '@';
    my $init = "${sig}h{$all_indexes}=($params);";
    my $field_values = "${sig}\$self{".join(',',map{"'$_'"}@fields)."}";

    $code .= unindent(<<"    END");
      { package IR::$name;
        \@IR::${name}::ISA = qw( IR::All );
        sub new {
          my(\$cls,$params)=\@_;
          my \%h;
          $init
          bless \\\%h,\$cls;
        }
        sub node_name { '$name' }
        sub match { shift->{match} }
        sub field_names { @{[
                             @fields == 0
                             ? 'return ()'
                             : 'qw{ '.join(" ",@fields).' }'
          ]} }
        sub field_values { my(\$self)=\@_; @{[
                             @fields == 0
                             ? 'return ()'
                             : $field_values
          ]} }
        sub describe {
          my(\$self)=\@_;
          @{[ @fields == 0
              ? "${name}()"
              : '"'.$name.'(".join(",",map{$self->describe_anything($_)}'.$field_values.').")"'
          ]}
        }
      }
    END
  }
  text2file($code,$file);
}

sub write_ast_handlers {
  my $file = "./elf_a_src/ast_handlers.pl";
  my @paragraphs = load_paragraphs("./elf_a_src/ast_handlers.config");

  my $code = "#line 2 ast_handlers.pl\n".unindent(<<"  END");
    # Warning: This file is mechanically written.  Your changes will be overwritten.
    { package IRBuild;
  END

  for my $para (@paragraphs) {
    $para =~ /^([\w:]+)\n(.*)/s or die "bug";
    my($name,$body)=($1,$2);

    $body =~ s/(\$m(?:<\w+>)+)/ir($1)/g;
    $body =~ s/<(\w+)>/->{hash}{$1}/g;
    $body =~ s/([A-Z]\w+)\.new\(/IR::$1->new(\$m,/g;
    $body =~ s/\*text\*/(\$m->match_string)/g;
    if($body =~ /\*1\*/) {
      $body =~ s/\*1\*/\$one/g;
      $body = unindent(<<'      END',"  ").$body;
        my @keys = map{$_ eq "match" ? () : ($_)} keys %{$m->{hash}};
        die("Unexpectedly more than 1 field - dont know which to choose\n".
            $m->match_describe."\n") if(@keys > 1);
        my $one = ir($m->{hash}{$keys[0]});
      END
    }

    $code .= unindent(<<"    END","    ");
      \$IRBuild::constructors{'$name'} = sub {
        my(\$m)=\@_;
      $body;
      };
    END

  }
  $code .= unindent(<<"  END");
  }
  END
  text2file($code,$file);
}

sub write_emit_p5 {
  my $file = "./elf_a_src/emit_p5.pl";
  my @paragraphs = load_paragraphs("./elf_a_src/emit_p5.config");

  my $code = "#line 2 emit_p5.pl\n".unindent(<<'  END');
    # Warning: This file is mechanically written.  Your changes will be overwritten.
    { package IR;
      sub emit_p5_for {
        my($this,$tree)=@_;
        if(not ref($tree)) {
          $tree;
        } else {
          my $ref = ref($tree);
          if($ref eq 'ARRAY') {
            [map{$this->emit_p5_for($_)} @$tree]
          }
          elsif($ref eq 'HASH') {
            my %h;
            for my $k (keys %$tree) {
              my $v = $tree->{$k};
              $h{$k} = $this->emit_p5_for($v);
            }
            \%h;
          }
          else {
            $tree->emit_p5
          }
        }
      }
    }
    { package IR::All;
      sub emit_p5 {
        my $name = $_[0]->node_name;
        print STDERR "ERROR: emit_p5 is not defined for $name.\n";
        "***<$name>***";
      }
    }
  END

  for my $para (@paragraphs) {
    $para =~ /^(\w+)\n(.*)/s or die "bug";
    my($name,$body)=($1,$2);
    die "Unknown IR node in emit config: $name\n" if !IRNodesRef->node_named($name);
    $body =~ s/(\$n(?:<\w+>)+)/IR->emit_p5_for($1)/g;
    $body =~ s/<(\w+)>/->{$1}/g;
    $code .= unindent(<<"    END");
      { package IR::$name; sub emit_p5 {
          my(\$n)=\@_;
          $body
        }
      }
    END
  }
  text2file($code,$file);
}

#; Local Variables:
#; perl-indent-level: 2
#; End:
#; vim: shiftwidth=2:
