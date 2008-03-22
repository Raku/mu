#!/usr/bin/perl
# A script to help create and run elf_a .
use strict;
use warnings;

# IR nodes
our @nodes;
our %node_index;
load_ir_node_config();
# Generate various files.
write_ir_nodes();
write_ir_constructors();
write_emit_p5();
# Collect them into the executable.
system("(cd elf_a_src/; cat head.pl Match.pm ir_nodes.pl ir_build.pl ir_constructors.pl emit_p5.pl main.pl) > ./elf_a") == 0 or die $!;
# Run the executable - helps with debugging.
exec("./elf_a",@ARGV);
# That's it.


sub load_ir_node_config {
  my $ir_config = `cat ./elf_a_src/ir_nodes.config`;
  for my $line (split(/\n/,$ir_config)) {
    next if $line =~ /^\s*$|^\s*\#/;
    $line =~ s/#.*//;
    my($name,@fields)=eval('qw{'.$line.'}');
    push(@nodes,[$name,[@fields]]);
    $node_index{$name} = [$name,[@fields]];
  }
}

sub write_ir_nodes {
  my $code = "#line 2 ir_nodes.pl\n".<<'END';
# Warning: This file is mechanically written.  Your changes will be overwritten.
{ package IR::All;
}
END

  for my $node (@nodes) {
    my($name,@fields)=($node->[0],@{$node->[1]});
    
    $code .= <<"END";
{ package IR::$name;
  \@IR::${name}::ISA = qw( IR::All );
END

    my @all = ('zmatch',@fields);
    my $params = join(',',map{"\$$_"}@all);
    my $all_indexes = join(',',map{"'$_'"}@all);
    my $sig = @all == 1 ? '$' : '@';
    my $init = "${sig}h{$all_indexes}=($params);";
    
    $code .= <<"END";
  sub new {
    my(\$cls,$params)=\@_;
    my \%h;
    $init
    bless \\\%h,\$cls;
  }
  sub node_name { '$name' }
  sub match { shift->{zmatch} }
  sub field_names { @{[
                       @fields == 0
                       ? 'return ()'
                       : 'qw{ '.join(" ",@fields).' }'
    ]} }
  sub field_values { my(\$self)=\@_; @{[
                       @fields == 0
                       ? 'return ()'
                       : "${sig}\$self{".join(',',map{"'$_'"}@fields)."}"
    ]} }
}
END
  }
  open(F,">./elf_a_src/ir_nodes.pl") or die $!;
  print F $code;
  close F;
}

sub write_ir_constructors {
  my $ir_constructors = `cat ./elf_a_src/ir_constructors.config`;
  $ir_constructors =~ s/^\s*#.*\n//mg;
  $ir_constructors =~ s/\s*#.*//g;

  my $code = "#line 2 ir_constructors.pl\n".<<"END";
# Warning: This file is mechanically written.  Your changes will be overwritten.
{ package IRBuild;
END

  for my $para (split(/\n\n/,$ir_constructors)) {
    $para =~ /^(\w+)\n(.*)/s or die "bug";
    my($name,$body)=($1,$2);

    $body =~ s/(\$m(?:<\w+>)+)/ir($1)/g;
    $body =~ s/<(\w+)>/->{hash}{$1}/g;
    $body =~ s/([A-Z]\w+)\.new\(/IR::$1->new(\$m,/g;
    $body =~ s/\*text\*/(\$m->match_string)/g;
    if($body =~ /\*1\*/) {
      $body =~ s/\*1\*/\$one/g;
      $body = <<'END'.$body;
    my @keys = map{$_ eq "zmatch" ? () : ($_)} keys %{$m->{hash}};
    die("Unexpectedly more than 1 field - dont know which to choose\n".
        $m->match_describe."\n") if(@keys > 1);
    my $one = ir($m->{hash}{$keys[0]});
END
    }

    $code .= <<"END";
  \$IRBuild::constructors{$name} = sub {
    my(\$m)=\@_;
    $body;
  };
END

  }
  $code .= <<"END";
}
END
  open(F,">./elf_a_src/ir_constructors.pl") or die $!;
  print F $code;
  close F;
}

sub write_emit_p5 {
  my $emit_p5 = `cat ./elf_a_src/emit_p5.config`;
  $emit_p5 =~ s/^\s*#.*\n//mg;
  $emit_p5 =~ s/\s*#.*//g;

  my $code = "#line 2 emit_p5.pl\n".<<'END';
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

  for my $para (split(/\n\n/,$emit_p5)) {
    $para =~ /^(\w+)\n(.*)/s or die "bug";
    my($name,$body)=($1,$2);
    die "Unknown IR node in emit config: $name\n" if !$node_index{$name};
    $body =~ s/(\$n(?:<\w+>)+)/IR->emit_p5_for($1)/g;
    $body =~ s/<(\w+)>/->{$1}/g;
    $code .= <<"END";
{ package IR::$name; sub emit_p5 {
    my(\$n)=\@_;
    $body
  }
}
END
  }
  $code .= <<'END';
END
  open(F,">./elf_a_src/emit_p5.pl") or die $!;
  print F $code;
  close F;
}
