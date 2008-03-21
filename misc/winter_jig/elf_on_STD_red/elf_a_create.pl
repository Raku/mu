#!/usr/bin/perl
# A script to help create and run elf_a .
use strict;
use warnings;

my $ir_config = `cat ./elf_a_src/ir_nodes.config`;
open(F,">./elf_a_src/ir_nodes.pl") or die $!;
print F "#line 1 ir_nodes.pl\n";
print F "# Warning: This file is mechanically written.  Your changes will be overwritten.\n";
for my $line (split(/\n/,$ir_config)) {
  next if $line =~ /^\s*$|^\s*\#/;
  my($name,@fields)=eval('qw{'.$line.'}');
  my @all = ('match',@fields);

  my $params = join(',',map{"\$$_"}@all);
  my $all_indexes = join(',',map{"'$_'"}@all);
  my $sig = @all == 1 ? '$' : '@';
  my $init = "${sig}h{$all_indexes}=($params);";

  my $code = <<"END";
  sub new {
    my(\$cls,$params)=\@_;
    my \%h = {};
    $init
    bless \\\%h,\$cls;
  }
  sub match { shift->{match} }
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
END
  $code = "{ package $name;\n$code\n}\n";
  print F $code;
}
close F;


system("(cd elf_a_src/; cat head.pl Match.pm ir_nodes.pl main.pl) > ./elf_a") == 0 or die $!;

exec("./elf_a",@ARGV);
