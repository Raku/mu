#!/usr/bin/pugs

use v6;
use Test;

# http://use.perl.org/~autrijus/journal/25365
# A closure form of but is desugared into a do given block that eliminates the
# need of returning $_ explicitly. So those two forms are equivalent:
#
#   my $foo = Cls.new but {
#       .attr = 1;
#   };
#
#   my $foo = do given Cls.new {
#       .attr = 1;
#       $_;
#   };

plan 7;

# Without an own class
{
  my $was_in_but_block;
  my $topic_in_but_block;

  my $num = 3 but {
    # $_ is 3.
    $was_in_but_block++;
    $topic_in_but_block = $_;
    23;
    # Here is an implicit ($_;) to get 3 back to $num, insteaf of 23.
  };

  is $num,                3, "syntax but worked on a literal";
  ok $was_in_but_block,      "syntax but on a literal was executed";
  is $topic_in_but_block, 3, "topic in syntax but on a literal was correct";
}

# With an own class
{
  class SampleClass { has $.attr }

  my $was_in_but_block;
  my $topic_in_but_block;

  my $obj = SampleClass.new but {
    # $_ is the fresh SampleClass.new.
    $was_in_but_block++;
    $topic_in_but_block = $_;
    .attr = 42;
    23;
    # Here is an implicit ($_;) to get 3 back to $num, insteaf of 23.
  };

  ok $was_in_but_block, 'syntax but ($obj but {...}) was executed';
  cmp_ok $topic_in_but_block, &infix:<=:=>, $obj,
    'topic in syntax but ($obj but {...}) was correct';
  my $attr = try { $obj.attr };
  is $attr, 42, "attribute setting worked correctly in syntax but";
  cmp_ok $obj, &infix:<~~>, SampleClass, "syntax but returned the original object";
}
