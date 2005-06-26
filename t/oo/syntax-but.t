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

plan 4;

class SampleClass { has $.var }

{
  my $was_in_but_block;
  my $topic_in_but_block;

  my $obj = eval 'SampleClass.new but {
    $was_in_but_block++;
    $topic_in_but_block++;
    .attr = 42;
    23;
  }';

  ok $was_in_but_block, 'syntax but ($obj but {...}) was executed';
  cmp_ok $topic_in_but_block, &infix:<=:=>, $obj,
    'topic in syntax but ($obj but {...}) was correct';
  is try { $obj.attr }, 42, "attribute setting worked correctly in syntax but";
  cmp_ok $obj, &infix:<!=>, 23, "syntax but returned the original object";
}
