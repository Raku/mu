use v6-alpha;

use Test;

# L<"http://use.perl.org/~autrijus/journal/25365">
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

plan 13;

# Without an own class
{
  my $was_in_but_block;
  my $topic_in_but_block;

  my $num = 3 but {
    # $_ is 3.
    $was_in_but_block++;
    $topic_in_but_block = $_;
    23;
    # Here is an implicit ($_;) to get 3 back to $num, instead of 23.
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
    # Here is an implicit ($_;) to get 3 back to $num, instead of 23.
  };

  ok $was_in_but_block, 'syntax but ($obj but {...}) was executed';
  cmp_ok $topic_in_but_block, &infix:<===>, $obj,
    'topic in syntax but ($obj but {...}) was correct';
  my $attr = try { $obj.attr };
  is $attr, 42, "attribute setting worked correctly in syntax but";
  cmp_ok $obj, &infix:<~~>, SampleClass, "syntax but returned the original object";
}

# L<S02/Context/"can override the class definition:">
# L<S12/Roles/generalize a particular enumerated value to its role.>
my $true_zero is context;
eval_ok '$+true_zero = 0 but True; 1', "0 but True syntax evaluates", :todo<bug>;
ok ($true_zero == 0), "0 but True is numerically equal to 0";
ok ?($true_zero), "0 but True is true", :todo<bug>;
# TimToady++ says I can test False as well
my $false_positive is context;
eval_ok '$+false_positive = 3 but False; 1', "3 but False syntax evaluates", :todo<bug>;
ok ($false_positive == 3), "3 but False is numerically equal to 3", :todo<bug>;
ok !($false_positive), "3 but False is false";
