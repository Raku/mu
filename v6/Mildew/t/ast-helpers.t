use strict;
use warnings;
use Mildew::AST::Helpers;
use Mildew::AST;
use Test::More;
use YAML::XS;
my $empty = [];
trailing_return($empty);
is_deeply($empty,[],"trailing return doesn't modify an empty list");
my $integer = [integer 12];
isa_ok($integer->[0],'Mildew::AST::IntegerConstant');
trailing_return($integer);
isa_ok($integer->[0],'Mildew::AST::IntegerConstant');

my $block = Mildew::AST::Block->new(stmts=>[integer 45],regs=>['foo']);
my $wrapped = wrap_in_block($block);
isa_ok($wrapped,'Mildew::AST::Block','a wrapped block');
# TODO more tests for wrap_in_block


done_testing;
