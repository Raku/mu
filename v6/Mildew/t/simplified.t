use strict;
use warnings;
use Mildew::AST;
use Test::More;
my $empty_block = Mildew::AST::Block->new(regs=>['interpreter','scope'],stmts=>[]);
my $simplified = $empty_block->simplified;
is_deeply($simplified->stmts,[]);
is_deeply($simplified->regs,['interpreter','scope']);

my $branch = Mildew::AST::Branch->new(cond=>reg '$foo',then=>Mildew::AST::Block->new(stmts=>[]),else=>Mildew::AST::Block->new(stmts=>[]));
my ($branch_simplified,@setup) = $branch->simplified;
is_deeply(\@setup,[]);
isa_ok($branch_simplified,'Mildew::AST::Branch');
done_testing;
