use strict;
use warnings;
use AST;
use Test::More;
my $empty_block = AST::Block->new(regs=>['interpreter','scope'],stmts=>[]);
my $simplified = $empty_block->simplified;
is_deeply($simplified->stmts,[]);
is_deeply($simplified->regs,['interpreter','scope']);

my $branch = AST::Branch->new(cond=>reg '$foo',then=>AST::Block->new(stmts=>[]),else=>AST::Block->new(stmts=>[]));
my ($branch_simplified,@setup) = $branch->simplified;
is_deeply(\@setup,[]);
isa_ok($branch_simplified,'AST::Branch');
done_testing;
