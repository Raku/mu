use strict;
use warnings;
use AST;
use Test::More;
my $empty_block = AST::Block->new(regs=>['interpreter','scope'],stmts=>[]);
my $simplified = $empty_block->simplified;
is_deeply($simplified->stmts,[]);
is_deeply($simplified->regs,['interpreter','scope']);
done_testing;
