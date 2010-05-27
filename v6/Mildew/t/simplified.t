use strict;
use warnings;
use AST;
use Test::More;
my $empty_block = AST::Block->new(regs=>['interpreter','scope'],stmts=>[]);
my $simplified = $empty_block->simplified;
ok('lived after simplifing an empty block');
done_testing;
