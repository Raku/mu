use strict;
use warnings;
use AST;
use Test::More;
use v5.10;
use Mildew::Frontend::M0ld;
my $frontend = Mildew::Frontend::M0ld->new;
{
my $ast = $frontend->parse('my $foo = 123;');
isa_ok($ast,'AST::Block');
isa_ok($ast->stmts->[0],'AST::Assign');
is($ast->stmts->[0]->rvalue->value,123);
}
{
my $ast = $frontend->parse('my $foo = "\n";');
isa_ok($ast,'AST::Block');
isa_ok($ast->stmts->[0],'AST::Assign');
is($ast->stmts->[0]->rvalue->value,"\n");
}
done_testing;


