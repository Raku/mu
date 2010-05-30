use strict;
use warnings;
use AST;
use Test::More;
use Test::Exception;
use v5.10;
use Mildew::Frontend::M0ld;
my $frontend = Mildew::Frontend::M0ld->new;
{
my $ast = $frontend->parse('my $foo = 123;');
isa_ok($ast,'AST::Block');
my $seq = $ast->stmts->[0];
isa_ok($seq,'AST::Seq');
isa_ok($seq->stmts->[0],'AST::Assign');
is($seq->stmts->[0]->rvalue->value,123);
}

{
my $ast = $frontend->parse('my $foo = "\n";');
my $seq = $ast->stmts->[0];
isa_ok($ast,'AST::Block');
isa_ok($seq,'AST::Seq');
isa_ok($seq->stmts->[0],'AST::Assign');
is($seq->stmts->[0]->rvalue->value,"\n");
}

throws_ok {
my $ast = $frontend->parse('syntax error ?!@?!');
} qr/Can't parse m0ld code/, 'syntax error';

{
my $ast = $frontend->parse('foo: bar: my $foo = 1;');
is($ast->stmts->[0]->id,'foo');
is($ast->stmts->[1]->id,'bar');
isa_ok($ast->stmts->[1]->stmts->[0],'AST::Assign');
}
{
my $ast = $frontend->parse('goto foo;foo: my $foo = 1;goto foo;');
is($ast->stmts->[1]->id,'foo');
isa_ok($ast->stmts->[1]->stmts->[0],'AST::Assign');
isa_ok($ast->stmts->[1]->stmts->[1],'AST::Goto');
is($ast->stmts->[1]->stmts->[1]->block,$ast->stmts->[1]);
}

done_testing;


