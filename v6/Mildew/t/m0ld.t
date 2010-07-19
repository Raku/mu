use strict;
use warnings;
use Mildew::AST;
use Test::More;
use Test::Exception;
use v5.10;
use Mildew::Frontend::M0ld;
my $frontend = Mildew::Frontend::M0ld->new;

{
    my $ast1 = $frontend->parse('my $foo;my $bar;');
    my $ast2 = $frontend->parse('my $foo;my $bar;my $baz;');
    is_deeply($ast1->regs,['foo','bar']);
    is_deeply($ast2->regs,['foo','bar','baz']);
}

{
my $ast = $frontend->parse('my $foo = 123;');
isa_ok($ast,'Mildew::AST::Block');
my $seq = $ast->stmts->[0];
isa_ok($seq,'Mildew::AST::Seq');
isa_ok($seq->stmts->[0],'Mildew::AST::Assign');
is($seq->stmts->[0]->rvalue->value,123);
}

{
my $ast = $frontend->parse('my $foo = "\n";');
my $seq = $ast->stmts->[0];
isa_ok($ast,'Mildew::AST::Block');
isa_ok($seq,'Mildew::AST::Seq');
isa_ok($seq->stmts->[0],'Mildew::AST::Assign');
is($seq->stmts->[0]->rvalue->value,"\n");
}

throws_ok {
my $ast = $frontend->parse('syntax error ?!@?!');
} qr/Can't parse m0ld code/, 'syntax error';

{
my $ast = $frontend->parse('foo: bar: my $foo = 1;');
is($ast->stmts->[0]->id,'foo');
is($ast->stmts->[1]->id,'bar');
isa_ok($ast->stmts->[1]->stmts->[0],'Mildew::AST::Assign');
}

{
my $ast = $frontend->parse('goto foo;foo: my $foo = 1;goto foo;');
is($ast->stmts->[1]->id,'foo');
isa_ok($ast->stmts->[1]->stmts->[0],'Mildew::AST::Assign');
isa_ok($ast->stmts->[1]->stmts->[1],'Mildew::AST::Goto');
is($ast->stmts->[1]->stmts->[1]->block,$ast->stmts->[1]);
}

{
my $ast = $frontend->parse('
my $cond = 1;
my $foo;
if $cond { goto true } else { goto false };
false: $foo = 1;
true:  $foo = 0;
');
isa_ok($ast->stmts->[0]->stmts->[1],'Mildew::AST::Branch','conditional branch');
}

{
my $ast = $frontend->parse('
my $subm0ld = mold {
my $cond = 1;
};
');
isa_ok($ast->stmts->[0]->stmts->[0]->rvalue,'Mildew::AST::Block','submold');
isa_ok($ast->stmts->[0]->stmts->[0]->rvalue->stmts->[0]->stmts->[0],'Mildew::AST::Assign','assignment in submold');
}

{
my $ast = $frontend->parse('
    noop;
    noop;
');

is_deeply($ast->stmts,[],'noops');
}

throws_ok {
my $ast = $frontend->parse('foo: noop;foo: noop;');
} qr/label foo/, 'Label duplication is caught';


done_testing;


