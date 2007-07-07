use strict;
use warnings;
use Test::More 'no_plan';
use Pugs::Compiler::Grammar;

my $obj = Pugs::Compiler::Grammar->compile(<<'_EOC_');
grammar Perl;

token foo { 'a'* }

grammar C ;

token bar { <ident> }

_EOC_
ok $obj, 'obj ok';
isa_ok $obj, 'Pugs::Compiler::Grammar';
ok $obj->{perl5}, 'p5 code okay';
eval $obj->{perl5};
is $@, '', "no error while eval";
my $match = Perl->foo('aaba');
ok $match, 'matched';
is $match, 'aa', 'capture okay';

$match = C->bar('hello');
is $match, 'hello', 'capture okay';

$obj = Pugs::Compiler::Grammar->compile(<<'_EOC_');
grammar MyC;
token def {
    <type> <?ws> <var_list> <?ws>? ';'
}
token type { int | float | double | char }
token var_list { <ident>**{1} <?ws>? [ ',' <?ws>? <ident> ]* }
_EOC_
ok $obj;
isa_ok $obj, 'Pugs::Compiler::Grammar';
ok $obj->{perl5}, 'p5 code okay';
eval $obj->{perl5};
$match = MyC->def('int a, b, c;');
ok $match->bool, 'matched';
is $match, 'int a, b, c;';
$match = MyC->type('double');
is $match, 'double';
$match = MyC->def('char d,b ;');
is $match, 'char d,b ;';
is $match->{type}, 'char';
is $match->{var_list}->{ident}->[0], 'd';
#print $obj->perl5;

