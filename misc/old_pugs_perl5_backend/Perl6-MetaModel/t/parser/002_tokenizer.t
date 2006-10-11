#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::MetaModel::Parser::Tokenizer');

{
    my $t = Perl6::MetaModel::Parser::Tokenizer->new();
    isa_ok($t, 'Perl6::MetaModel::Parser::Tokenizer');

    my $source = "class Foo {}";
    
    is((join '' => $t->tokenize($source)), $source, '... tokenized the source correctly');
}

{
    my $t = Perl6::MetaModel::Parser::Tokenizer->new();
    isa_ok($t, 'Perl6::MetaModel::Parser::Tokenizer');

    my $source = q|
    class Foo {
        has $.bar;
    }
    |;

    is((join '' => $t->tokenize($source)), $source, '... tokenized the source correctly');
}

{
    my $t = Perl6::MetaModel::Parser::Tokenizer->new();
    isa_ok($t, 'Perl6::MetaModel::Parser::Tokenizer');

    my $source = q|
    class Foo {
        has $.bar;
        has $:baz;
    }
    |;

    is((join '' => $t->tokenize($source)), $source, '... tokenized the source correctly');
}

{
    my $t = Perl6::MetaModel::Parser::Tokenizer->new();
    isa_ok($t, 'Perl6::MetaModel::Parser::Tokenizer');

    my $source = q|
    class Foo {
        has $.bar;
        method baz (Foo $self:) {
            $.bar;
        }
    }
    |;

    is((join '' => $t->tokenize($source)), $source, '... tokenized the source correctly');
}

{
    my $t = Perl6::MetaModel::Parser::Tokenizer->new();
    isa_ok($t, 'Perl6::MetaModel::Parser::Tokenizer');

    my $source = q|
class Foo {
    has @.bar;
    method baz (Foo $self:) {
        push @.bar, (1, 2, 3);
    }
}
|;

    my @tokens = ('', "\n",
    'class', ' ', 'Foo', ' ', '{', "\n",
    '    ', 'has', ' ', '@.bar', ';', "\n",
    '    ', 'method', ' ', 'baz', ' ', '(', 'Foo', ' ', '$self:', ')', ' ', '{', "\n", 
    '        ', 'push', ' ', '@.bar', ',', ' ', '(', '1', ',', ' ', '2', ',', ' ', '3', ')', ';', "\n",
    '    ', '}', "\n",
    '}', "\n");

    $t->tokenize($source);
    my $i = $t->iterator;    
    while ($i->hasNextToken()) {
        is($i->nextToken(), shift @tokens, '... got the right next-token');
    }
    
    is((join '' => $t->tokenize($source)), $source, '... tokenized the source correctly');    
}


