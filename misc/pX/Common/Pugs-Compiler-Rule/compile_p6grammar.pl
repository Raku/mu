# Usage: perl compile_p6grammar.pl Perl6GrammarFile.pm > Perl5PCRFile.pm

package Grammar::Compiler;
use Pugs::Compiler::Rule;
use base 'Pugs::Grammar::Base';

*grammar_name = Pugs::Compiler::Rule->compile(q( \S+ ))->code;
*rule_name = Pugs::Compiler::Rule->compile(q( \w+ ))->code;
*block = Pugs::Compiler::Rule->compile(q( \{ [<block>|<any>]*? \} ))->code;
*rule = Pugs::Compiler::Rule->compile(q( rule <ws> <rule_name> <ws> <block> <ws> { return "*" . $<rule_name>() . " = Pugs::Compiler::Rule->compile(q(" . substr($<block>(), 1, -1) . "))->code;" } ))->code;
*grammar = Pugs::Compiler::Rule->compile(q( grammar <ws> <grammar_name>\; <ws> <rule>* { return "package " . $<grammar_name>() . ";\nuse Pugs::Compiler::Rule;\nuse base 'Pugs::Grammar::Base';\n\n" . join("\n", map { $_->() } @{$<rule>} ) . "\n" } ))->code;

package main;
use IO::File;

my $source_file = shift(@ARGV);
my $source = slurp($source_file);
my $match  = Grammar::Compiler->grammar($source);
print $match->();

sub slurp {
    my $fh = IO::File->new(shift) || return;
    return join('', $fh->getlines);
}

