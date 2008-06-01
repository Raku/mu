package Perl6in5::Interpreter;

use warnings;
use strict;

use File::Slurp;
use base 'Exporter';
our @EXPORT = qw ( make_runner );

# Interpreter accepts a grammar generator coderef (produced by make_parser) and an input stream/reader.  When invoked with the input stream/reader, the parser generator coderef returns a coderef with 

#use Perl6in5::Grammar 'make_grammar'; # someday

# Eventually the make_parser will return an AST
#use Perl6in5::Compiler::AST 'make_AST';
# make_AST takes a parser generator and an input generator and returns an AST generator.

# Today we're just going to 'use' whatever Grammar is passed by the invocant,
# because we don't yet have a default standard Perl 6 Grammar (in Perl 5), nor
# do we have one that produces an AST, nor a backend to evaluate it. :)

sub make_runner {
    my ($grammar, $inputfile) = @_;
    my $input = read_file($inputfile);
    my @data = ($input);
    my $reader = sub { return shift @data };
    eval( "use $grammar;" );
    if (@$) { die 'failed to load grammar: '.$! };
    my $runner = eval( $grammar.'::make_parser($reader)' );
    if (@$) { die 'parser generation failed: '.$! };
    $runner;
}