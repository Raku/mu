package Perl6in5::Grammar;

use warnings;
use strict;

use Filter::Simple;
FILTER {
    # a bit of misdirection so the grammars look more Perl6y.
    s/^\s+rule\s+([A-Za-z_])(\w*)\s+\{/\$\u$1$2=parser { \$$1$2->(\@_)\};\$N{\$\u$1$2}=\u$1$2;\$$1$2 = execnow {/mg;
};

use base 'Exporter';
our @EXPORT_OK = qw ( rule token load_rules);
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

use Perl6in5::Compiler::Trace; # set env var TRACE for trace output

use Perl6in5::Compiler::Stream ':all';
use Perl6in5::Compiler::Parser ':all';
use Perl6in5::Compiler::Lexer ':all';

sub rule ($&) {
    return;
}

# for fun
sub token { goto &rule }

1;
