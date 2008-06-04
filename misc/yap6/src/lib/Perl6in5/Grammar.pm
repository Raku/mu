package Perl6in5::Grammar;

use warnings;
use strict;

my $caller_package;
use File::Slurp;

use Perl6in5::Compiler::Trace; # set env var TRACE for trace output

use Perl6in5::Compiler::Stream ':all';
use Perl6in5::Compiler::Parser ':all';
use Perl6in5::Compiler::Lexer ':all';

use Filter::Simple;
FILTER {
    # if (/package\s+ ([A-Za-z_][:\w]*[A-Za-z_][\w]*);/) {   $caller_package = $1;   };
    
    # if (/^\s+rule\s+([A-Za-z_]\w*)\s+\{/) {
        # warn "trying to write to P65GL.pm in ";
        # write_file('P65GL.pm',"caller package is ".$caller_package);
    # }
    # a bit of misdirection so the grammars look more Perl6y.
    s/^\s+rule\s+([A-Za-z_])(\w*)\s+\{/\$\u$1$2=parser { \$$1$2->(\@_)\};\$N{\$\u$1$2}=\u$1$2;\$$1$2 = execnow {/mg;
};

use base 'Exporter';
our @EXPORT_OK = qw ( rule token load_rules);
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

sub rule ($&) {
    return;
}

# for fun
sub token { goto &rule }

1;
