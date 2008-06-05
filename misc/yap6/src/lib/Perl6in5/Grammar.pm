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
    # Convert:  rule rulename {
    # To:       $rulename = execnow (implied sub) {
    # execnow() merely runs the sub, just as if it were declared
    # the normal way in MJD's grammar system.
    s/^\s+rule\s+([A-Za-z_])(\w*)\s+\{/\$$1$2 = execnow {/mg;
    s/'(.)'/ch('$1')/mg;
};

use base 'Exporter';
our @EXPORT_OK = qw ( rule token load_rules);
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

1;
