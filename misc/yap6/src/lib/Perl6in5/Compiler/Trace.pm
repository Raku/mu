package Perl6in5::Compiler::Trace;

# a source filter to blank lines that start or end with with trace

use Filter::Simple;
FILTER {
    s/^\s+trace.*$//mg unless ($ENV{TRACE});
    s/^.*trace$//mg unless ($ENV{TRACE});
}
