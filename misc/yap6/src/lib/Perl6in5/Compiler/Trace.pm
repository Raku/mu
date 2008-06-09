package Perl6in5::Compiler::Trace;

# a source filter to blank lines that start or end with with trace

use Filter::Simple;
FILTER {
    my $level = $ENV{TRACE};
    my $maxtrace = 6;
    if (defined $level && $level <= $maxtrace && $level > 0) {
        s/'tracelevel'/${level}/mg;
        while ($level < $maxtrace ) {
            $level++;
            s/^\s+trace.${level}.*$//mg;
        }
    } else {
        s/^\s+trace.*$//mg;
        s/^.*trace$//mg;
    }
}
