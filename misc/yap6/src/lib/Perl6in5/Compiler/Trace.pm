package Perl6in5::Compiler::Trace;

# a source filter to blank lines that start or end with with trace
# so that the contents of the lines are not even produced...

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
        # remove the memoization stuff, b/c it breaks tracing/debugging.
        s/^.*memoize$//mg;
    } else {
        s/^\s+trace.*$//mg;
        s/^.*trace$//mg;
    }
}
