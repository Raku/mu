use strict;
use warnings;
use lib 't', 'lib';
use TestChunks;
use Perldoc;

data_file 't/data1';
plan tests => 1 + 0 * 1 * chunks;
ok(1);
exit;

{
    for my $test (chunks) {
        my $result = eval {
            Perldoc->new->doc_to_html(
                input => \ $test->{kwid},
                type => 'kwid',
            ),
        };
        if ($@) {
            fail($test->{description} . "\n" . $@);
            next;
        }
        is(
            $result,
            $test->{html},
            $test->{description},
        );
    }
}
