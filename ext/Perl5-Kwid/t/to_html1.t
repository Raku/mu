use strict;
use warnings;
use lib 't', 'lib';
use TestChunks;
use Kwid;

data_file 't/data1';
plan tests => 1 * chunks;

{
    for my $test (chunks) {
        my $result = eval {
            Kwid->new->kwid_to_html($test->{kwid}), 
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
