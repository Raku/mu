use strict;
use warnings;
use lib 't', 'lib';
use TestChunks;
use Kwid;

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


__DATA__
=== Basic Kwid to HTML
==> kwid
This is a paragraph.

This is a second paragraph.
With 2 lines.
==> html
<p>
This is a paragraph.
</p>
<p>
This is a second paragraph. With 2 lines.
</p>
==> pod
This is a paragraph.

This is a second paragraph.
With 2 lines.
=== Line Comments
==> kwid
#
#content line
# comment line
2nd line

# line1
# line2


line3
==> html
<p>
#content line 2nd line
</p>
<p>
line3
</p>
=== HTML Escaping
==> kwid
<foo> & </bar>
==> html
<p>
&lt;foo&gt; &amp; &lt;/bar&gt;
</p>
