#!/usr/bin/perl6

use Test;
use URI::Find::Simple;

ok my $text = qq:to/EOF/, "set text string";
this is a long string with http://www.news.com urls in it in
<http://various.com> different forms. mailto:tom\@jerakeen.org.
Some urls are ftp://not.http.urls/and/have/paths.
EOF

ok my @list = list_uris($text), "got list of uris";

is +@list, 4, "got 4 uris";
is @list[0], 'http://www.news.com/', "got news.com uri";
is @list[1], 'http://various.com/', "got various.com uri";
is @list[2], 'mailto:tom@jerakeen.org', "got email address";
is @list[3], 'ftp://not.http.urls/and/have/paths', "got ftp uri";

ok my $new_text = change_uris($text, -> { "[[ $^text ]]" }), "changed text";

ok my $expected = qq:to/EOF/, "set expected text string";
this is a long string with [[ http://www.news.com/ ]] urls in it in
[[ http://various.com/ ]] different forms. [[ mailto:tom\@jerakeen.org ]].
Some urls are [[ ftp://not.http.urls/and/have/paths ]].
EOF

is $new_text, $expected, "expcted matches new text";
