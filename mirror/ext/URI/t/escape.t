#!/usr/bin/pugs
use v6;

use Test;
plan 8;

use URI::Escape;

is(uri_escape("|abc\229"), '%7Cabc%E5', 'uri_escape');
is(uri_escape("abc", 'b-d'), 'a%62%63', 'uri_escape (explicit unsafe chars)');
#ok(!defined uri_escape(undef), 'uri_escape(undef)'); # not applicable

is(uri_unescape('%7Cabc%e5'), '|abcå', 'uri_unescape');
is(uri_unescape("%40A%42", "CDE", "F%47H").join(':'), '@AB:CDE:FGH', 'uri_unescape (list)');

skip_rest('use doesn\'t work properly');

{
    #use URI::Escape <%escapes>;
    #is(%escapes<%>, '%25', '%escapes<%> eq %25');
}

{
    #use URI::Escape <uri_unescape_utf8>;
    
    # XXX parse error here for some reason...
    #throws_ok(uri_escape('abc' . chr(300)), rx:P5/^Can't escape \\x{012C}, try uri_escape_utf8\(\) instead/, 'uri_escape doesn't handle characters > 255');
    #is(uri_escape_utf8(chr(0xFFF)), '%E0%BF%BF', 'uri_escape_utf8', :todo<feature>);
}
