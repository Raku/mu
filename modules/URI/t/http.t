#!/usr/bin/perl6

require Test;
require URI;

plan 19;

my URI $u .= new(uri => "<http://www.perl.com/path?q=fôo>");

is ~$u, "http://www.perl.com/path?q=f%F4o";
is $u.port, 80;

ok $u.port = 8080;
is $u.port, 8080;
is ~$u, "http://www.perl.com:8080/path?q=f%F4o";
ok $u.port = 80;
is ~$u, "http://www.perl.com:80/path?q=f%F4o";

$u.port = "";
is ~$u, "http://www.perl.com:/path?q=f%F4o";
is $u.port, 80;

$u.port = undef;
is ~$u, "http://www.perl.com/path?q=f%F4o";

my @q = $u.query_form;
is +@q, 2;
is ~@q, "q fôo";
$u.query_form = (foo => "bar", bar => "baz");
is $u.query, "foo=bar&bar=baz";

is $u.host, "www.perl.com";

is $u.path, "/path";

ok $u.scheme = "https";
is $u.port, 443;
is ~$u, "https://www.perl.com/path?foo=bar&bar=baz";

my URI $v .= new(uri => "http://%77%77%77%2e%70%65%72%6c%2e%63%6f%6d/%70%75%62/%61/%32%30%30%31/%30%38/%32%37/%62%6a%6f%72%6e%73%74%61%64%2e%68%74%6d%6c");
is ~$u.canonical, "http://www.perl.com/pub/a/2001/08/27/bjornstad.html";
