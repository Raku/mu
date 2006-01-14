#!/usr/bin/pugs

use v6;
use Test;

plan 13;

=pod

More tests for CGI header() function

=cut

use CGI; pass "(dummy instead of broken use_ok)";

is(header(), 
   "Status: 200 OK
Content-Type: text/html

", 'got the header we expected');

# check with positional parameters

is(header('text/html', '404 Not Found'), 
   "Status: 404 Not Found
Content-Type: text/html

", 'got the header we expected (using positional args)');

is(header('text/xml', '404 Not Found'), 
   "Status: 404 Not Found
Content-Type: text/xml

", 'got the header we expected (using positional args)');

is(header('text/xml', '404 Not Found', 'Latin'), 
   "Status: 404 Not Found
Content-Type: text/xml; charset=Latin

", 'got the header we expected (using positional args)');

# test it with named args

is(header(charset => 'Latin'), 
   "Status: 200 OK
Content-Type: text/html; charset=Latin

", 'got the header we expected (using named args)');

is(header(charset => 'Arabic', status => '500 Internal Server Error'), 
   "Status: 500 Internal Server Error
Content-Type: text/html; charset=Arabic

", 'got the header we expected (using named args)');

is(header(content_type => 'text/xml', charset => 'Chinese', status => '500 Internal Server Error'), 
   "Status: 500 Internal Server Error
Content-Type: text/xml; charset=Chinese

", 'got the header we expected (using named args)');

is header(cookies => "Foo"),
    "Status: 200 OK
Content-Type: text/html
Set-Cookie: Foo

", "single cookie";
is header(cookies => ["Foo", "Bar"]),
    "Status: 200 OK
Content-Type: text/html
Set-Cookie: Foo
Set-Cookie: Bar

", "two cookies";
is header(cookies => ["Foo", "Bar", "Baz"]),
    "Status: 200 OK
Content-Type: text/html
Set-Cookie: Foo
Set-Cookie: Bar
Set-Cookie: Baz

", "three cookies";

is header(cost => "Three smackeroos"),
    "Status: 200 OK
Content-Type: text/html
Cost: Three smackeroos

", 'extra params';

is header(cost => "Three smackeroos", tax_deductible => "Yes"),
    "Status: 200 OK
Content-Type: text/html
Cost: Three smackeroos
Tax-Deductible: Yes

", 'extra params (hyphenation)';
