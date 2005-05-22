#!/usr/bin/pugs

use v6;
require Test;

plan 8;

=pod

More tests for CGI header() function

=cut

use_ok('CGI');

is(header(), 
   "Status: 200 OK
Content-type: text/html

", 'got the header we expected');

# check with positional parameters

is(header('404 Not Found'), 
   "Status: 404 Not Found
Content-type: text/html

", 'got the header we expected (using positional args)');

is(header('404 Not Found', 'text/xml'), 
   "Status: 404 Not Found
Content-type: text/xml

", 'got the header we expected (using positional args)');

is(header('404 Not Found', 'text/xml', 'Latin'), 
   "Status: 404 Not Found
Content-type: text/xml; charset=Latin

", 'got the header we expected (using positional args)');

# test it with named args

is(header(charset => 'Latin'), 
   "Status: 200 OK
Content-type: text/html; charset=Latin

", 'got the header we expected (using named args)');

is(header(charset => 'Arabic', status => '500 Internal Server Error'), 
   "Status: 500 Internal Server Error
Content-type: text/html; charset=Arabic

", 'got the header we expected (using named args)');

is(header(content_type => 'text/xml', charset => 'Chinese', status => '500 Internal Server Error'), 
   "Status: 500 Internal Server Error
Content-type: text/xml; charset=Chinese

", 'got the header we expected (using named args)');

is header(cookies => "Foo"),
    "Status: 200 OK
Set-Cookie: Foo
Content-type: text/html

", "single cookie";
is header(cookies => ["Foo", "Bar"]),
    "Status: 200 OK
Set-Cookie: Foo
Set-Cookie: Bar
Content-type: text/html

", "two cookies";
is header(cookies => ["Foo", "Bar", "Baz"]),
    "Status: 200 OK
Set-Cookie: Foo
Set-Cookie: Bar
Set-Cookie: Baz
Content-type: text/html

", "three cookies";