#!/usr/bin/pugs

use v6;
use Test;

plan 25;

=pod

basic tests for url_encode/decode

=cut

use_ok('CGI');

is(url_decode('with%20space'), 'with space', 'got the right decoded string');
is(url_decode('with%20more%20than%20one%20space'), 'with more than one space', 'got the right decoded string');
is(url_decode('string%20%26%20stuff'), 'string & stuff', 'got the right decoded string');

is(url_encode('with space'), 'with%20space', 'got the right encoded string');
is(url_encode('with more than one space'), 'with%20more%20than%20one%20space', 'got the right encoded string');
is(url_encode('string & stuff'), 'string%20%26%20stuff', 'got the right encoded string');

is(url_decode('%26%7E%21%40%23%25%24%5E%28%29%2B%3D%7B%7D%5B%5D%7C%5C%3B%3A%27%22%3C%3E%3F%2F%2C'),
  "&~!@#%$^()+=\{\}[]|\\;:'\"<>?/,", 
  'got the right decoded string');

is(url_encode("&~!@#%$^()+=\{\}[]|\\;:'\"<>?/,"),
  '%26%7E%21%40%23%25%24%5E%28%29%2B%3D%7B%7D%5B%5D%7C%5C%3B%3A%27%22%3C%3E%3F%2F%2C', 
  'got the right encoded string');

set_url_encoding('iso-8859-1');

is(url_decode('%E2%82%AC'), chr(0xE2)~chr(0x82)~chr(0xAC), 'got the right decoded string'); # 3 characters

set_url_encoding('utf-8');

is(url_decode('%E2%82%AC'), chr(0x20AC), 'got the right decoded string'); # euro, 1 character

is(url_decode('%00'), chr(0), 'got the right decoded string');
is(url_decode('%7F'), chr(127), 'got the right decoded string');
is(url_decode('%C2%80'), chr(128), 'got the right decoded string');
is(url_decode('%DF%BF'), chr(2047), 'got the right decoded string');
is(url_decode('%E0%A0%80'), chr(2048), 'got the right decoded string');
#is(url_decode('%EF%BF%BD'), chr(65533), 'got the right decoded string');
is(url_decode('%F0%90%80%80'), chr(65536), 'got the right decoded string');
is(url_decode('%F4%8F%BF%BE'), chr(1114110), 'got the right decoded string');

is(url_encode(chr(0)), '%00', 'got the right decoded string');
is(url_encode(chr(127)), '%7F', 'got the right decoded string');
is(url_encode(chr(128)), '%C2%80', 'got the right decoded string');
is(url_encode(chr(2047)), '%DF%BF', 'got the right decoded string');
is(url_encode(chr(2048)), '%E0%A0%80', 'got the right decoded string');
#is(url_encode(chr(65533)), '%EF%BF%BD', 'got the right decoded string');
is(url_encode(chr(65536)), '%F0%90%80%80', 'got the right decoded string');
is(url_encode(chr(1114110)), '%F4%8F%BF%BE', 'got the right decoded string');
