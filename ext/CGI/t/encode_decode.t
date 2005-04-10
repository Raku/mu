#!/usr/bin/pugs

use v6;
require Test;

plan 9;

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
