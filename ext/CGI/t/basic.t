#!/usr/bin/pugs

use v6;
require Test;

=pod

Very basic tests for CGI

=cut

require CGI;

is(header(), "Content-type: text/html\n\n", 'got the header correctly');

is(url_encode('this is a string to encode'), 'this+is+a+string+to+encode', 'got the right encoded string');
is(url_decode('this+is+a+string+to+decode'), 'this is a string to decode', 'got the right decoded string');

my $query_string = 'test=hello+world&this+is+a+number=1';

unpack_params($query_string);
is(pack_params(), $query_string, 'packed the params correctly');

my @param_keys = param();
is(+@param_keys, 2, 'we have 2 parameters');
is(@param_keys[0], 'test', 'the first one is "test"');
is(@param_keys[1], 'this is a number', 'the second one is "this is a number"');

is(param(@param_keys[0]), 'hello world', 'the value for param "test" is correct');
is(param(@param_keys[1]), 1, 'the value for param "this is a number" is correct');

