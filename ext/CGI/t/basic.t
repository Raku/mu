#!/usr/bin/pugs

use v6;
require Test;

plan 21;

=pod

Very basic tests for CGI

=cut

use_ok('CGI');

is(header(), "Status: 200 OK\nContent-type: text/html\n\n", 'got the header correctly');
is(redirect("http://www.yahoo.com"), "Status: 302 Moved\nLocation: http://www.yahoo.com\n\n", 'got the header correctly');

is(url_encode('this is a string to encode'), 'this%20is%20a%20string%20to%20encode', 'got the right encoded string');
is(url_decode('this+is+a+string+to+decode'), 'this is a string to decode', 'got the right decoded string');
is(url_decode('this%20is%20a%20string%20to%20decode'), 'this is a string to decode', 'got the right decoded string');

my $query_string = 'test=hello%20world&this%20is%20a%20number=1';

unpack_params($query_string);
is(pack_params(), $query_string, 'packed the params correctly');

my @param_keys = param();
is(+@param_keys, 2, 'we have 2 parameters');
is(@param_keys[0], 'test', 'the first one is "test"');
is(@param_keys[1], 'this is a number', 'the second one is "this is a number"');

is(param(@param_keys[0]), 'hello world', 'the value for param "test" is correct');
is(param(@param_keys[1]), 1, 'the value for param "this is a number" is correct');

# these will all be undefined
is(query_string(), undef, 'the value returned is undef');
is(request_method(), undef, 'the value returned is undef');
is(content_type(), undef, 'the value returned is undef');
is(content_length(), undef, 'the value returned is undef');

# as with these
is(path_info(), undef, 'the value returned is undef');
is(request_uri(), undef, 'the value returned is undef');
is(referer(), undef, 'the value returned is undef');
is(document_root(), undef, 'the value returned is undef');

# but not this one
is(script_name(), $*PROGRAM_NAME, 'the value returned is that of $*PROGRAM_NAME');
