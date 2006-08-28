use v6-alpha;
use Test;

plan 22;

=pod

Very basic tests for CGI

=cut

use CGI; pass "(dummy instead of broken use_ok)";

my $q = CGI.new;

is($q.header,
    "Status: 200 OK
Content-Type: text/html

", 'got the header correctly');

is($q.redirect("http://www.yahoo.com"),
    "Status: 302 Found
Content-Type: 
Location: http://www.yahoo.com

", 'got the header correctly');

is($q.url_encode('this is a string to encode'), 'this%20is%20a%20string%20to%20encode', 'got the right encoded string');
is($q.url_decode('this+is+a+string+to+decode'), 'this is a string to decode', 'got the right decoded string');
is($q.url_decode('this%20is%20a%20string%20to%20decode'), 'this is a string to decode', 'got the right decoded string');

my $query_string = 'test=hello%20world;this%20is%20a%20number=1';

$q.unpack_params($query_string);
is($q.pack_params, $query_string, 'packed the params correctly');

my @param_keys = $q.param;
is(+@param_keys, 2, 'we have 2 parameters');
is(@param_keys[0], 'test', 'the first one is "test"');
is(@param_keys[1], 'this is a number', 'the second one is "this is a number"');

is($q.param(@param_keys[0]), 'hello world', 'the value for param "test" is correct');
is($q.param(@param_keys[1]), 1, 'the value for param "this is a number" is correct');

# these will all be undefined
is($q.query_string, undef, 'the value returned is undef');
is($q.request_method, undef, 'the value returned is undef');
is($q.content_type, undef, 'the value returned is undef');
is($q.content_length, undef, 'the value returned is undef');

# as with these
is($q.path_info, undef, 'the value returned is undef');
is($q.request_uri, undef, 'the value returned is undef');
is($q.referer, undef, 'the value returned is undef');
is($q.document_root, undef, 'the value returned is undef');

# but not this one
is($q.script_name, $*PROGRAM_NAME, 'the value returned is that of $*PROGRAM_NAME');

%*ENV<PATH_INFO> = 'mark';
is($q.path_info, %*ENV<PATH_INFO>, "path_info method works as expected");
