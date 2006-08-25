use v6-alpha;
use Test;

plan 9;

=pod

Tests for the &CGI::redirect function.

=cut

use CGI; pass "(dummy instead of broken use_ok)";

my $q = CGI.new;

is $q.redirect('http://www.example.com/'),
    "Status: 302 Found
Content-Type: 
Location: http://www.example.com/

", 'positional args -- location';

is $q.redirect('http://www.example.com/', 'foo'),
    "Status: 302 Found
Content-Type: 
Location: http://www.example.com/
Window-Target: foo

", 'positional args -- location, window target';

is $q.redirect('http://www.example.com/', 'foo', '301 Moved Permanently'),
    "Status: 301 Moved Permanently
Content-Type: 
Location: http://www.example.com/
Window-Target: foo

", 'positional args -- location, window target, status code';

is $q.redirect(location => 'http://www.example.com/'),
    "Status: 302 Found
Content-Type: 
Location: http://www.example.com/

", 'named args -- location';

is $q.redirect(location => 'http://www.example.com/', status => '301 Moved Permanently'),
    "Status: 301 Moved Permanently
Content-Type: 
Location: http://www.example.com/

", 'named args -- location, status';

is $q.redirect(status => '301 Moved Permanently', location => 'http://www.example.com/'),
    "Status: 301 Moved Permanently
Content-Type: 
Location: http://www.example.com/

", 'named args -- status, location';

is $q.redirect('http://www.example.com/', cookie => 'CUSTOMER=WILE_E_COYOTE; path=/; expires=Wednesday, 09-Nov-99 23:12:40 GMT'),
    "Status: 302 Found
Content-Type: 
Location: http://www.example.com/
Set-Cookie: CUSTOMER=WILE_E_COYOTE; path=/; expires=Wednesday, 09-Nov-99 23:12:40 GMT

", 'cookie';

is $q.redirect('http://www.example.com/', cost => "Three smackeroos"),
    "Status: 302 Found
Content-Type: 
Location: http://www.example.com/
Cost: Three smackeroos

", 'extra params';
