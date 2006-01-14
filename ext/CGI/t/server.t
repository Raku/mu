#!/usr/bin/pugs

use v6;
use Test;

plan 5;

=pod

Tests for CGI::Server module

=cut

use CGI::Server; pass "(dummy instead of broken use_ok)";

is(server_name(), 'localhost', 'got the right default value');
is(server_software(), 'cmdline', 'got the right default value');
is(server_port(), 80, 'got the right default value');
is(server_protocol(), 'HTTP/1.0', 'got the right default value');
