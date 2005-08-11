#!/usr/bin/pugs

use v6;
require Test;

plan 5;

=pod

Tests for CGI::Server module

=cut

use_ok('CGI::Server');

is(server_name(), 'localhost', 'got the right default value');
is(server_software(), 'cmdline', 'got the right default value');
is(server_port(), 80, 'got the right default value');
is(server_protocol(), 'HTTP/1.0', 'got the right default value');
