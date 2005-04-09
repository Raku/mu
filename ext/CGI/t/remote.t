#!/usr/bin/pugs

use v6;
require Test;

plan 6;

=pod

Tests for CGI::Remote

=cut

use_ok('CGI::Remote');

is(remote_host(), 'localhost', 'got the right default value');
is(remote_address(), '127.0.0.1', 'got the right default value');
is(remote_ident(), undef, 'got the right default value (undef)');
is(remote_user(), undef, 'got the right default value (undef)');
is(auth_type(), undef, 'got the right default value (undef)');