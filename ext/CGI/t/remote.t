#!/usr/bin/pugs

use v6;
use Test;

plan 6;

=pod

Tests for CGI::Remote

=cut

use CGI::Remote; pass "(dummy instead of broken use_ok)";

is(remote_host(), 'localhost', 'got the right default value');
is(remote_address(), '127.0.0.1', 'got the right default value');
is(remote_ident(), undef, 'got the right default value (undef)');
is(remote_user(), undef, 'got the right default value (undef)');
is(auth_type(), undef, 'got the right default value (undef)');
