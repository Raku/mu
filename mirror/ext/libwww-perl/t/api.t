#!/usr/bin/pugs

use v6;
require Test;

=head1 API Test

Tests that the same API gets exported as with the
Perl 5 version of the module.

=cut

my @api = < get getstore getprint mirror head >;
plan 1+@api;

use_ok('LWP::Simple');

for @api -> $function {
  ok( eval( "defined &$function" ), "$function is exported" );
};
