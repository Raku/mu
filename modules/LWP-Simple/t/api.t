#!/usr/bin/pugs

use v6;
require Test;

=head1 API Test

Tests that the same API gets exported as with the
Perl5 version of the module.

=cut

my @api = < get getstore getprint mirror head >;
plan 1+@api;

# TODO: use use_ok() instead of hand-rolled variant
try {
  require LWP::Simple;
};

is( $!, undef, "LWP::Simple imported" );

for @api -> $function {
  ok( eval "defined &$function", "$function is exported" );
};
