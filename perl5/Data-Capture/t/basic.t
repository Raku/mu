#!/usr/bin/perl
use strict;
use warnings;
use Test::More tests => 6;

use_ok('Data::Capture');

my $c = Data::Capture->new( { invocant => \'xyz',
			      positional => [\(2, 'fnord', 3)],
			      named => { xy => \3, fnord => \5 } } );

is(${ $c->invocant }, 'xyz');

is_deeply( [map {$$_} @{ $c->positional }], [2, 'fnord', 3] );
is(${$c->positional->[1]}, 'fnord');

is(${$c->named->{xy}}, 3);
is(${$c->named->{fnord}}, 5);
