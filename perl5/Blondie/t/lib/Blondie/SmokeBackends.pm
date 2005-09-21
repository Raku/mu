#!/usr/bin/perl

package Blondie::SmokeBackends;

use strict;
use warnings;

use Test::More 'no_plan';
#use Test::NoWarnings;

use Blondie::TestPrograms qw/smoke_runtime/;;

sub import {
	my $class = shift;
	close STDERR;

	foreach my $backend (@_) {
		my $m = "Blondie::Backend::$backend";
		use_ok($m);

		can_ok($m, "new");
		isa_ok($m->new, $m);

		can_ok($m, "run");
		smoke_runtime($m);
	}
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::SmokeBackends - 

=head1 SYNOPSIS

	use Blondie::SmokeBackends;

=head1 DESCRIPTION

=cut


