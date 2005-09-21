#!/usr/bin/perl

package Blondie::Prelude::IO;

use strict;
use warnings;

use Blondie::Nodes;

sub symbols {
    stub('&printf', '$handle', '$format', '$string'),

    '$*OUT' => Stub('$*OUT'),

	'&print' => Thunk(
		Seq(
			Param('$handle'),
			Param('$string'),
			App(
				Sym('&printf'),
				Sym('$handle'),
				Val('%s'),
				Sym('$string'),
			),
		),
	),
    '&say' => Thunk(
        Seq(
            Param('$string'),
            App(
                Sym('&print'),
                Sym('$*OUT'),
                App(
                    Sym('&infix:<~>'),
                    Sym('$string'),
                    Val("\n"),
                ),
            ),
        ),
    ),
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Prelude::IO - IO related builtins

=head1 SYNOPSIS

    use Blondie::Prelude::IO;

=head1 DESCRIPTION

=cut


