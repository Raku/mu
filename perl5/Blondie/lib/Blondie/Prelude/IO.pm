#!/usr/bin/perl

package Blondie::Prelude::IO;

use strict;
use warnings;

use Blondie::Nodes;

sub symbols {
    stub('&print'),

    '$*OUT' => Stub('$*OUT'),

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

Blondie::Prelude::IO - 

=head1 SYNOPSIS

    use Blondie::Prelude::IO;

=head1 DESCRIPTION

=cut


