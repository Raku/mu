#!/usr/bin/perl

package Blondie::Prelude::Control;

use strict;
use warnings;

use Blondie::Nodes;

sub symbols {
    stub('&ternary:<?? !!>', '$cond', '$true', '$false'), # non short circuiting

    '&control_structure:<if>' => Thunk(
        Seq(
            Param('$cond'),
            Param('$left_thunk'),
            Param('$right_thunk'),
            App(
                App(
                    Sym('&ternary:<?? !!>'),
                    Sym('$cond'),
                    Sym('$left_thunk'),
                    Sym('$right_thunk'),
                ),
            ),
        ),
    ),
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Prelude::Control - 

=head1 SYNOPSIS

    use Blondie::Prelude::Control;

=head1 DESCRIPTION

=cut


