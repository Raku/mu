#!/usr/bin/perl

package Blondie::Prelude::Math;

use strict;
use warnings;

use Blondie::Nodes;

sub symbols {
    (
		map { stub($_, '$x', '$y') }
        '&infix:<==>',
        '&infix:<<=>',
        '&infix:<<>',
        '&infix:<->',
        '&infix:<+>',
    ),

    '&infix:<**>' => Thunk(
        Seq(
            Param('$x'),
            Param('$y'),
            App(
                Sym('&repeatedly_apply_and_accum'),
                Sym('&infix:<*>'),
                Sym('$x'),
                Sym('$x'),
                Sym('$y'),
            ),
        ),
    ),

    '&infix:<*>' => Thunk(
        Seq(
            Param('$x'),
            Param('$y'),
            App(
                Sym('&repeatedly_apply_and_accum'),
                Sym('&infix:<+>'),
                Sym('$x'),
                Sym('$x'),
                Sym('$y'),
            ),
        ),
    ),

    '&infix:</>' => Thunk(
        Seq(
            Param('$x'),
            Param('$y'),
            App(
                Sym('&control_structure:<if>'),
                App(
                    Sym('&infix:<<>'),
                    Sym('$x'),
                    Sym('$y'),
                ),
                Val(
                    Thunk(
                        Val(0),
                    )
                ),
                Val(
                    Thunk(
                        App(
                            Sym('&infix:<+>'),
                            Val(1),
                            App(
                                Sym('&infix:</>'),
                                App(
                                    Sym('&infix:<->'),
                                    Sym('$x'),
                                    Sym('$y'),
                                ),
                                Sym('$y'),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    ),

    '&repeatedly_apply_and_accum' => Thunk(
        Seq(
            Param('&f'),
            Param('$accum'),
            Param('$x'),
            Param('$y'),
            App(
                Sym('&control_structure:<if>'),
                App(
                    Sym('&infix:<==>'),
                    Sym('$y'),
                    Val(1),
                ),
                Val(
                    Thunk(
                        Sym('$accum'),
                    ),
                ),
                Val(
                    Thunk(
                        App(
                            Sym('&repeatedly_apply_and_accum'),
                            Sym('&f'),
                            App(
                                Sym('&f'),
                                Sym('$accum'),
                                Sym('$x'),
                            ),
                            Sym('$x'),
                            App(
                                Sym('&infix:<->'),
                                Sym('$y'),
                                Val(1),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    ),
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Prelude::Math - Math related builtins

=head1 SYNOPSIS

    use Blondie::Prelude::Math;

=head1 DESCRIPTION

=cut


