#!/usr/bin/perl

package Blondie;

use strict;
use warnings;

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie - Blondie is a dumb intermediate language and a compiler collection for
the purpose of demonstrating an approach to compilation.

=head1 SYNOPSIS

    # a program that reduces to the value 4

    use Blondie::Nodes; # for easy constructors

    my $prog = App(
        Val(
            Thunk(
                Seq(
                    Param('$x'),
                    App(
                        Sym('&infix:<+>')
                        Val(2),
                        Sym('$x'),
                    ),
                ),
            ),
        ),
        Val(2),
    ),

    use Blondie::Backend::Perl; # something to run the program
    
    my $result = Blondie::Backend::Perl->new->run($prog);

    use Blondie::Emitter::Pretty; # in case the result is an AST
    
    print Blondie::Emitter::Pretty->new->string($result); # works for all nodes


    # perl equivalent

    (sub {
        my $x = shift;
        2 + $x
    })->( 2);

=head1 DESCRIPTION

The blondie intermediate language is a language for machines and not for
humans. It has no syntax, only an abstract syntax tree, and some very simple
operations.

=head1 CONSTRUCTS

=head2 Base ADTs

All nodes as well as a few other objects are implemented as one of three ADTs.

=over 4

=item Unit

A unit is an encapsulation of a single value and a type.

C<Val> is a type of unit because it encloses one value, and has a type (C<Val>).

=item List

A list is a a node with multiple children.

C<App> is a type of value - it applies it's first child to the rest of it's
children.

=item Map

A map is like a hash - it has keys and values.

C<Env> is a map - it contains symbol to value mappings.

=back

=head2 Misc

=over 4

=item Blondie::Env

Note that this is not a Blondie::Node! You cannot execute environments, they
contain ASTs that are compiled by the compiler into a single AST.

A mapping from symbols to values:

    Blondie::Env->new(
        '&func' => Thunk(
            Stub("&func is not really implemented"),
        ),
    );

Use the default one, in L<Blondie::Prelude>, or preferably let
L<Blondie::Runtime> load it for you.

Inherits Map.

=back

=head2 Nodes

Nodes are parts of the Blondie Abstract Syntax Tree, and can be executed by a
runtime. An example runtime is L<Blondie::Backend::Perl>, which happens to be
an interpreter.

=over 4

=item Thunk

Something you thunk of but haven't yet executed.

For example:

    Val(
        Thunk(
            ...
        ),
    ),

If you reduce this AST the code inside the thunk won't be run - it must be the
first child of an App for that to happen.

Inherits Unit.

=item App

An App makes a Thunk reduce to it's value (and also prepares a dynamic scope
for it).

If we have a Thunk inside '&func' in the Env, executing this tree will invoke
'&func':

    App(
        Sym('&func'),
        Val(1),
    ),

And also place the value 1 on the parameter stack.

Inherits List.

=item Sym

Resolves a symbol into a value. Could be a global or a dynamic symbol:

    App(
        Sym('&func'), # Sym is reduced to the Thunk mapped from '&func' in
                   # the env, which is then given to the App
    ),

Inherits Unit.

=item Val

Encapsulates a perl value. Usually either a Thunk object, a number or a string. For example

    App(
        Val(
            Thunk(
                Seq(
                    Param('$x'),
                    App(
                        Sym('&infix:<+>'),
                        Val(1),
                        Sym('$x'),
                    ),
                ),
            ),
        ),
        Val(10),
    ),

Will reduce all the children of the top level App into

    ( Thunk(...), 10 )

At which point the App will put the 10 into the parameter stack and reduce the
Thunk.

Inside the Thunk the Seq will reduce it's children one by one. First Param will
introduce a new symbol, $x, taking the 10 off the parameter stack and binding
it to $x. Then the app is reduced. First it's children will be reduced -
Sym('&infix:<+>') will resolve '&infix:<+>' into the thunk that does
addition, C<< Val(1) >> will reduce to 1, and C<< Sym('$x') >> will
be reduced to 10 (as bound by the Param object).

Then the thunk for + will be reduced, with 1 and 10 in the parameter stack, the
dynamic scope stack will be popped, then the Seq will finish, the Thunk will
finish, the stacks will be popped once more, and the whole AST is now reduced
to 11.

Inherits Unit.

=item Stub

A node that makes the runtime throw a fatal error. Used for thunks that cannot
have a value:

    Env(
        '&infix:<+>' => Thunk(
            Stub(
                'Addition must be implemented as a runtime primitive.' .
                'No, Blondie will not have church integers any time soon.',
            ),
        ),

Inherits Unit.

=item Seq

Applies it's children one by one and reduces to the value of it's last child.
Reducing this:

    Seq(
        Val(10),
        Val("moose"),
    ),

will yield "moose".

Inherits List.

=item Param

Shifts a parameter from the parameter stack and binds it to a symbol.

    App(
        Val(
            Thunk(
                Seq(
                    Param('$x'),
                    Sym('$x'),
                ),
            ),
        ),
        Val(10),
    ),

The above is the implementation of the identity function, or

    sub { my $x = shift; $x }

applied to the value 10 (ultimately reducing to 10).

Inherits Unit.

=back

=cut


