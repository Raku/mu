#!/usr/bin/pugs
use v6;

# External packages used by packages in this file, that don't export symbols:
# (None Yet)

###########################################################################
###########################################################################

# Constant values used by packages in this file:
my Str $EMPTY_STR is readonly = q{};

###########################################################################
###########################################################################

class Relation-0.0.1 {

    # External packages used by the Relation class, that do export symbols:
    # (None Yet)

    # Attributes of every Relation object:
    # (None Yet)

###########################################################################


###########################################################################

} # class Relation

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Relation -
Relations for Perl 6

=head1 VERSION

This document describes Relation version 0.0.1.

=head1 SYNOPSIS

    use Relation;

I<This documentation is pending.>

=head1 DESCRIPTION

This class implements a Relation data type that corresponds to the
"relation" of logic and philosophy ("a predicate ranging over more than one
argument"), which is also the basis of the relational data model proposed
by E. F. Codd, upon which anything in the world can be modelled.

A relation is essentially a set of sets, or a set of logical tuples; one
can be represented visually by a table, where each tuple is a row and each
relation/tuple attribute is a column, but it is not the same as a table.

The intended interface and use of this class in Perl programs is similar to
the intended use of a L<Set> class; a Relation is like a Set that exists
over 2 main dimensions rather than one.  The Relation operators are
somewhat of a superset of the Set operators.

While the implementation can be changed greatly (it isn't what's important;
the interface/behaviour is), this Relation data type is proposed to be
generally useful, and very basic, and worthy of inclusion in the Perl 6
core, at least as worthy as a Set data type is.

=head1 INTERFACE

The interface of Relation is entirely object-oriented; you use it by
creating objects from its member classes, usually invoking C<new()> on the
appropriate class name, and then invoking methods on those objects.  All of
their attributes are private, so you must use accessor methods.  Relation
does not declare any subroutines or export such.

The usual way that Relation indicates a failure is to throw an exception;
most often this is due to invalid input.  If an invoked routine simply
returns, you can assume that it has succeeded.

=head2 The Relation Class

I<This documentation is pending.>

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 6.x.y that is at least 6.0.0.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

L<Set>.

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHOR

Darren R. Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the Relation library.

Relation is Copyright (c) 2006, Darren R. Duncan.  But any copyright
interest will be transferred to The Perl Foundation for portions or
derivations that become part of Perl 6 itself.

Relation is free software; you can redistribute it and/or modify it under
the same terms as Perl 6 itself.

=head1 ACKNOWLEDGEMENTS

None yet.

=cut
