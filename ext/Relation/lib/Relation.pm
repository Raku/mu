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
    has Role %!attrs;
        # Hash(Str) of Role
        # Each hash key is a name of one of this Relation's attributes; the
        # corresponding hash value is the role that the class of each value
        # of that Relation attribute must implement.
        # Note that it is valid for a Relation to have zero attributes.
    has Hash @!tuples;
        # Array of Hash(Str) of Any
        # Each array element is a tuple of this Relation, where the tuple
        # is represented as a hash; each key of that hash is the name of
        # one of this Relation's attributes and the value corresponding to
        # that is the value for that attribute for that tuple.
        # Note that it is valid for a Relation to have zero tuples.
        # Note that while an array is used here, it is meant to represent a
        # set of tuples, so its elements are conceptually not in any order.

###########################################################################

submethod BUILD (Role :%attrs? = {}, Hash :@tuples? = []) {

    die "Arg :%attrs has the empty string for a key."
        if %attrs.exists($EMPTY_STR);
    %!attrs = {%attrs};

    for @tuples -> %tuple {
        die "An element of arg :@tuples has an element count that does not"
                ~ "match the element count of arg :%attrs."
            if +%tuple.keys != +%attrs.keys;
        die "An element of arg :@tuples has the empty string for a key."
            if %tuple.exists($EMPTY_STR);
        for %tuple.kv -> $atnm, $atvl {
            die "An element of arg :@tuples has a key, '$atnm', that does"
                    ~ " not match any key of arg :%attrs."
                if !%attrs.exists($atnm);
            die "An element of arg :@tuples has a value for key '$atnm'"
                    ~ " whose implementing class, '{$atvl.ref}' does not"
                    ~ " implement the role specified for the corresponding"
                    ~ " :%attrs element, '{%attrs{$atnm}}'."
                if !$atvl.does(%attrs{$atnm});
        }
    }
    # TODO: Validate that all the given tuples are mutually distinct, and
    # throw an exception if not.  Or alternately just merge them silently.
    @!tuples = [@tuples.map:{ {$_} }];

    return;
}

###########################################################################

method export_as_hash () returns Hash {
    return {
        'attrs'  => {%!attrs},
        'tuples' => [@!tuples.map:{ {$_} }],
    };
}

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

A Relation object is an unordered set of tuples, each of which is an
unordered set of named and typed attributes; all tuples in a Relation are
of the same degree, and the attribute names of each tuple are all the same
as those of all the other tuples, and the implementing classes of their
attribute values all fulfil the same role as corresponding by their names.

For purposes of the Relation class' API, a tuple is represented by a Perl
Hash where each Hash key is an attribute name and each Hash value is the
corresponding attribute value, which knows its own implementing class.

Every Relation attribute has a name that is distinct from the other
attributes, though several attributes may store values of the same class;
every relation tuple must be distinct from the other tuples.  All Relation
attributes may be individually addressed only by their names, and all
Relation tuples may be individually addressed only by their values; neither
may be addressed by any ordinal value.

Note that it is valid for a Relation to have zero tuples.  It is also valid
for a Relation to have zero attributes, though such a Relation can have no
more than a single tuple.  A zero-attribute Relation with zero tuples or
one tuple respectively have a special meaning in relational algebra which
is analagous to what the identity numbers 0 and 1 mean to normal algebra.

A Relation can be visually represented by a table, where each of its tuples
is a row, and each attribute is a column, but a Relation is not a table.

The Relation class is pure and deterministic, such that all of its class
and object methods will each return the same result and/or make the same
change to an object when the permutation of its arguments and any invocant
object's attributes is identical; they do not interact with the outside
environment at all.

A Relation object has 2 main attributes (implementation details subject to
change):

=over

=item C<%!attrs> - B<Attributes>

Hash(Str) of Role - This contains zero or more Relation attribute names and
Role names, that together define the header of this Relation.  Each
attribute name is a non-empty character string, and each Role name must
match a possible representation / role of the class implementing a
corresponding attribute value; eg, a tuple attribute value is only
acceptable if it satisfies "<value>.does(<role>)".

=item C<@!tuples> - B<Tuples>

Array of Hash(Str) of Any - This contains zero or more member tuples of the
Relation; each array element is a Hash whose keys and values are attribute
names and values.  Each Hash key of Tuples must match a Hash key of
Attributes, and each Hash value of Tuples must satisfy the corresponding
Hash value of Attributes, and those two hashes must be of the same degree;
the values of all tuples, as seen through the roles they must implement,
must be mutually distinct.  Despite this property being implemented (for
now) with an Array, its elements are all conceptually not in any order.

=back

This is the main Relation constructor method:

=over

=item C<new( Role :%attrs?, Hash :@tuples? )>

This method creates and returns a new Relation object, whose Attributes and
Tuples attributes are set respectively from the optional named parameters
%attrs and @tuples.  If %attrs is undefined or an empty Hash, the Relation
has zero attributes.  If @tuples is undefined or an empty Array, the
Relation has zero tuples.  If a Relation has zero attributes, then @tuples
may be an Array with a single element that is an empty Hash.

=back

A Relation object has these methods:

=over

=item C<export_as_hash()>

This method returns a deep copy of this Relation as a Hash ref of 2
elements, which correspond to the 2 named parameters of new().

=back

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
