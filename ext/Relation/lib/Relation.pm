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
    has Set of Str $!heading;
        # Set of Str
        # Each Set member is a name of one of this Relation's attributes.
        # Note that it is valid for a Relation to have zero attributes.
    has Set of Mapping(Str) of Any $!body;
        # Set of Mapping(Str) of Any
        # Each Set member is a tuple of this Relation, where the tuple
        # is represented as a Mapping; each key of that Mapping is the name
        # of one of this Relation's attributes and the value corresponding
        # to that is the value for that attribute for that tuple.
        # Note that it is valid for a Relation to have zero tuples.

###########################################################################

submethod BUILD (Set of Str :$heading? = set(),
        Set of Mapping(Str) of Any :$body? = set()) {

    for $body.values -> $tuple {
        die "The keys of a member of arg :$body? do not match the members"
                ~ " of arg :$heading?; the header of a tuple in :$body?"
                ~ " does not match the header of this relation."
            if !(all( $tuple.keys ) === $heading);
    }

    $!heading = $heading;
    $!body    = $body;

    return;
}

###########################################################################

method export_as_hash () returns Hash {
    return {
        'heading' => $!heading,
        'body'    => $!body,
    };
}

###########################################################################

method heading () returns Set of Str {
    return $!heading;
}

method body () returns Set of Mapping(Str) of Any {
    return $!body;
}

method size () returns Int {
    return +$!body.values;
}

method exists (Mapping(Str) of Any $tuple!) returns Bool {
    return $tuple === any($!body);
}

###########################################################################

method equal (Relation $other!) returns Bool {
    return $other!heading === $?SELF!heading
        and $other!body === $?SELF!body;
}

method not_equal (Relation $other!) returns Bool {
    return !$?SELF.equal( $other );
}

###########################################################################

method rename (Mapping(Str) of Str $mapping!) returns Relation {

    die "Some keys of $mapping! do not match this relation's attributes."
        if !(all( $mapping.keys ) === any( $!heading ));
    die "Some values of $mapping! duplicate each other."
        if +all( $mapping.values ) != +$mapping.values;
    die "Some values of $mapping! duplicate attribute names of this"
            ~ " relation that aren't being renamed."
        if any( $mapping.values )
            === any( $!heading.difference( all( $mapping.keys ) ) );

    my %full_map = { $!heading.values.map:{ $_ => $_ }, $mapping.pairs };

    return Relation.new(
        heading => set( %full_map.values ),
        body => set( $!body.values.map:{
            mapping( $_.pairs.map:{ %full_map{$_.key} => $_.value } )
        }),
    );
}

###########################################################################

method project (Set of Str $attrs!) returns Relation {

    if ($attrs.size() == 0) {
        return Relation.new( heading => set(), body => set( mapping() ) );
    }

    if ($attrs === $!heading) {
        return $?SELF;
    }

    die "Some members of $attrs! do not match this relation's attributes."
        if !(all( $attrs ) === any( $!heading ));

    return Relation.new(
        heading => $attrs,
        body => set( $!body.values.map:{
            mapping( $_.pairs.grep:{ $_.key === any( $attrs ) } )
        }),
    );
}

&select ::= &project;

method project_all_but (Set of Str $attrs!) returns Relation {
    return $?SELF.project( $!heading.difference( $attrs ) );
}

&select_all_but ::= &project_all_but;

###########################################################################

method extend (Mapping(Str) of Code $attrs!) returns Relation {

    if ($attrs.size() == 0) {
        return $?SELF;
    }

    die "Some keys of $attrs! duplicate attribute names of this relation."
        if any( $attrs ) === any( $!heading );

    return Relation.new(
        heading => $!heading.union( set( $attrs.keys ) ),
        body => set( $!body.values.map:{
            mapping( $_.pairs,
                $attrs.pairs.map:{ $_.key => $_.value.( CALLER::<$_> ) } )
        }),
    );
}

###########################################################################

method map (Set of Str $heading!, Code $replacements!) returns Relation {
    return Relation.new(
        heading => $heading,
        body => set( $!body.values.map:{ $replacements.( $_ ) }),
    );
}

###########################################################################

method restrict (Code $predicate!) returns Relation {
    return Relation.new(
        heading => $!heading,
        body => set( $!body.values.grep:{ $predicate.( $_ ) }),
    );
}

&where ::= &restrict;
&grep  ::= &restrict;

method delete (Code $predicate!) returns Relation {
    return Relation.new(
        heading => $!heading,
        body => set( $!body.values.grep:{ !$predicate( $_ ) }),
    );
}

&delete_where ::= &delete;

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
"relation" of logic and mathematics and philosophy ("a predicate ranging
over more than one argument"), which is also the basis of the relational
data model proposed by Edgar. F. Codd, upon which anything in the world can
be modelled.

A relation is essentially a set of mappings, or a set of logical tuples; a
picture of one can look like a table, where each tuple is a row and each
relation/tuple attribute is a column, but it is not the same as a table.

The intended interface and use of this class in Perl programs is similar to
the intended use of a L<Set> class; a Relation is like a Set that exists
over N dimensions rather than one.  The Relation operators are somewhat of
a superset of the Set operators.

Like the Set data type, the Relation data type is immutable.  The value of
a Relation object is determined when it is constructed, and the object can
not be changed afterwards.

If you want something similar but that is more mutable, you can accomplish
that manually using a set of mappings, or a multi-dimensional object Hash,
or various combinations of other data types.

While the implementation can be changed greatly (it isn't what's important;
the interface/behaviour is), this Relation data type is proposed to be
generally useful, and very basic, and worthy of inclusion in the Perl 6
core, at least as worthy as a Set data type is.

=head1 INTERFACE

The interface of Relation is entirely object-oriented; you use it by
creating objects from its member classes, usually invoking C<new()> on the
appropriate class name, and then invoking methods on those objects.  All of
their class/object attributes are private, so you must use accessor
methods.  Relation does not declare any subroutines or export such.

The usual way that Relation indicates a failure is to throw an exception;
most often this is due to invalid input.  If an invoked routine simply
returns, you can assume that it has succeeded.

=head2 The Relation Class

A Relation object is an unordered set of tuples, each of which is an
unordered set of named attributes; all tuples in a Relation are of the same
degree, and the attribute names of each tuple are all the same as those of
all the other tuples.

For purposes of the Relation class' API, a tuple is represented by a Perl
Mapping where each Mapping key is an attribute name and each Mapping value
is the corresponding attribute value.

Every Relation attribute has a name that is distinct from the other
attributes, though several attributes may store values of the same class;
every Relation tuple must be distinct from the other tuples.  All Relation
attributes may be individually addressed only by their names, and all
Relation tuples may be individually addressed only by their values; neither
may be addressed by any ordinal value.

Note that it is valid for a Relation to have zero tuples.  It is also valid
for a Relation to have zero attributes, though such a Relation can have no
more than a single tuple.  A zero-attribute Relation with zero tuples or
one tuple respectively have a special meaning in relational algebra which
is analagous to what the identity numbers 0 and 1 mean to normal algebra.

A picture of a Relation can look like a table, where each of its tuples is
a row, and each attribute is a column, but a Relation is not a table.

The Relation class is pure and deterministic, such that all of its class
and object methods will each return the same result when invoked on the
same object with the same arguments; they do not interact with the outside
environment at all.

A Relation object has 2 main attributes (implementation details subject to
change):

=over

=item C<$!heading> - B<Relation Heading>

Set of Str - This contains zero or more Relation attribute names that
define the heading of this Relation.  Each attribute name is a character
string.

=item C<$!body> - B<Relation Body>

Set of Mapping(Str) of Any - This contains zero or more member tuples of
the Relation; each Set member is a Mapping whose keys and values are
attribute names and values.  Each Mapping key of a Body tuple must match a
Set member of Heading, and the value of every tuple in Body must be
mutually distinct.

=back

This is the main Relation constructor method:

=over

=item C<new (Set of Str :$heading?, Set of Mapping(Str) of Any :$body?)>

This method creates and returns a new Relation object, whose Heading and
Body attributes are set respectively from the optional named parameters
C<$heading> and C<$body>.  If C<$heading> is undefined or an empty Set, the
Relation has zero attributes.  If C<$body> is undefined or an empty Set,
the Relation has zero tuples.  If a Relation has zero attributes, then
C<$body> may be an Set with a single member that is an empty Mapping.

=back

A Relation object has these methods:

=over

=item C<export_as_hash () returns Hash>

This method returns a deep copy of this Relation as a Hash ref of 2
elements, which correspond to the 2 named parameters of C<new>.

=item C<heading () returns Set of Str>

This method returns this Relation's heading.

=item C<body () returns Set of Mapping(Str) of Any>

This method returns this Relation's body.

=item C<size () returns Int>

This method returns a count of this Relation's member tuples as an Int.

=item C<exists (Mapping(Str) of Any $tuple!) returns Bool>

This method returns a Bool indicating whether the argument C<$tuple> exists
in / is a member of this Relation.

=item C<equal (Relation $other!) returns Bool>

This method returns a Bool indicating whether the immutable identity of the
argument C<$other> equals the immutable identity of the invocant.

=item C<not_equal (Relation $other!) returns Bool>

This method returns the complement of C<equal> with the same argument.

=item C<rename ( Mapping(Str) of Str $mapping! ) returns Relation>

This method is a generic relational operator that returns a new Relation
which is the same as the invocant Relation but that some of its attributes
are renamed.  The argument C<$mapping> says which attributes are being
renamed, with its keys being the old names and its values the new names.
This method will fail if any C<$mapping> keys do not match invocant
Relation attribute names, or any C<$mapping> values duplicate each other,
or duplicate attribute names that aren't being renamed.  This method
supports renaming attributes to each others' names.

=item C<project (Set of Str $attrs!) returns Relation>

This method is a generic relational operator that returns a new Relation
which has a subset of the original's attributes; that subset is the same as
those attribute names in the argument C<$attrs>.  The new Relation has all
of the tuples of the original (or rather, the corresponding projection of
each tuple), but that any duplicates following the projection have been
eliminated.  Trivial cases are where C<$attrs> is either empty or equal to
the invocant Relation's header, in which case it returns the identity-one
Relation or the invocant Relation respectively.  This method will fail if
any members of C<$attrs> do not match attribute names of the invocant
Relation.  This method has an alias named C<select>.

=item C<project_all_but (Set of Str $attrs!) returns Relation>

This method is the same as C<project> but that the returned Relation has
the complement subset of the original's attributes to what C<project> would
return given the same C<$attrs>.  This method has an alias named
C<select_all_but>.

=item C<extend (Mapping(Str) of Code $attrs!) returns Relation>

This method is a generic relational operator that returns a new Relation
which has a superset of the original's attributes; the new Relation has as
many additional attributes as there are pairs in the argument C<$attrs>,
where the keys of C<$attrs> provide the names of the new attributes, and
the values provide values applied to each tuple.  The new Relation has all
of the tuples of the original (or rather, the corresponding extension of
each tuple).  Each value of C<$attrs> is an anonymous function that, when
given each original tuple/Mapping as its sole read-only argument, returns a
value that is optionally a calculation using original attribute values as
inputs.  As a trivial case, if C<$attrs> is empty, the output relation is
identical to the invocant.  This method will fail if any keys of C<$attrs>
duplicate attribute names of the invocant Relation.

=item C<map (Set of Str $heading!, Code $transformer!) returns Relation>

This method is a short-hand or alternative for the functionality provided
by the 3 generic relational operators [C<extend>, C<project>, C<rename>],
applied in that order, and it works like Perl's standard C<map> operator.
It returns a new Relation whose attributes (provided in C<$heading>) may or
may not resemble those of the original.  The anonymous function in the
argument C<$transformer> is invoked for each original tuple/Mapping in
turn, which it gets as its sole read-only argument, and it must return a
new tuple/Mapping whose attributes match those of C<$heading>.  The new
Relation has all of the tuples of the original (or rather, the
corresponding transformation of each tuple), but that any duplicates
following the transformation have been eliminated.  Trivial cases are where
C<$transformer> returns either an empty tuple/Mapping or a tuple that is
identical to the original, in which case this method returns the
identity-one Relation or the invocant relation, respectively.

=item C<restrict (Code $predicate!) returns Relation>

This method is a generic relational operator that returns a new Relation
which has a subset of the original's tuples, and it works like Perl's
standard C<grep> operator; that subset is those for which the the
Bool-returning anonymous function in the argument C<$predicate> returns
True when given the tuple/Mapping as its sole read-only argument. The new
Relation has all of the same attributes as the original.  Trivial cases are
where C<$predicate> simply returns True or False regardless of its
argument, in which case the new Relation has either all of the tuples of
the original, or has zero tuples, respectively.  This method has aliases
named C<where> and C<grep>.

=item C<delete (Code $predicate!) returns Relation>

This method is the same as C<restrict> but that the returned Relation has
the complement subset of the original's tuples to what C<restrict> would
return given the same C<$predicate>.  This method has an alias named
C<delete_where>.  This method is conceptually the same as the C<delete>
statement of SQL, but that it isn't a mutator; to have the same affect as
SQL, you say C<$r = $r.delete_where( $predicate );>.

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

Relation is Copyright (c) 2006, Darren R. Duncan.

Relation is free software; you can redistribute it and/or modify it under
the same terms as Perl 6 itself.

=head1 ACKNOWLEDGEMENTS

None yet.

=cut
