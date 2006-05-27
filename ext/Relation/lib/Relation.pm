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
    return $!body.size();
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

method wrap ( Mapping(Str) of Set of Str $mapping! ) returns Relation {
    # TODO.
}

###########################################################################

method unwrap ( Set of Str $attrs! ) returns Relation {
    # TODO.
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

###########################################################################

method union (Relation *@others) returns Relation {

    for @others -> $r2 {
        die "The heading of at least one given relation in @others does"
                ~ " not match the heading of the invocant relation."
            if !($r2!heading === $!heading);
    }

    my Relation $r1 = $?SELF;

    for @others -> $r2 {
        $r1 = Relation.new(
            heading => $!heading,
            body => $r1!body.union( $r2!body ),
        );
    }

    return $r1;
}

&plus ::= &union;

###########################################################################

method exclusion (Relation *@others) returns Relation {

    for @others -> $r2 {
        die "The heading of at least one given relation in @others does"
                ~ " not match the heading of the invocant relation."
            if !($r2!heading === $!heading);
    }

    my Relation $r1 = $?SELF;

    for @others -> $r2 {
        $r1 = Relation.new(
            heading => $!heading,
            body => $r1!body.symmetric_difference( $r2!body ),
        );
    }

    return $r1;
}

&disjoint_union       ::= &exclusion;
&d_union              ::= &exclusion;
&symmetric_difference ::= &exclusion;

###########################################################################

method intersection (Relation *@others) returns Relation {

    for @others -> $r2 {
        die "The heading of at least one given relation in @others does"
                ~ " not match the heading of the invocant relation."
            if !($r2!heading === $!heading);
    }

    my Relation $r1 = $?SELF;

    for @others -> $r2 {
        $r1 = Relation.new(
            heading => $!heading,
            body => $r1!body.intersection( $r2!body ),
        );
    }

    return $r1;
}

&intersect ::= &intersection;

###########################################################################

method difference (Relation $other!) returns Relation {

    die "The heading of the relation in $other does"
            ~ " not match the heading of the invocant relation."
        if !($other!heading === $!heading);

    return Relation.new(
        heading => $!heading,
        body => $!body.difference( $other!body ),
    );
}

&minus  ::= &difference;
&except ::= &difference;

###########################################################################

method semidifference (Relation $other!) returns Relation {
    return $?SELF.difference( $?SELF.semijoin( $other ) );
}

&semiminus    ::= &semidifference;
&not_matching ::= &semidifference;

###########################################################################

method semijoin (Relation $other!) returns Relation {

    die "The heading of the relation in $other is not a full subset"
            ~ " of the heading of the invocant relation."
        if !(all( $other!heading ) === any( $!heading ));

    # First look for trivial cases that are more efficient to do different.
    if ($!body.size() == 0 or $other!body.size() == 0) {
        # Both sources have zero tuples; so does result.
        return Relation.new( heading => $!heading, body => set() );
    }
    if ($other!heading.size() == 0) {
        # Second source is identity-one tuple; result is first source.
        return $?SELF;
    }
    if ($other!heading === $!heading) {
        # Both sources have identical headings; result is src intersection.
        return $?SELF.intersection( $other );
    }

    # Now, the standard case of a semijoin.
    return Relation.new(
        heading => $!heading,
        body => set( $!body.values.grep:{
            mapping( $_.pairs.map:{
                $_.key === any( $other!heading )
            } ) === any( $other!body.values )
        } ),
    );
}

&matching ::= &semijoin;

###########################################################################

method product (Relation *@others) returns Relation {

    if (+@others == 0) {
        return $?SELF;
    }

    my Relation @sources = ($?SELF, *@others);
    while (my $r1 = @sources.shift()) {
        for @sources -> $r2 {
            die "The heading of at least one given relation in @others has"
                    ~ " attributes in common with either other relations"
                    ~ " in @others or with the invocant."
                if any( $r2!heading ) === any( $r1.heading );
        }
    }

    return $?SELF.join( @others );
}

&cartesian_product ::= &product;
&cross_product     ::= &product;
&cross_join        ::= &product;

###########################################################################

method join (Relation *@others) returns Relation {

    # First do some optimizing of the order that source relations are
    # combined, so to try and make the least expensive kinds of combining
    # occur first, and most expensive later.
    if (+@others == 0) {
        return $?SELF;
    }

    my Relation @sources = ($?SELF, *@others);
    my Relation @r_with_zero_tuples;
    my Relation @r_with_shared_attrs;
    my Relation @r_with_disjoint_attrs;
    while (my $r1 = @sources.shift()) {
        if ($r1!body.size() == 0) {
            @r_with_zero_tuples.push( $r1 );
        }
        elsif ($r1!heading.size() == 0) {
            # identity-one tuples can be discarded as they have no effect
        }
        else {
            SWITCH:
            {
                for @sources -> $r2 {
                    if (any( $r2!heading ) === any( $r1.heading )) {
                        @r_with_shared_attrs.push( $r1 );
                        last SWITCH;
                    }
                }
                @r_with_disjoint_attrs.push( $r1 );
            }
        }
    }
    @r_with_shared_attrs = @r_with_shared_attrs.sort:{
            $^b.heading.size() <=> $^a.heading.size()
        }; # sort widest to narrowest, to help get more semijoins
    @sources = (@r_with_zero_tuples, @r_with_shared_attrs,
        @r_with_disjoint_attrs);

    # TODO: more or better optimization.

    # Now do the actual combination work.
    # Start with identity-one relation and join all sources to it.
    my Relation $r1 .= new( heading => set(), body => set( mapping() ) );

    for @sources -> $r2 {
        if ($r2!body.size() == 0) {
            # Trivial case: a source has no tuples, so dest has no tuples.
            $r1 = Relation.new(
                heading => $r1!heading.union( $r2!heading ),
                body => set(),
            );
        }

        # If we get here, both sources have at least 1 tuple each.

        elsif ($r1!heading.size() == 0) {
            # Trivial case: the first source is the identity-one relation,
            # and so the result of the join is the second source.
            $r1 = $r2;
        }
        # No need to check $r2 for identity-one tuples, as they were
        # filtered from the sources earlier.

        # If we get here, both sources have at least 1 attribute and
        # at least 1 tuple each.

        elsif ($r2!heading === $r1!heading) {
            # Both sources have identical headings, so this join
            # degenerates into an intersection (a special case of join).
            $r1 = $r1.intersection( $r2 );
        }

        elsif (all( $r1!heading ) === none( $r2!heading )) {
            # Both sources have exclusive headings, so this join
            # takes the form of a cross-product.
            $r1 = Relation.new(
                heading => $r1!heading.union( $r2!heading ),
                body => set( gather {
                    for $r1!body.values -> $t1 {
                        for $r2!body.values -> $t2 {
                            take mapping( $t1.pairs, $t2.pairs );
                        }
                    }
                } ),
            );
        }

        # Both sources have overlapping non-identical headings.

        elsif (all( $r2!heading ) === any( $r1!heading )) {
            # The second source's heading is a proper subset of the
            # first source's heading, so simplify to a semijoin.
            $r1 = $r1.semijoin( $r2 );
        }

        else {
            # This form takes the form of an ordinary natural join,
            # where some source attributes are in common, and each
            # source has attributes not in the other.

            if ($r1!body.size() > $r2!body.size()) {
                # Another optimization:
                # In case it is faster for outer loop to have fewer
                # iterations rather than the inner loop having fewer.
                ($r1, $r2) = ($r2, $r1);
            }

            Set $common_h = $r1!heading.intersection( $r2!heading );
            Set $r1only_h = $r1!heading.difference( $r2!heading );
            Set $r2only_h = $r2!heading.difference( $r1!heading );

            $r1 = Relation.new(
                heading => $r1!heading.union( $r2!heading ),
                body => set( gather {
                    for $r1!body.values -> $t1 {
                        $t1common_m = mapping( $t1.pairs.map:{
                            $_.key === any( $common_h )
                        } )
                        $t1only_m = mapping( $t1.pairs.map:{
                            $_.key === any( $r1only_h )
                        } )
                        for $r2!body.values -> $t2 {
                            $t2common_m = mapping( $t2.pairs.map:{
                                $_.key === any( $common_h )
                            } )
                            if ($t2common_m === $t1common_m) {
                                $t2only_m = mapping( $t2.pairs.map:{
                                    $_.key === any( $r2only_h )
                                } )
                                take mapping(
                                    $t1only_m.pairs,
                                    $t1common_m.pairs,
                                    $t2only_m.pairs,
                                );
                            }
                        }
                    }
                } ),
            );
        }
    }

    return $r1;
}

&natural_join ::= &join;

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

=item C<wrap ( Mapping(Str) of Set of Str $mapping! ) returns Relation>

I<TODO.  This method has not yet been implemented.>

This method is a generic relational operator that returns a new Relation
which is the same as the invocant Relation but that some of its attributes
have been combined/wrapped into tuple/Mapping-valued attributes.  The
argument C<$mapping> says which attributes are being combined/wrapped, with
its keys being the new tuple-valued attribute names and each of its values
the set of old attribute names that are being wrapped.  This method will
fail if any C<$mapping> keys duplicate attribute names that aren't being
wrapped, or any C<$mapping> value set members do not match invocant
Relation attribute names, or duplicate any other value set members.  This
method supports having the new attributes having the same names as old
attributes that become wrapped, and it supports having new tuple-valued
attributes which wrap zero old attributes (they are effectively just an
extension of the relation).

=item C<unwrap ( Set of Str $attrs! ) returns Relation>

I<TODO.  This method has not yet been implemented.>

This method is a generic relational operator that returns a new Relation
which is the same as the invocant Relation but that some of its
tuple/Mapping-valued attributes have been split/unwrapped into other
attributes.  The members of the argument C<$attrs> are the names of the
invocant's attributes to split/unwrap.  This method will fail if any of the
named attributes are not tuple-valued attributes, or if any attribute names
in one unwrapped tuple-valued attribute are the same as those of another,
or are the same as the unaffected invocant's attributes.  This method
supports having the new attributes having the same names as old attributes
that were wrapped, and it supports having old tuple-valued attributes which
have zero attributes (they just vanish).

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

=item C<union (Relation *@others) returns Relation>

This method is a generic relational operator that takes any number of
Relations as its C<@others> argument, where every C<@others> member has the
same heading as the invocant Relation, and combines all of said Relations
into a new Relation that has the same heading and whose body contains all
the tuples of the source Relations, including one copy each of any
duplicated tuples.  The sequence in which any two source relations are
combined is inconsequential.  This method will fail if any relations in
C<@others> do not have identical headers to the invocant relation.  A
trivial case is where C<@others> is an empty list, in which case the
returned Relation is identical to the invocant.  This method has an alias
named C<plus>.

=item C<exclusion (Relation *@others) returns Relation>

This method is a generic relational operator that is like C<union> except
that the returned Relation contains only tuples that are in exactly one of
the source Relations.  This method has aliases named C<disjoint_union>,
C<d_union>, and C<symmetric_difference>.

=item C<intersection (Relation *@others) returns Relation>

This method is a generic relational operator that is like C<union> except
that the returned Relation contains only tuples that are in all of the
source Relations.  This method has an alias named C<intersect>.

=item C<difference (Relation $other!) returns Relation>

This method is a generic relational operator that takes a single Relation
in its C<$other> argument, where <$other> has the same heading as the
invocant Relation, and returns a new Relation that has the same heading and
whose body contains all of the tuples that are in the invocant Relation but
that aren't in C<$other>.  This method will fail if C<$other> does not have
an identical header to the invocant relation.  This method has aliases
named C<minus> and C<except>.

=item C<semidifference (Relation $other!) returns Relation>

This method is a generic relational operator that returns the complement of
C<semijoin> with the same argument.  This method has aliases named
C<semiminus> and C<not_matching>.

=item C<semijoin (Relation $other!) returns Relation>

This method is a generic relational operator that takes a single Relation
in its C<$other> argument, where the heading of <$other> is a subset of the
heading of the invocant Relation, and returns a new Relation that has the
same heading as the invocant and whose body contains all of the tuples that
are in the invocant Relation and that match any tuples of C<$other> along
their common attributes.  This method will fail if the heading of <$other>
is not a subset of the heading of the invocant Relation.  This method
degenerates into an C<intersection> if the two source headings are
identical.  This method has an alias named C<matching>.

=item C<product (Relation *@others) returns Relation>

This method is a generic relational operator that is identical to C<join>
except that this method will fail if any of its source Relations have
attributes in common; it will only accept inputs that would result in a
cross-product when joined.  This method has aliases named
C<cartesian_product>, C<cross_product>, C<cross_join>.

=item C<join (Relation *@others) returns Relation>

This method is a generic relational operator that takes any number of
Relations as its C<@others> argument and combines all of those Relations,
along with the invocant relation, into a new Relation, such that all common
attribute names and corresponding common tuple values are aligned and
merged.  The heading of the new relation is the union of all the headings
of the source relations.  The body of the new relation is the result of
first pairwise-matching every tuple of each source relation with every
tuple of each other source relation, then where each member of a tuple pair
has attribute names in common, eliminating pairs where the values of those
attributes differ and unioning the remaining said tuple pairs, then
eliminating any result tuples that duplicate others.  The sequence in which
any two source relations are combined is inconsequential.  A trivial case
is where C<@others> is an empty list, in which case the returned Relation
is identical to the invocant.  Another trivial case is where any any source
relation has zero tuples, in which case the result does too.  Another
trivial case is if any source relation is the identity-one relation (zero
attributes, one tuple), the result is as if it wasn't there at all.  In any
situation where a pair of source relations have identical headings, for
these their joining degenerates to a C<intersection>.  In any situation
where a pair of source relations have zero attributes in common, their
joining degenerates to a C<product>.  In any situation where the heading of
one source relation is a full subset of another, the result degenerates to
a C<semijoin>.  This method has an alias named C<natural_join>.

=item C<compose>

I<TODO.>

=item C<group>

I<TODO.>

=item C<ungroup>

I<TODO.>

=item C<summarize>

I<TODO.>

=item C<substitute>

I<TODO.>

=item C<divide>

I<TODO.>

=item C<transitive_closure>

I<TODO.>  Aliases: C<tclose>.

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
