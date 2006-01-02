#!/usr/bin/pugs
use v6;

# External packages used by packages in this file, that don't export symbols:
use Locale::KeyedText-(1.72.0...);

###########################################################################
###########################################################################

# Constant values used by packages in this file:
my Str $EMPTY_STR is readonly = q{};

###########################################################################
###########################################################################

package SQL::Routine-0.710.0 {
    # Note: This given version applies to all of this file's packages.
} # package SQL::Routine

###########################################################################
###########################################################################

class SQL::Routine::Document {

    # External packages used by the SQL::Routine::Document class, that do export symbols:
    # (None Yet)

    # Attributes of every SQL::Routine::Document object:
    has SQL::Routine::Node @!all_nodes;
    # Array of SQL::Routine::Node
    # The set of all Nodes that this Document contains.
    has SQL::Routine::Node @!root_nodes;
    # Array of SQL::Routine::Node
    # List of all Nodes that have no parent Nodes; a sub-set of all_nodes.

    # Entrust the SQL::Routine::Node class to see our private attributes.
    trusts SQL::Routine::Node;

###########################################################################

submethod BUILD (Hash :@root_nodes? = []) {

    $?SELF!_assert_arg_rt_nd_aoh( 'new', ':@root_nodes?', @root_nodes );

    @!all_nodes  = [];
    @!root_nodes = [];
    for @root_nodes -> $root_node {
        SQL::Routine::Node.new( 'document' => $?SELF, *%{$root_node} );
    }

    return;
}

###########################################################################

method export_as_hash () returns Hash {
    return {
        'root_nodes' => [@!root_nodes.map:{ .export_as_hash() }],
    };
}

###########################################################################

my method _assert_arg_rt_nd_aoh (Str $meth!, Str $arg!, Str @val!) {
    $?SELF!_die_with_msg( 'LKT_ARG_UNDEF',
            { 'METH' => $meth, 'ARG' => $arg } )
        if !@val.defined;
    for @val -> $val_elem {
        $?SELF!_die_with_msg( 'LKT_ARG_ARY_ELEM_UNDEF',
                { 'METH' => $meth, 'ARG' => $arg } )
            if !$val_elem.defined;
        $?SELF!_die_with_msg( 'LKT_ARG_ARY_ELEM_NO_HASH',
                { 'METH' => $meth, 'ARG' => $arg, 'VAL' => $val_elem } )
            if !$val_elem.does(Hash);
        for ('document', 'parent_node') -> $k {
            $?SELF!_die_with_msg(
                    'SRT_D_ARG_AOH_TO_CONSTR_CH_ND_HAS_KEY_CONFL',
                    { 'METH' => $meth, 'ARG' => $arg, 'KEY' => $k } )
                if $val_elem.exists($k);
        }
    }
}

###########################################################################

} # class SQL::Routine::Document

###########################################################################
###########################################################################

class SQL::Routine::Node {

    # External packages used by the SQL::Routine::Node class, that do export symbols:
    # (None Yet)

    # Attributes of every SQL::Routine::Node object:
    has SQL::Routine::Document $!document;
    # SQL::Routine::Document
    # The Document that this Node lives in.
    has SQL::Routine::Node     $!parent_node;
    # SQL::Routine::Node
    # The parent Node of this Node, if there is one.
    has Str                    $!node_type;
    # Str
    # What type of Node this is.
    has Any                    %!attributes;
    # Hash(Str) of Any
    # Named attribute values that this Node has, if any.
    has SQL::Routine::Node     @!child_nodes;
    # Array of SQL::Routine::Node
    # List of this Node's child Nodes, if there are any.

###########################################################################

submethod BUILD (
            SQL::Routine::Document :$document!,
            SQL::Routine::Node     :$parent_node? = undef,
            Str                    :$node_type!,
            Any                    :%attributes?  = {},
            Hash                   :@child_nodes? = [],
        ) {

    $?SELF!_assert_arg_doc( 'new', ':$document!', $document );
    if ($parent_node.defined) {
        $?SELF!_assert_arg_node_assume_def(
            'new', ':$parent_node?', $parent_node );
    }
    $?SELF!_assert_arg_str( 'new', ':$node_type!', $node_type );
    $?SELF!_assert_arg_hash( 'new', ':%attributes?', %attributes );
    $?SELF!_assert_arg_ch_nd_aoh( 'new', ':@child_nodes?', @child_nodes );

    $!document = $document;
    $document!all_nodes.push( $?SELF );
    if ($parent_node.defined) {
        $!parent_node = $parent_node;
        $parent_node!child_nodes.push( $?SELF );
    }
    else {
        $document!root_nodes.push( $?SELF );
    }
    $!node_type   = $node_type;
    %!attributes  = %attributes;
    @!child_nodes = [];
    for @child_nodes -> $child_node {
        $?CLASS.new(
            'document'    => $document,
            'parent_node' => $?SELF,
            *%{$child_node},
        );
    }

    return;
}

###########################################################################

method export_as_hash () returns Hash {
    return {
        'node_type'   => $!node_type,
        'attributes'  => {%!attributes},
        'child_nodes' => [@!child_nodes.map:{ .export_as_hash() }],
    };
}

###########################################################################

my method _die_with_msg (Str $msg_key!, Any %msg_vars? is ref = {}) {
    %msg_vars{'CLASS'} = 'SQL::Routine::Node';
    die Locale::KeyedText::Message.new(
        'msg_key' => $msg_key, 'msg_vars' => %msg_vars );
}

my method _assert_arg_str (Str $meth!, Str $arg!, Str $val!) {
    $?SELF!_die_with_msg( 'LKT_ARG_UNDEF',
            { 'METH' => $meth, 'ARG' => $arg } )
        if !$val.defined;
    $?SELF!_die_with_msg( 'LKT_ARG_EMP_STR',
            { 'METH' => $meth, 'ARG' => $arg } )
        if $val eq $EMPTY_STR;
}

my method _assert_arg_hash (Str $meth!, Str $arg!, Any %val!) {
    $?SELF!_die_with_msg( 'LKT_ARG_UNDEF',
            { 'METH' => $meth, 'ARG' => $arg } )
        if !%val.defined;
    $?SELF!_die_with_msg( 'LKT_ARG_HASH_KEY_EMP_STR',
            { 'METH' => $meth, 'ARG' => $arg } )
        if %val.exists($EMPTY_STR);
}

my method _assert_arg_doc (Str $meth!, Str $arg!, $val!) {
    $?SELF!_die_with_msg( 'LKT_ARG_UNDEF',
            { 'METH' => $meth, 'ARG' => $arg } )
        if !$val.defined;
    $?SELF!_die_with_msg( 'LKT_ARG_NO_EXP_TYPE', { 'METH' => $meth,
            'ARG' => $arg, 'EXP_TYPE' => 'SQL::Routine::Document',
            'VAL' => $val } )
        if !$val.does(SQL::Routine::Document);
}

my method _assert_arg_node_assume_def (Str $meth!, Str $arg!, $val!) {
    $?SELF!_die_with_msg( 'LKT_ARG_NO_EXP_TYPE', { 'METH' => $meth,
            'ARG' => $arg, 'EXP_TYPE' => 'SQL::Routine::Node',
            'VAL' => $val } )
        if !$val.does(SQL::Routine::Node);
}

my method _assert_arg_ch_nd_aoh (Str $meth!, Str $arg!, Str @val!) {
    $?SELF!_die_with_msg( 'LKT_ARG_UNDEF',
            { 'METH' => $meth, 'ARG' => $arg } )
        if !@val.defined;
    for @val -> $val_elem {
        $?SELF!_die_with_msg( 'LKT_ARG_ARY_ELEM_UNDEF',
                { 'METH' => $meth, 'ARG' => $arg } )
            if !$val_elem.defined;
        $?SELF!_die_with_msg( 'LKT_ARG_ARY_ELEM_NO_HASH',
                { 'METH' => $meth, 'ARG' => $arg, 'VAL' => $val_elem } )
            if !$val_elem.does(Hash);
        for ('document', 'parent_node') -> $k {
            $?SELF!_die_with_msg(
                    'SRT_N_ARG_AOH_TO_CONSTR_CH_ND_HAS_KEY_CONFL',
                    { 'METH' => $meth, 'ARG' => $arg, 'KEY' => $k } )
                if $val_elem.exists($k);
        }
    }
}

###########################################################################

} # class SQL::Routine::Node

###########################################################################
###########################################################################

=pod

=head1 NAME

SQL::Routine -
Specify all database tasks with SQL routines

=head1 VERSION

This document describes SQL::Routine version 0.710.0.

It also describes the same-number versions of SQL::Routine::Document
("Document") and SQL::Routine::Node ("Node").

I<Note that the "SQL::Routine" package serves only as the name-sake
representative for this whole file, which can be referenced as a unit by
documentation or 'use' statements or Perl archive indexes.  Aside from
'use' statements, you should never refer directly to "SQL::Routine" in your
code; instead refer to other above-named packages in this file.>

=head1 SYNOPSIS

I<This documentation is pending.>

=head1 DESCRIPTION

SQL::Routine provides an effective language for defining relational data
models, both the means to create them and the means to interact with them.
The language loosely resembles the ANSI/ISO SQL:2003 standard in purpose
and structure, but its details are different.  This is partly so that it
can more elegantly support the specific relational model that E. F. Codd
proposed in his 1970 publication titled "A Relational Model of Data for
Large Shared Data Banks", but that SQL diverged from in some ways.
Regardless, it should be easy to translate database definitions and queries
between SQL and the language SQL::Routine provides.

Please see the pod-only file L<SQL::Routine::Language> ("Language"), which
is the human readable authoritative design document for SQL::Routine's
language; the file SQL::Routine itself is a machine readable language
specification that is derived from the human readable version, and in the
case of a conflict, Language takes precedence.

SQL::Routine is implemented as abstract syntax trees, and you use it by
creating, manipulating, and reading nodes in these trees.  Each tree node
is atomic, so you can just build the trees by copying scalar values from a
data dictionary; no stitching or parsing more complicated command strings
is necessary like with SQL.

L<Rosetta> (distributed separately) is a relational database access
solution that uses SQL::Routine objects as its native instruction set
rather than SQL strings.  But SQL::Routine can also be used independently
of Rosetta, such as when translating SQL from one dialect to another.

=head1 INTERFACE

The interface of SQL::Routine is entirely object-oriented; you use it by
creating objects from its member classes, usually invoking C<new()> on the
appropriate class name, and then invoking methods on those objects.  All of
their attributes are private, so you must use accessor methods.
SQL::Routine does not declare any subroutines or export such.

The usual way that SQL::Routine indicates a failure is to throw an
exception; most often this is due to invalid input.  If an invoked routine
simply returns, you can assume that it has succeeded, even if the return
value is undefined.

SQL::Routine's input validation is performed over 2 main phases, which are
referred to as "immediate" and "deferred".  The immediate validations are
performed at the moment the user tries to set the input, and input that
fails immediate evaluation will not be set at all.  The scope of immediate
validation is kept to the minimum possible, and is essentially just
concerned with the well-formedness of the input, such as that mandatory
constructor arguments are provided and that they are of the correct
container type (eg, hash vs array).  The deferred validations are performed
on demand at some time after the input has been set, and could potentially
never be performed at all.  They validate everything except
well-formedness, such as that SQL::Routine Nodes are arranged correctly
depending on their types, that their attributes have reasonable values, and
that attributes or Nodes are not missing.  The deferred validations, which
can be arbitrarily complex, make up the bulk of the SQL::Routine code, and
these could potentially be extended by third party add-ons.

=head2 The SQL::Routine::Document Class

A Document object is a simple container which stores data to be used or
displayed by your program.  It is analagous to a simplified version of the
"Document" interface defined in the XML DOM spec; it exists as a container
in which Node objects live.  The Document class is pure and deterministic,
such that all of its class and object methods will each return the same
result and/or make the same change to an object when the permutation of its
arguments and any invocant object's attributes is identical; they do not
interact with the outside environment at all.

A Document object has 2 main attributes:

=over

=item C<@!all_nodes> - B<All Nodes>

Array of SQL::Routine::Node - This stores a collection of Node objects,
which are all of the Nodes that live in this Document.

=item C<@!root_nodes> - B<Root Nodes>

Array of SQL::Routine::Node - This stores an ordered list of all this
Document's Node objects that do not have parent Nodes of their own; it is a
sub-set of All Nodes.

=back

This is the main Document constructor method:

=over

=item C<new( :@root_nodes? )>

This method creates and returns a new SQL::Routine::Document object.  If
the optional named argument @root_nodes (an array ref) is set, then each
element in it is used to initialize a new Node object (plus an optional
hierarchy of new child Nodes of that new Node) that gets stored in the Root
Nodes attribute (that attribute defaults to empty if the argument is
undefined).  The new Document object's All Nodes attribute starts out
empty, but gains one element for each Node in the Node hierarchies created
from a defined @root_nodes.

Some sample usage:

    my SQL::Routine::Document $document .= new();

    my SQL::Routine::Document $document2 .= new(
        'root_nodes' => [
            {
                'node_type'  => 'data_sub_type',
                'attributes' => { 'predef_base_type' => 'BOOLEAN' },
            },
        ],
    );

=back

A Document object has these methods:

=over

=item C<export_as_hash()>

This method returns a deep copy of all of this Document's member Nodes as a
tree of primitive Perl data structures (hash refs, array refs, scalars),
which is suitable as a generic data interchange format between SQL::Routine
and other Perl code such as persistence solutions.  Moreover, these data
structures are in exactly the right input format for Document.new(), by
which you can create an identical Document (with member Nodes) to the one
you first invoked export_as_hash() on.

Specifically, export_as_hash() returns a Perl hash ref whose key list
('root_nodes') corresponds exactly to the named arguments of
Document.new(); you can produce a direct clone like this:

    my $cloned_doc = SQL::Routine::Document.new(
        *%{$original_doc.export_as_hash()} );

Or, to demonstrate the use of a persistence solution:

    # When saving.
    my $hash_to_save = $original_doc.export_as_hash();
    MyPersist.put( $hash_to_save );

    # When restoring.
    my $hash_was_saved = MyPersist.get();
    my $cloned_doc = SQL::Routine::Document.new( *%{$hash_was_saved} );

=back

=head2 The SQL::Routine::Node Class

A Node object is a simple container which stores data to be used or
displayed by your program.  It is analagous to a simplified version of the
"Node" interface defined in the XML DOM spec; it has a type (what the XML
spec calls 'name') and attributes, and is arranged in a hierarchy with
other Nodes (it does not support any analogy of CDATA sections, though).
The Node class is pure and deterministic, such that all of its class and
object methods will each return the same result and/or make the same change
to an object when the permutation of its arguments and any invocant
object's attributes is identical; they do not interact with the outside
environment at all.

A Node object has 5 main attributes:

=over

=item C<$!document> - B<Document>

SQL::Routine::Document - This stores a reference to the Document that this
Node and all of its relative Nodes live in.

=item C<$!parent_node> - B<Parent Node>

SQL::Routine::Node - This stores a reference to this Node's parent Node, if
it has one.  Nodes that do not have this attribute set are considered "root
Nodes" and are listed in their Document's Root Nodes property; in a manner
of speaking, the Document itself is the parent of such Nodes.

=item C<$!node_type> - B<Node Type>

Str - This string identifies what kind of Node this is, and by extension
what kinds of attributes or relative Nodes it can have, as well as where
and how the Node can be used.

=item C<%!attributes> - B<Attributes>

Hash(Str) of Any - This contains zero or more attribute names and values
that help to define this Node.

=item C<@!child_nodes> - B<Child Nodes>

Array of SQL::Routine::Node - This stores an ordered list of all this
Node's child Nodes, if it has any.

=back

This is the main Node constructor method:

=over

=item C<new( :$document!, :$parent_node?, :$node_type!, :%attributes?,
:@child_nodes? )>

This method creates and returns a new SQL::Routine::Node object, that lives
in the Document object given in the named argument $document, and whose
Node Type attribute is set from the named argument $node_type (a string);
the optional named argument %attributes (a hash ref) sets the "Attributes"
attribute if provided (it defaults to empty if the argument is not
provided).  If the optional argument $parent_node (a Node) is set, then the
new Node's "Parent Node" attribute is set to it, and the new Node is also
stored in $parent_node's Child Nodes attribute; if $parent_node is not set,
then the new Node is instead stored in its Document's "Root Nodes"
attribute.  If the optional named argument @child_nodes (an array ref) is
set, then each element in it is used to initialize a new Node object (plus
an optional hierarchy of new child Nodes of that new Node) that gets stored
in the Child Nodes attribute (that attribute defaults to empty if the
argument is undefined).

Some sample usage:

    # Declare a unsigned 32-bit integer data type.
    my SQL::Routine::Node $dt_uint32 .= new(
        'node_type'  => 'data_sub_type',
        'attributes' => {
            'predef_base_type' => 'NUMERIC',
            'num_precision'    => 2**32,
            'num_scale'        => 1,
            'num_min_value'    => 0,
        },
    );

    # Declare an enumerated ('F','M') character value data type.
    my SQL::Routine::Node $dt_sex .= new(
        'node_type'   => 'data_sub_type',
        'attributes'  => {
            'predef_base_type' => 'CHAR_STR',
            'char_max_length'  => 1,
            'char_repertoire'  => 'UNICODE',
        },
        'child_nodes' => [
            {
                'node_type'  => 'data_sub_type_value',
                'attributes' => { 'value' => 'F' },
            },
            {
                'node_type'  => 'data_sub_type_value',
                'attributes' => { 'value' => 'M' },
            },
        ],
    );

=back

A Node object has these methods:

=over

=item C<export_as_hash()>

This method returns a deep copy of this Node plus all of its descendent
Nodes (if any) as a tree of primitive Perl data structures (hash refs,
array refs, scalars), which is suitable as a generic data interchange
format between SQL::Routine and other Perl code such as persistence
solutions.  Moreover, these data structures are in exactly the right input
format for Node.new(), by which you can create an identical Node (with
child Nodes) to the one you first invoked export_as_hash() on.

Specifically, export_as_hash() returns a Perl hash ref whose key list
('node_type', 'attributes', 'child_nodes') corresponds exactly to the named
arguments of Node.new(); you need to supply its $document and optional
$parent_node though; you can produce a direct clone like this:

    my $cloned_node = SQL::Routine::Document.new(
        'document' => $document, *%{$original_node.export_as_hash()} );

Or, to demonstrate the use of a persistence solution:

    # When saving.
    my $hash_to_save = $original_node.export_as_hash();
    MyPersist.put( $hash_to_save );

    # When restoring.
    my $hash_was_saved = MyPersist.get();
    my $cloned_node = SQL::Routine::Document.new(
        'document' => $document, *%{$hash_was_saved} );

=back

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 6.x.y that is at least 6.0.0.

It also requires these Perl 6 classes that are on CPAN:
L<Locale::KeyedText-(1.72.0...)|Locale::KeyedText> (for error messages).

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

These documentation files are included in the SQL::Routine distribution:
L<SQL::Routine::Language>, L<SQL::Routine::Migration>.

These Perl 6 packages are the initial main dependents of SQL::Routine:
L<Rosetta>, L<SQL::Routine::SQLBuilder>, L<SQL::Routine::SQLParser>.

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHOR

Darren R. Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the SQL::Routine database portability library.

SQL::Routine is Copyright (c) 2002-2005, Darren R. Duncan.  All rights
reserved. Address comments, suggestions, and bug reports to
C<perl@DarrenDuncan.net>, or visit L<http://www.DarrenDuncan.net/> for more
information.

SQL::Routine is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License (GPL) as published by the
Free Software Foundation (L<http://www.fsf.org/>); either version 2 of the
License, or (at your option) any later version.  You should have received a
copy of the GPL as part of the SQL::Routine distribution, in the file named
"GPL"; if not, write to the Free Software Foundation, Inc., 51 Franklin St,
Fifth Floor, Boston, MA 02110-1301, USA.

Linking SQL::Routine statically or dynamically with other files is making
a combined work based on SQL::Routine.  Thus, the terms and conditions of
the GPL cover the whole combination.  As a special exception, the copyright
holders of SQL::Routine give you permission to link SQL::Routine with
independent files, regardless of the license terms of these independent
files, and to copy and distribute the resulting combined work under terms
of your choice, provided that every copy of the combined work is
accompanied by a complete copy of the source code of SQL::Routine (the
version of SQL::Routine used to produce the combined work), being
distributed under the terms of the GPL plus this exception.  An independent
file is a file which is not derived from or based on SQL::Routine, and
which is fully useable when not linked to SQL::Routine in any form.

Any versions of SQL::Routine that you modify and distribute must carry
prominent notices stating that you changed the files and the date of any
changes, in addition to preserving this original copyright notice and other
credits. SQL::Routine is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

While it is by no means required, the copyright holders of SQL::Routine
would appreciate being informed any time you create a modified version of
SQL::Routine that you are willing to distribute, because that is a
practical way of suggesting improvements to the standard version.

=head1 ACKNOWLEDGEMENTS

None yet.

=cut
