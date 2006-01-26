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

package Rosetta::Model-0.720.0 {
    # Note: This given version applies to all of this file's packages.
} # package Rosetta::Model

###########################################################################
###########################################################################

class Rosetta::Model::Document {

    # External packages used by the Rosetta::Model::Document class, that do export symbols:
    # (None Yet)

    # Attributes of every Rosetta::Model::Document object:
    has Rosetta::Model::Node @!all_nodes;
    # Array of Rosetta::Model::Node
    # The set of all Nodes that this Document contains.
    has Rosetta::Model::Node @!root_nodes;
    # Array of Rosetta::Model::Node
    # List of all Nodes that have no parent Nodes; a sub-set of all_nodes.

    # Entrust the Rosetta::Model::Node class to see our private attributes.
    trusts Rosetta::Model::Node;

###########################################################################

submethod BUILD (Hash :@root_nodes? = []) {

    $?SELF!_assert_arg_rt_nd_aoh( 'new', ':@root_nodes?', @root_nodes );

    @!all_nodes  = [];
    @!root_nodes = [];
    for @root_nodes -> $root_node {
        Rosetta::Model::Node.new( 'document' => $?SELF, *%{$root_node} );
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
                    'ROS_M_D_ARG_AOH_TO_CONSTR_CH_ND_HAS_KEY_CONFL',
                    { 'METH' => $meth, 'ARG' => $arg, 'KEY' => $k } )
                if $val_elem.exists($k);
        }
    }
}

###########################################################################

} # class Rosetta::Model::Document

###########################################################################
###########################################################################

class Rosetta::Model::Node {

    # External packages used by the Rosetta::Model::Node class, that do export symbols:
    # (None Yet)

    # Attributes of every Rosetta::Model::Node object:
    has Rosetta::Model::Document $!document;
    # Rosetta::Model::Document
    # The Document that this Node lives in.
    has Rosetta::Model::Node     $!parent_node;
    # Rosetta::Model::Node
    # The parent Node of this Node, if there is one.
    has Str                      $!node_type;
    # Str
    # What type of Node this is.
    has Any                      %!attributes;
    # Hash(Str) of Any
    # Named attribute values that this Node has, if any.
    has Rosetta::Model::Node     @!child_nodes;
    # Array of Rosetta::Model::Node
    # List of this Node's child Nodes, if there are any.

###########################################################################

submethod BUILD (
            Rosetta::Model::Document :$document!,
            Rosetta::Model::Node     :$parent_node? = undef,
            Str                      :$node_type!,
            Any                      :%attributes?  = {},
            Hash                     :@child_nodes? = [],
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
    %msg_vars{'CLASS'} = 'Rosetta::Model::Node';
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
            'ARG' => $arg, 'EXP_TYPE' => 'Rosetta::Model::Document',
            'VAL' => $val } )
        if !$val.does(Rosetta::Model::Document);
}

my method _assert_arg_node_assume_def (Str $meth!, Str $arg!, $val!) {
    $?SELF!_die_with_msg( 'LKT_ARG_NO_EXP_TYPE', { 'METH' => $meth,
            'ARG' => $arg, 'EXP_TYPE' => 'Rosetta::Model::Node',
            'VAL' => $val } )
        if !$val.does(Rosetta::Model::Node);
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
                    'ROS_M_N_ARG_AOH_TO_CONSTR_CH_ND_HAS_KEY_CONFL',
                    { 'METH' => $meth, 'ARG' => $arg, 'KEY' => $k } )
                if $val_elem.exists($k);
        }
    }
}

###########################################################################

} # class Rosetta::Model::Node

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Rosetta::Model -
Intermediate Relational Language

=head1 VERSION

This document describes Rosetta::Model version 0.720.0.

It also describes the same-number versions of Rosetta::Model::Document
("Document") and Rosetta::Model::Node ("Node").

I<Note that the "Rosetta::Model" package serves only as the name-sake
representative for this whole file, which can be referenced as a unit by
documentation or 'use' statements or Perl archive indexes.  Aside from
'use' statements, you should never refer directly to "Rosetta::Model" in
your code; instead refer to other above-named packages in this file.>

=head1 SYNOPSIS

I<This documentation is pending.>

=head1 DESCRIPTION

Rosetta::Model provides an effective language for defining relational data
models, both the means to create them and the means to interact with them.
The language is IRL (Intermediate Relational Language), and it loosely
resembles both Christopher J. Date's and Hugh Darwen's Tutorial D language
and the ANSI/ISO SQL:2003 standard in purpose and structure, but its
details are different.  This is partly so that it can more elegantly
support the specific relational model that E. F. Codd proposed in his 1970
publication titled "A Relational Model of Data for Large Shared Data
Banks", but that SQL diverged from in some ways.  Regardless, it should be
easy to translate database definitions and queries between IRL and both
Tutorial D and SQL.

Please see the pod-only file L<Rosetta::Language> ("Language"), which is
the human readable authoritative design document for IRL; the file
Rosetta::Model itself is a machine readable language specification that is
derived from the human readable version, and in the case of a conflict,
Language takes precedence.

Rosetta::Model is implemented as abstract syntax trees, and you use it by
creating, manipulating, and reading nodes in these trees.  Each tree node
is atomic, so you can just build the trees by copying scalar values from a
data dictionary; no stitching or parsing more complicated command strings
is necessary like with Tutorial D and SQL.

L<Rosetta> (in the current distribution) is a relational database access
solution that uses Rosetta::Model objects as its native instruction set
rather than SQL strings.  But Rosetta::Model can also be used independently
of Rosetta, such as when translating SQL from one dialect to another, or
between Tutorial D and SQL.

=head1 INTERFACE

The interface of Rosetta::Model is entirely object-oriented; you use it by
creating objects from its member classes, usually invoking C<new()> on the
appropriate class name, and then invoking methods on those objects.  All of
their attributes are private, so you must use accessor methods.
Rosetta::Model does not declare any subroutines or export such.

The usual way that Rosetta::Model indicates a failure is to throw an
exception; most often this is due to invalid input.  If an invoked routine
simply returns, you can assume that it has succeeded, even if the return
value is undefined.

Rosetta::Model's input validation is performed over 2 main phases, which
are referred to as "immediate" and "deferred".  The immediate validations
are performed at the moment the user tries to set the input, and input that
fails immediate evaluation will not be set at all.  The scope of immediate
validation is kept to the minimum possible, and is essentially just
concerned with the well-formedness of the input, such as that mandatory
constructor arguments are provided and that they are of the correct
container type (eg, hash vs array).  The deferred validations are performed
on demand at some time after the input has been set, and could potentially
never be performed at all.  They validate everything except
well-formedness, such as that Rosetta::Model Nodes are arranged correctly
depending on their types, that their attributes have reasonable values, and
that attributes or Nodes are not missing.  The deferred validations, which
can be arbitrarily complex, make up the bulk of the Rosetta::Model code,
and these could potentially be extended by third party add-ons.

=head2 The Rosetta::Model::Document Class

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

Array of Rosetta::Model::Node - This stores a collection of Node objects,
which are all of the Nodes that live in this Document.

=item C<@!root_nodes> - B<Root Nodes>

Array of Rosetta::Model::Node - This stores an ordered list of all this
Document's Node objects that do not have parent Nodes of their own; it is a
sub-set of All Nodes.

=back

This is the main Document constructor method:

=over

=item C<new( :@root_nodes? )>

This method creates and returns a new Rosetta::Model::Document object.  If
the optional named parameter @root_nodes (an array ref) is set, then each
element in it is used to initialize a new Node object (plus an optional
hierarchy of new child Nodes of that new Node) that gets stored in the Root
Nodes attribute (that attribute defaults to empty if the parameter's
corresponding argument is undefined).  The new Document object's All Nodes
attribute starts out empty, but gains one element for each Node in the Node
hierarchies created from a defined @root_nodes.

Some sample usage:

    my Rosetta::Model::Document $document .= new();

    my Rosetta::Model::Document $document2 .= new(
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
which is suitable as a generic data interchange format between
Rosetta::Model and other Perl code such as persistence solutions.
Moreover, these data structures are in exactly the right input format for
Document.new(), by which you can create an identical Document (with member
Nodes) to the one you first invoked export_as_hash() on.

Specifically, export_as_hash() returns a Perl hash ref whose key list
('root_nodes') corresponds exactly to the named parameters of
Document.new(); you can produce a direct clone like this:

    my $cloned_doc = Rosetta::Model::Document.new(
        *%{$original_doc.export_as_hash()} );

Or, to demonstrate the use of a persistence solution:

    # When saving.
    my $hash_to_save = $original_doc.export_as_hash();
    MyPersist.put( $hash_to_save );

    # When restoring.
    my $hash_was_saved = MyPersist.get();
    my $cloned_doc = Rosetta::Model::Document.new( *%{$hash_was_saved} );

=back

=head2 The Rosetta::Model::Node Class

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

Rosetta::Model::Document - This stores a reference to the Document that
this Node and all of its relative Nodes live in.

=item C<$!parent_node> - B<Parent Node>

Rosetta::Model::Node - This stores a reference to this Node's parent Node,
if it has one.  Nodes that do not have this attribute set are considered
"root Nodes" and are listed in their Document's Root Nodes property; in a
manner of speaking, the Document itself is the parent of such Nodes.

=item C<$!node_type> - B<Node Type>

Str - This string identifies what kind of Node this is, and by extension
what kinds of attributes or relative Nodes it can have, as well as where
and how the Node can be used.

=item C<%!attributes> - B<Attributes>

Hash(Str) of Any - This contains zero or more attribute names and values
that help to define this Node.

=item C<@!child_nodes> - B<Child Nodes>

Array of Rosetta::Model::Node - This stores an ordered list of all this
Node's child Nodes, if it has any.

=back

This is the main Node constructor method:

=over

=item C<new( :$document!, :$parent_node?, :$node_type!, :%attributes?,
:@child_nodes? )>

This method creates and returns a new Rosetta::Model::Node object, that
lives in the Document object given in the named parameter $document, and
whose Node Type attribute is set from the named parameter $node_type (a
string); the optional named parameter %attributes (a hash ref) sets the
"Attributes" attribute if provided (it defaults to empty if the parameter's
corresponding argument is not provided).  If the optional named parameter
$parent_node (a Node) is set, then the new Node's "Parent Node" attribute
is set to it, and the new Node is also stored in $parent_node's Child Nodes
attribute; if $parent_node is not set, then the new Node is instead stored
in its Document's "Root Nodes" attribute.  If the optional named parameter
@child_nodes (an array ref) is set, then each element in it is used to
initialize a new Node object (plus an optional hierarchy of new child Nodes
of that new Node) that gets stored in the Child Nodes attribute (that
attribute defaults to empty if the corresponding argument is undefined).

Some sample usage:

    # Declare a unsigned 32-bit integer data type.
    my Rosetta::Model::Node $dt_uint32 .= new(
        'node_type'  => 'data_sub_type',
        'attributes' => {
            'predef_base_type' => 'NUMERIC',
            'num_precision'    => 2**32,
            'num_scale'        => 1,
            'num_min_value'    => 0,
        },
    );

    # Declare an enumerated ('F','M') character value data type.
    my Rosetta::Model::Node $dt_sex .= new(
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
format between Rosetta::Model and other Perl code such as persistence
solutions.  Moreover, these data structures are in exactly the right input
format for Node.new(), by which you can create an identical Node (with
child Nodes) to the one you first invoked export_as_hash() on.

Specifically, export_as_hash() returns a Perl hash ref whose key list
('node_type', 'attributes', 'child_nodes') corresponds exactly to the named
parameters of Node.new(); you need to supply its $document and optional
$parent_node though; you can produce a direct clone like this:

    my $cloned_node = Rosetta::Model::Document.new(
        'document' => $document, *%{$original_node.export_as_hash()} );

Or, to demonstrate the use of a persistence solution:

    # When saving.
    my $hash_to_save = $original_node.export_as_hash();
    MyPersist.put( $hash_to_save );

    # When restoring.
    my $hash_was_saved = MyPersist.get();
    my $cloned_node = Rosetta::Model::Document.new(
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

The Perl 6 package L<Rosetta> is bundled with Rosetta::Model and is its
primary dependent.

These Perl 6 packages are also early dependents of Rosetta::Model:
L<Rosetta::Utility::SQLBuilder>, L<Rosetta::Utility::SQLParser>.

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHOR

Darren R. Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the Rosetta database portability library.

Rosetta is Copyright (c) 2002-2006, Darren R. Duncan.

See the LICENCE AND COPYRIGHT of L<Rosetta> for details.

=head1 ACKNOWLEDGEMENTS

The ACKNOWLEDGEMENTS in L<Rosetta> apply to this file too.

=cut
