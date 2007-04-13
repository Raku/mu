use v6-alpha;

###########################################################################
###########################################################################

my $EMPTY_STR = q{};

my $FALSE = (1 == 0);
my $TRUE  = (1 == 1);

###########################################################################
###########################################################################

module QDRDBMS::Engine::Example::PhysType-0.0.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub Bool of QDRDBMS::Engine::Example::PhysType::Bool
        (Bool :$v!) is export {
    return QDRDBMS::Engine::Example::PhysType::Bool.new( :v($v) );
}

sub Text of QDRDBMS::Engine::Example::PhysType::Text (Str :$v!) is export {
    return QDRDBMS::Engine::Example::PhysType::Text.new( :v($v) );
}

sub Blob of QDRDBMS::Engine::Example::PhysType::Blob
        (Blob :$v!) is export {
    return QDRDBMS::Engine::Example::PhysType::Blob.new( :v($v) );
}

sub Int of QDRDBMS::Engine::Example::PhysType::Int (Int :$v!) is export {
    return QDRDBMS::Engine::Example::PhysType::Int.new( :v($v) );
}

sub TextKeyedMap of QDRDBMS::Engine::Example::PhysType::TextKeyedMap
        (Hash :$map!) is export {
    return QDRDBMS::Engine::Example::PhysType::TextKeyedMap.new(
        :map($map) );
}

sub Heading of QDRDBMS::Engine::Example::PhysType::Heading
        (Array :$attr_defs_aoa!) is export {
    return QDRDBMS::Engine::Example::PhysType::Heading.new(
        :attr_defs_aoa($attr_defs_aoa) );
}

sub Tuple of QDRDBMS::Engine::Example::PhysType::Tuple
        (QDRDBMS::Engine::Example::PhysType::Heading :$heading!,
        QDRDBMS::Engine::Example::PhysType::TextKeyedMap :$body!)
        is export {
    return QDRDBMS::Engine::Example::PhysType::Tuple.new(
        :heading($heading), :body($body) );
}

sub Relation of QDRDBMS::Engine::Example::PhysType::Relation
        (:$heading!, :$body!, :$key_defs_aoh!, :$index_defs_aoh!)
        is export {
    return QDRDBMS::Engine::Example::PhysType::Relation.new(
        :heading($heading), :body($body),
        :key_defs_aoh($key_defs_aoh), :index_defs_aoh($index_defs_aoh) );
}

###########################################################################

} # module QDRDBMS::Engine::Example::PhysType

###########################################################################
###########################################################################

role QDRDBMS::Engine::Example::PhysType::Value {
#    has QDRDBMS::Engine::Example::PhysType::TypeRef $!root_type;
        # QDRDBMS::Engine::Example::PhysType::TypeRef.
        # This is the fundamental QDRDBMS D data type that this ::Value
        # object's implementation sees it as a generic member of, and which
        # generally determines what operators can be used with it.
        # It is a supertype of the declared type.
#    has QDRDBMS::Engine::Example::PhysType::TypeRef $!decl_type;
        # QDRDBMS::Engine::Example::PhysType::TypeRef.
        # This is the QDRDBMS D data type that the ::Value was declared to
        # be a member of when the ::Value object was created.
#    has QDRDBMS::Engine::Example::PhysType::TypeRef $!last_known_mst;
        # QDRDBMS::Engine::Example::PhysType::TypeRef.
        # This is the QDRDBMS data type that is the most specific type of
        # this ::Value, as it was last determined.
        # It is a subtype of the declared type.
        # Since calculating a value's mst may be expensive, this object
        # attribute may either be unset or be out of date with respect to
        # the current type system, that is, not be automatically updated at
        # the same time that a new subtype of its old mst is declared.

#    has Str $!which;
        # Str.
        # This is a unique identifier for the value that this object
        # represents that should compare correctly with the corresponding
        # identifiers of all ::Value-doing objects.
        # It is a text string of format "<tnl> <tn> <vll> <vl>" where:
        #   1. <tn> is the value's root type name (fully qualified)
        #   2. <tnl> is the character-length of <tn>
        #   3. <vl> is the (class-determined) stringified value itself
        #   4. <vll> is the character-length of <vl>
        # This identifier is mainly used when a ::Value needs to be used as
        # a key to index the ::Value with, not necessarily when comparing
        # 2 values for equality.
        # This identifier can be expensive to calculate, so it will be done
        # only when actually required; eg, by the which() method.

###########################################################################

method root_type () {
    die "not implemented by subclass\n";
}

method declared_type () {
    die "not implemented by subclass\n";
}

method most_specific_type () {
    die "not implemented by subclass\n";
}

method which () {
    die "not implemented by subclass\n";
}

###########################################################################

} # role QDRDBMS::Engine::Example::PhysType::Value

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Bool {
    does QDRDBMS::Engine::Example::PhysType::Value;

    has Bool $!v;

    has Str $!which;

###########################################################################

submethod BUILD (Bool :$v!) {
    $!v = $v;
}

###########################################################################

method root_type of Str () {
    return 'sys.type.Bool';
}

method which of Str () {
    if (!$!which.defined) {
        my Str $s = ~$!v;
        $!which = "13 sys.type.Bool {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Bool

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Text {
    does QDRDBMS::Engine::Example::PhysType::Value;

    has Str $!v;

    has Str $!which;

###########################################################################

submethod BUILD (Str :$v!) {
    $!v = $v;
}

###########################################################################

method root_type of Str () {
    return 'sys.type.Text';
}

method which of Str () {
    if (!$!which.defined) {
        my Str $s = $!v;
        $!which = "13 sys.type.Text {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Text

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Blob {
    does QDRDBMS::Engine::Example::PhysType::Value;

    has Blob $!v;

    has Str $!which;

###########################################################################

submethod BUILD (Blob :$v!) {
    $!v = $v;
}

###########################################################################

method root_type of Str () {
    return 'sys.type.Blob';
}

method which of Str () {
    if (!$!which.defined) {
        my Str $s = ~$!v;
        $!which = "13 sys.type.Blob {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Blob

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Int {
    does QDRDBMS::Engine::Example::PhysType::Value;

    has Int $!v;

    has Str $!which;

###########################################################################

submethod BUILD (Int :$v!) {
    $!v = $v;
}

###########################################################################

method root_type of Str () {
    return 'sys.type.Int';
}

method which of Str () {
    if (!$!which.defined) {
        my Str $s = ~$!v;
        $!which = "12 sys.type.Int {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Int

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::TextKeyedMap {
    does QDRDBMS::Engine::Example::PhysType::Value;

#    has Hash(Str) of Any $!map;
    has Hash $!map;
        # A Hash with 0..N elements:
            # Each Hash key is a Str.
            # Each Hash value is a ::Example::* value of some kind.

    has Str $!which;

###########################################################################

submethod BUILD (Hash :$map!) {
    my ($self, $map) = @_;
    $!map = $map;
}

###########################################################################

method root_type of Str () {
    return 'sys.type._TextKeyedMap';
}

method which of Str () {
    if (!$!which.defined) {
        my Str $s = $!map.pairs.sort.map:{
                "K {.key.graphs} {.key} V {.value.which()}";
            }.join( q{ } );
        $!which = "22 sys.type._TextKeyedMap {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

method ref_to_attr_map of Hash () {
    return $!map;
}

###########################################################################

method pairs of Array () {
    return $!map.pairs;
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::TextKeyedMap

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Heading {
    does QDRDBMS::Engine::Example::PhysType::Value;

#    has Hash of Array $!attr_defs_by_name;
    has Hash $!attr_defs_by_name;
        # A p6 Hash with 0..N elements:
            # Each Hash key is a Str; an attr name.
            # Each Hash value would describe a single tuple|relation
            # attribute; it is a p6 Array with 3 elements:
                # 1. attr name: a Str; same as Hash key.
                # 2. major type: a Str, one of: 'S','T','R'
                # 3. minor type: a disjunction depending on maj-tp value:
                    # 'S': a Str.
                    # 'T'|'R': a Heading.
#    has Array of Array $!attr_defs_ordered;
    has Array $!attr_defs_ordered;
        # A p6 Array with 0..N elements; its elements are all of the Hash
        # values of $!attr_defs_by_name, sorted by the attr-name/Hash key.

    has Str $!which;

###########################################################################

submethod BUILD (Array :$attr_defs_aoa!) {
    $!attr_defs_by_name = {$attr_defs_aoa.map:{ .[0] => $_ }};
    $!attr_defs_ordered = [$!attr_defs_by_name.pairs.sort.map:{ .value }];
}

###########################################################################

method root_type of Str () {
    return 'sys.type._Heading';
}

method which of Str () {
    if (!$!which.defined) {
        my Str $s = $!attr_defs_ordered.map:{
                my ($atnm, $mjtp, $mntp) = .values;
                "ATNM {$atnm.graphs} $atnm"
                    ~ " MJTP {$mjtp.graphs} $mjtp"
                    ~ " MNTP {$mntp.which()}";
            }.join( q{ } );
        $!which = "17 sys.type._Heading {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

method get_attr_attr_defs_ordered of Array () {
    return $!attr_defs_ordered;
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Heading

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Tuple {
    does QDRDBMS::Engine::Example::PhysType::Value;

    has QDRDBMS::Engine::Example::PhysType::Heading $!heading;
        # A Heading.
    has QDRDBMS::Engine::Example::PhysType::TextKeyedMap $!body;
        # A TextKeyedMap whose keys match the attribute names in $!heading,
        # and whose values are of the types specified in $!heading.

    has Str $!which;

###########################################################################

submethod BUILD (QDRDBMS::Engine::Example::PhysType::Heading :$heading!,
        QDRDBMS::Engine::Example::PhysType::TextKeyedMap :$body!) {
    $!heading = $heading;
    $!body    = $body;
}

###########################################################################

method root_type of Str () {
    return 'sys.type.Tuple';
}

method which of Str () {
    if (!$!which.defined) {
        my Str $s = "H {$!heading.which()} B {$!body.which()}";
        $!which = "14 sys.type.Tuple {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Tuple

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Relation {
    does QDRDBMS::Engine::Example::PhysType::Value;

    has $!heading;
        # A Heading.
    has $!body;
        # A p6 Array with 0..N elements, each element being a
        # TextKeyedMap whose keys match the attribute names in $!heading,
        # and whose values are of the types specified in $!heading.
    has $!key_defs;
        # A p6 Hash with 1..N elements
    has $!key_data;
    has $!index_defs;
        # A p6 Hash with 0..N elements
    has $!index_data;

    has Str $!which;

###########################################################################

submethod BUILD (:$heading!, :$body!, :$key_defs_aoh!, :$index_defs_aoh!) {
    # Assume input $body may contain duplicate elements (okay; silently
    # remove), and/or duplicate attributes where the attributes are keys
    # (not okay; throw an exception).
    # Otherwise assume all input is okay, and no key|index redundancy.
    my $attr_defs_ordered = $heading.get_attr_attr_defs_ordered();
    if ($key_defs_aoh.elems == 0) {
        # There is no explicit key, so make an implicit one over all attrs.
        $key_defs_aoh.push( {$attr_defs_ordered.map:{ .[0] => undef }} );
    }
    my $key_defs = {};
    my $index_defs = {};




    $!heading    = $heading;
    $!body       = $body;
    $!key_defs   = $key_defs;
    $!index_defs = $index_defs;
}

###########################################################################

method root_type of Str () {
    return 'sys.type.Relation';
}

method which of Str () {
    if (!$!which.defined) {
        my Str $s = "H {$!heading.which()} B " ~ $!body.map:{
                .which();
            }.sort.join( q{ } );
        $!which = "17 sys.type.Relation {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Relation

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

QDRDBMS::Engine::Example::PhysType -
Physical representations of all core data types

=head1 VERSION

This document describes QDRDBMS::Engine::Example::PhysType version 0.0.0
for Perl 6.

It also describes the same-number versions for Perl 6 of ::Bool, ::Text,
::Blob, ::Int, ::Tuple, and ::Relation.

=head1 DESCRIPTION

This file is used internally by L<QDRDBMS::Engine::Example>; it is not
intended to be used directly in user code.

It provides physical representations of data types that this Example Engine
uses to implement QDRDBMS D.  The API of these is expressly not intended to
match the API that the language itself specifies as possible
representations for system-defined data types.

Specifically, this file represents the core system-defined data types that
all QDRDBMS D implementations must have, namely: Bool, Text, Blob, Int,
Tuple, Relation, and the Cat.* types.

By contast, the optional data types are given physical representations by
other files: L<QDRDBMS::Engine::Example::PhysType::Num>,
L<QDRDBMS::Engine::Example::PhysType::Temporal>,
L<QDRDBMS::Engine::Example::PhysType::Spatial>.

=head1 BUGS AND LIMITATIONS

This file assumes that it will only be invoked by other components of
Example, and that they will only be feeding it arguments that are exactly
what it requires.  For reasons of performance, it does not do any of its
own basic argument validation, as doing so should be fully redundant.  Any
invoker should be validating any arguments that it in turn got from user
code.  Moreover, this file will often take or return values by reference,
also for performance, and the caller is expected to know that they should
not be modifying said then-shared values afterwards.

=head1 AUTHOR

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the QDRDBMS framework.

QDRDBMS is Copyright © 2002-2007, Darren Duncan.

See the LICENCE AND COPYRIGHT of L<QDRDBMS> for details.

=cut
