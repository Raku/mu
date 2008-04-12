use v6-alpha;

###########################################################################
###########################################################################

my Bool $BOOL_FALSE = Bool::False;
my Bool $BOOL_TRUE  = Bool::True;

my Order $CAT_ORDER_INCREASE = (1 <=> 2);
my Order $CAT_ORDER_SAME     = (1 <=> 1);
my Order $CAT_ORDER_DECREASE = (2 <=> 1);

my Str $EMPTY_STR = q{};

###########################################################################
###########################################################################

module Muldis::DB::Engine::Example::Value-0.0.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub newBool of Muldis::DB::Engine::Example::Value::Bool
        (Bool :$v!) is export {
    return ::Muldis::DB::Engine::Example::Value::Bool.new( :v($v) );
}

sub newInt of Muldis::DB::Engine::Example::Value::Int
        (Int :$v!) is export {
    return ::Muldis::DB::Engine::Example::Value::Int.new( :v($v) );
}

sub newRat of Muldis::DB::Engine::Example::Value::Rat
        (Rat :$v!) is export {
    return ::Muldis::DB::Engine::Example::Value::Rat.new( :v($v) );
}

sub newBlob of Muldis::DB::Engine::Example::Value::Blob
        (Blob :$v!) is export {
    return ::Muldis::DB::Engine::Example::Value::Blob.new( :v($v) );
}

sub newText of Muldis::DB::Engine::Example::Value::Text
        (Str :$v!) is export {
    return ::Muldis::DB::Engine::Example::Value::Text.new( :v($v) );
}

sub newQuasiTuple of Muldis::DB::Engine::Example::Value::QuasiTuple
        (Hash :$v!) is export {
    return ::Muldis::DB::Engine::Example::Value::QuasiTuple.new( :v($v) );
}

sub newTuple of Muldis::DB::Engine::Example::Value::Tuple
        (Hash :$v!) is export {
    return ::Muldis::DB::Engine::Example::Value::Tuple.new( :v($v) );
}

sub newQuasiRelation of Muldis::DB::Engine::Example::Value::QuasiRelation
        (Hash :$heading!, Array :$body!) is export {
    return ::Muldis::DB::Engine::Example::Value::QuasiRelation.new(
        :heading($heading), :body($body) );
}

sub newRelation of Muldis::DB::Engine::Example::Value::Relation
        (Hash :$heading!, Array :$body!) is export {
    return ::Muldis::DB::Engine::Example::Value::Relation.new(
        :heading($heading), :body($body) );
}

sub newCat_Order of Muldis::DB::Engine::Example::Value::Cat_Order
        (Order :$v!) is export {
    return ::Muldis::DB::Engine::Example::Value::Cat_Order.new( :v($v) );
}

###########################################################################

} # module Muldis::DB::Engine::Example::Value

###########################################################################
###########################################################################

role Muldis::DB::Engine::Example::Value::Universal {
    has Str $!root_type;
        # Str.
        # This is the fundamental Muldis D data type that this ::Universal
        # object's implementation sees it as a generic member of, and which
        # generally determines what operators can be used with it.
        # It is a supertype of the declared type.
    has Str $!last_known_mst;
        # Str.
        # This is the Muldis D data type that is the most specific type
        # of this ::Universal, as it was last determined.
        # Since calculating a value's mst may be expensive, this object
        # attribute may either be unset or be out of date with respect to
        # the current type system, that is, not be automatically updated at
        # the same time that a new subtype of its old mst is declared.

    has Str $!which;
        # Str.
        # This is a unique identifier for the value that this object
        # represents that should compare correctly with the corresponding
        # identifiers of all ::Universal-doing objects.
        # It is a text string of format "<tnl> <tn> <vll> <vl>" where:
        #   1. <tn> is the value's root type name (fully qualified)
        #   2. <tnl> is the character-length of <tn>
        #   3. <vl> is the (class-determined) stringified value itself
        #   4. <vll> is the character-length of <vl>
        # This identifier is mainly used when a ::Universal needs to be
        # used as a key to index the ::Universal with, not necessarily when
        # comparing 2 values for is_equality.
        # This identifier can be expensive to calculate, so it will be done
        # only when actually required; eg, by the which() method.

###########################################################################

submethod BUILD (Bool :$v!) {
    $!root_type = self._root_type();
    $!last_known_mst = $!root_type;
    $!which = undef;
    return;
}

###########################################################################

method root_type of Str () {
    return $!root_type;
}

method last_known_mst of Str () {
    return $!last_known_mst;
}

method which of Str () {
    if (!$!which.defined) {
        my Str $rt = $!root_type;
        my Str $main = self._which();
        $!which = "{$rt.graphs} $rt {$main.graphs} $main";
    }
    return $!which;
}

##########################################################################

method is_equal of Bool (:$other!) {
    return $BOOL_FALSE
        if $other.WHAT !=== self.WHAT;
    return $BOOL_FALSE
        if $other!root_type !=== $!root_type;
    return self._is_equal( $other );
}

###########################################################################

} # role Muldis::DB::Engine::Example::Value::Universal

###########################################################################
###########################################################################

role Muldis::DB::Engine::Example::Value::Scalar {
    does Muldis::DB::Engine::Example::Value::Universal;
    submethod BUILD {} # otherwise Pugs r16488 invo Universal.BUILD twice
} # role Muldis::DB::Engine::Example::Value::Scalar

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::Value::Bool {
    does Muldis::DB::Engine::Example::Value::Scalar;

    has Bool $!v;

###########################################################################

submethod BUILD (Bool :$v!) {
    $!v = $v;
    return;
}

###########################################################################

method _root_type of Str () {
    return 'sys.Core.Bool.Bool';
}

method _which of Str () {
    return ~$!v;
}

###########################################################################

method _is_equal of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Bool () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::Value::Bool

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::Value::Int {
    does Muldis::DB::Engine::Example::Value::Scalar;

    has Int $!v;

###########################################################################

submethod BUILD (Int :$v!) {
    $!v = $v;
    return;
}

###########################################################################

method _root_type of Str () {
    return 'sys.Core.Int.Int';
}

method _which of Str () {
    return ~$!v;
}

###########################################################################

method _is_equal of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Int () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::Value::Int

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::Value::Rat {
    does Muldis::DB::Engine::Example::Value::Scalar;

    has Rat $!v;

###########################################################################

submethod BUILD (Rat :$v!) {
    $!v = $v;
    return;
}

###########################################################################

method _root_type of Str () {
    return 'sys.Core.Rat.Rat';
}

method _which of Str () {
    return ~$!v;
}

###########################################################################

method _is_equal of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Rat () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::Value::Rat

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::Value::Blob {
    does Muldis::DB::Engine::Example::Value::Scalar;

    has Blob $!v;

###########################################################################

submethod BUILD (Blob :$v!) {
    $!v = $v;
    return;
}

###########################################################################

method _root_type of Str () {
    return 'sys.Core.Blob.Blob';
}

method _which of Str () {
    return ~$!v;
}

###########################################################################

method _is_equal of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Blob () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::Value::Blob

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::Value::Text {
    does Muldis::DB::Engine::Example::Value::Scalar;

    has Str $!v;

###########################################################################

submethod BUILD (Str :$v!) {
    $!v = $v;
    return;
}

###########################################################################

method _root_type of Str () {
    return 'sys.Core.Text.Text';
}

method _which of Str () {
    return $!v;
}

###########################################################################

method _is_equal of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Str () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::Value::Text

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::Value::QuasiTuple {
    does Muldis::DB::Engine::Example::Value::Universal;

    has Hash $!v;
        # Hash; keys are attr names, values are attr values.

###########################################################################

submethod BUILD (Hash :$v!) {
    $!v = $v;
    return;
}

###########################################################################

method _root_type of Str () {
    return 'sys.Core.QuasiTuple.QuasiTuple';
}

method _which of Str () {
    return $!v.pairs.sort.map:{
            "K {.key.graphs} {.key} V {.value.which()}";
        }.join( q{ } );
}

###########################################################################

method _is_equal of Bool (::T $self: T $other!) {
    return $BOOL_FALSE
        if $other!v.elems !=== $self!v.elems;
    my Hash $v1 = $self!v;
    my Hash $v2 = $other!v;
    for $v1.pairs -> $e {
        return $BOOL_FALSE
            if !$v2.exists($e.key);
        return $BOOL_FALSE
            if !$e.value.is_equal( :other($v2.{$e.key}) );
    }
    return $BOOL_TRUE;
}

###########################################################################

method v of Str () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::Value::QuasiTuple

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::Value::Tuple {
    does Muldis::DB::Engine::Example::Value::QuasiTuple;
    submethod BUILD {} # otherwise Pugs r16488 invo QuasiTuple.BUILD twice

###########################################################################

method _root_type of Str () {
    return 'sys.Core.Tuple.Tuple';
}

###########################################################################

} # class Muldis::DB::Engine::Example::Value::Tuple

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::Value::QuasiRelation {
    does Muldis::DB::Engine::Example::Value::Universal;

    has Hash  $!heading;
        # Hash; keys are attr names, values are undef.
    has Array $!body;
        # Array of Muldis::DB::Engine::Example::Value::QuasiTuple.
        # Each elem a tuple; keys are attr names, values are attr values.
    has Hash  $!key_over_all;
        # Hash of Muldis::DB::Engine::Example::Value::QuasiTuple.
        # Keys are the .WHICH of the values.

###########################################################################

submethod BUILD (Hash :$heading!, Array :$body!) {

    my $key_over_all = {$body.map:{ .which() => $_ }}; # elim dup tpl

    $!heading      = $heading;
    $!body         = [$!key_over_all.values]; # no dup in b
    $!key_over_all = $key_over_all;

    return;
}

###########################################################################

method _root_type of Str () {
    return 'sys.Core.QuasiRelation.QuasiRelation';
}

method _which of Str () {
    my Str $hs = $!heading.keys.sort.map:{ .graphs~q{ }~$_ }.join( q{ } );
    my Str $bs = $!key_over_all.keys.sort.join( q{ } );
    return "H $hs B $bs";
}

###########################################################################

method _is_equal of Bool (::T $self: T $other!) {
    return $BOOL_FALSE
        if $other!heading.elems !=== $self!heading.elems;
    my Hash $h1 = $self!heading;
    my Hash $h2 = $other!heading;
    for $h1.pairs -> $e {
        return $BOOL_FALSE
            if !$h2.exists($e.key);
    }
    return $BOOL_FALSE
        if $other!body.elems !=== $self!body.elems;
    my Hash $b1 = $self!key_over_all;
    my Hash $b2 = $other!key_over_all;
    for $b1.keys -> $ek {
        return $BOOL_FALSE
            if !$b2.exists($ek);
    }
    return $BOOL_TRUE;
}

###########################################################################

method heading of Hash () {
    return $!heading;
}

method body of Array () {
    return $!body;
}

method key_over_all of Hash () {
    return $!key_over_all;
}

###########################################################################

} # class Muldis::DB::Engine::Example::Value::QuasiRelation

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::Value::Relation {
    does Muldis::DB::Engine::Example::Value::QuasiRelation;
    submethod BUILD {} # else Pugs r16488 invo QuasiRelation.BUILD twice

###########################################################################

method _root_type of Str () {
    return 'sys.Core.Relation.Relation';
}

###########################################################################

} # class Muldis::DB::Engine::Example::Value::Relation

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::Value::Cat_Order {
    does Muldis::DB::Engine::Example::Value::Scalar;

    has Order $!v;

###########################################################################

submethod BUILD (Order :$v!) {
    $!v = $v;
    return;
}

###########################################################################

method _root_type of Str () {
    return 'sys.Core.Cat.Order';
}

method _which of Str () {
    return ~$!v;
}

###########################################################################

method _is_equal of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Order () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::Value::Cat_Order

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Muldis::DB::Engine::Example::Value -
Physical representations of all core data type values

=head1 VERSION

This document describes Muldis::DB::Engine::Example::Value version 0.0.0
for Perl 6.

It also describes the same-number versions for Perl 6 of [...].

=head1 DESCRIPTION

This file is used internally by L<Muldis::DB::Engine::Example>; it is not
intended to be used directly in user code.

It provides physical representations of data type values that this Example
Engine uses to implement Muldis D.  The API of these is expressly not
intended to match the API that the language itself specifies as possible
representations for system-defined data types.

Specifically, this file represents the core system-defined data types that
all Muldis D implementations must have, namely: Bool, Int, Rat, Blob, Text,
Tuple, Relation, QuasiTuple, QuasiRelation, and the Cat.* types.

By contrast, the optional data types are given physical representations by
other files: L<Muldis::DB::Engine::Example::Value::Temporal>,
L<Muldis::DB::Engine::Example::Value::Spatial>.

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

=head1 LICENSE AND COPYRIGHT

This file is part of the Muldis DB framework.

Muldis DB is Copyright © 2002-2008, Darren Duncan.

See the LICENSE AND COPYRIGHT of L<Muldis::DB> for details.

=head1 TRADEMARK POLICY

The TRADEMARK POLICY in L<Muldis::DB> applies to this file too.

=head1 ACKNOWLEDGEMENTS

The ACKNOWLEDGEMENTS in L<Muldis::DB> apply to this file too.

=cut
