use v6-alpha;

###########################################################################
###########################################################################

my Bool $BOOL_FALSE = Bool::False;
my Bool $BOOL_TRUE  = Bool::True;

my Order $ORDER_INCREASE = (1 <=> 2);
my Order $ORDER_SAME     = (1 <=> 1);
my Order $ORDER_DECREASE = (2 <=> 1);

my Str $EMPTY_STR = q{};

###########################################################################
###########################################################################

module Muldis::DB::Engine::Example::PhysType-0.3.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub ptBool of Muldis::DB::Engine::Example::PhysType::Bool
        (Bool :$v!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::Bool.new( :v($v) );
}

sub ptOrder of Muldis::DB::Engine::Example::PhysType::Order
        (Order :$v!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::Order.new( :v($v) );
}

sub ptInt of Muldis::DB::Engine::Example::PhysType::Int
        (Int :$v!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::Int.new( :v($v) );
}

sub ptBlob of Muldis::DB::Engine::Example::PhysType::Blob
        (Blob :$v!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::Blob.new( :v($v) );
}

sub ptText of Muldis::DB::Engine::Example::PhysType::Text
        (Str :$v!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::Text.new( :v($v) );
}

sub ptTuple of Muldis::DB::Engine::Example::PhysType::Tuple
        (Muldis::DB::Engine::Example::PhysType::TypeDict :$heading!,
        Muldis::DB::Engine::Example::PhysType::ValueDict :$body!)
        is export {
    return ::Muldis::DB::Engine::Example::PhysType::Tuple.new(
        :heading($heading), :body($body) );
}

sub ptQuasiTuple of Muldis::DB::Engine::Example::PhysType::QuasiTuple
        (Muldis::DB::Engine::Example::PhysType::QuasiTypeDict :$heading!,
        Muldis::DB::Engine::Example::PhysType::QuasiValueDict :$body!)
        is export {
    return ::Muldis::DB::Engine::Example::PhysType::QuasiTuple.new(
        :heading($heading), :body($body) );
}

sub ptRelation of Muldis::DB::Engine::Example::PhysType::Relation
        (Muldis::DB::Engine::Example::PhysType::TypeDict :$heading!,
        Array :$body!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::Relation.new(
        :heading($heading), :body($body) );
}

sub ptQuasiRelation of Muldis::DB::Engine::Example::PhysType::QuasiRelation
        (Muldis::DB::Engine::Example::PhysType::QuasiTypeDict :$heading!,
        Array :$body!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::QuasiRelation.new(
        :heading($heading), :body($body) );
}

sub ptTypeInvo of Muldis::DB::Engine::Example::PhysType::TypeInvo
        (Str :$kind!, Any :$spec!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::TypeInvo.new(
        :kind($kind), :spec($spec) );
}

sub ptQuasiTypeInvo of Muldis::DB::Engine::Example::PhysType::QuasiTypeInvo
        (Str :$kind!, Any :$spec!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::QuasiTypeInvo.new(
        :kind($kind), :spec($spec) );
}

sub ptTypeDict of Muldis::DB::Engine::Example::PhysType::TypeDict
        (Hash :$map!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::TypeDict.new(
        :map($map) );
}

sub ptQuasiTypeDict of Muldis::DB::Engine::Example::PhysType::QuasiTypeDict
        (Hash :$map!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::QuasiTypeDict.new(
        :map($map) );
}

sub ptValueDict of Muldis::DB::Engine::Example::PhysType::ValueDict
        (Hash :$map!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::ValueDict.new(
        :map($map) );
}

sub ptQuasiValueDict of Muldis::DB::Engine::Example::PhysType::QuasiValueDict
        (Hash :$map!) is export {
    return ::Muldis::DB::Engine::Example::PhysType::QuasiValueDict.new(
        :map($map) );
}

###########################################################################

} # module Muldis::DB::Engine::Example::PhysType

###########################################################################
###########################################################################

role Muldis::DB::Engine::Example::PhysType::Value {
#    has Muldis::DB::Engine::Example::PhysType::Cat_EntityName $!root_type;
        # Muldis::DB::Engine::Example::PhysType::Cat_EntityName.
        # This is the fundamental Muldis D data type that this ::Value
        # object's implementation sees it as a generic member of, and which
        # generally determines what operators can be used with it.
        # It is a supertype of the declared type.
#    has Muldis::DB::Engine::Example::PhysType::Cat_EntityName $!decl_type;
        # Muldis::DB::Engine::Example::PhysType::Cat_EntityName.
        # This is the Muldis D data type that the ::Value was declared to
        # be a member of when the ::Value object was created.
#    has Muldis::DB::Engine::Example::PhysType::Cat_EntityName
#        $!last_known_mst;
        # Muldis::DB::Engine::Example::PhysType::Cat_EntityName.
        # This is the Muldis::DB data type that is the most specific type
        # of this ::Value, as it was last determined.
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

method root_type {
    die q{not implemented by subclass } ~ self.WHAT;
}

method declared_type {
    die q{not implemented by subclass } ~ self.WHAT;
}

method most_specific_type {
    die q{not implemented by subclass } ~ self.WHAT;
}

method which {
    die q{not implemented by subclass } ~ self.WHAT;
}

###########################################################################

method as_ast {
    die q{not implemented by subclass } ~ self.WHAT;
}

###########################################################################

method equal of Bool (:$other!) {
    return $BOOL_FALSE
        if $other.WHAT !=== self.WHAT;
    return self._equal( $other );
}

method _equal {
    die q{not implemented by subclass } ~ self.WHAT;
}

###########################################################################

} # role Muldis::DB::Engine::Example::PhysType::Value

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::Bool {
    does Muldis::DB::Engine::Example::PhysType::Value;

    has Bool $!v;

    has Str $!which;

###########################################################################

submethod BUILD (Bool :$v!) {
    $!v = $v;
    return;
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

method as_ast of Muldis::DB::Literal::Bool () {
    return ::Muldis::DB::Literal::Bool.new( :v($!v) );
}

###########################################################################

method _equal of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Bool () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::PhysType::Bool

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::Order {
    does Muldis::DB::Engine::Example::PhysType::Value;

    has Order $!v;

    has Str $!which;

###########################################################################

submethod BUILD (Order :$v!) {
    $!v = $v;
    return;
}

###########################################################################

method root_type of Str () {
    return 'sys.type.Order';
}

method which of Str () {
    if (!$!which.defined) {
        my Str $s = ~$!v;
        $!which = "14 sys.type.Order {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

method as_ast of Muldis::DB::Literal::Order () {
    return ::Muldis::DB::Literal::Order.new( :v($!v) );
}

###########################################################################

method _equal of Order (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Order () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::PhysType::Order

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::Int {
    does Muldis::DB::Engine::Example::PhysType::Value;

    has Int $!v;

    has Str $!which;

###########################################################################

submethod BUILD (Int :$v!) {
    $!v = $v;
    return;
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

method as_ast of Muldis::DB::Literal::Int () {
    return ::Muldis::DB::Literal::Int.new( :v($!v) );
}

###########################################################################

method _equal of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Int () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::PhysType::Int

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::Blob {
    does Muldis::DB::Engine::Example::PhysType::Value;

    has Blob $!v;

    has Str $!which;

###########################################################################

submethod BUILD (Blob :$v!) {
    $!v = $v;
    return;
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

method as_ast of Muldis::DB::Literal::Blob () {
    return ::Muldis::DB::Literal::Blob.new( :v($!v) );
}

###########################################################################

method _equal of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Blob () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::PhysType::Blob

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::Text {
    does Muldis::DB::Engine::Example::PhysType::Value;

    has Str $!v;

    has Str $!which;

###########################################################################

submethod BUILD (Str :$v!) {
    $!v = $v;
    return;
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

method as_ast of Muldis::DB::Literal::Text () {
    return ::Muldis::DB::Literal::Text.new( :v($!v) );
}

###########################################################################

method _equal of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Str () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::Engine::Example::PhysType::Text

###########################################################################
###########################################################################

role Muldis::DB::Engine::Example::PhysType::_Tuple {
    does Muldis::DB::Engine::Example::PhysType::Value;

    has Muldis::DB::Engine::Example::PhysType::_TypeDict  $!heading;
    has Muldis::DB::Engine::Example::PhysType::_ValueDict $!body;

    has Str $!which;

###########################################################################

submethod BUILD
        (Muldis::DB::Engine::Example::PhysType::_TypeDict :$heading!,
        Muldis::DB::Engine::Example::PhysType::_ValueDict :$body!) {
    $!heading = $heading;
    $!body    = $body;
    return;
}

###########################################################################

method root_type of Str () {
    return 'sys.type.' ~ (self._allows_quasi() ?? 'Quasi' !! '') ~ 'Tuple';
}

method which of Str () {
    if (!$!which.defined) {
        my Str $root_type = 'sys.type.'
            ~ (self._allows_quasi() ?? 'Quasi' !! '') ~ 'Tuple';
        my Str $tpwl = $root_type.graphs ~ q{ } ~ $root_type;
        my Str $s = "H {$!heading.which()} B {$!body.which()}";
        $!which = "$tpwl {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

method as_ast of Muldis::DB::Literal::_Tuple () {
    my $call_args = \( :heading($!heading.as_ast()),
        :body($!body.as_ast()) );
    return self._allows_quasi()
        ?? ::Muldis::DB::Literal::QuasiTuple.new.callwith( |$call_args )
        !! ::Muldis::DB::Literal::Tuple.new.callwith( |$call_args );
}

###########################################################################

method _equal of Bool (::T $self: T $other!) {
    return ($self!heading.equal( :other($other!heading) )
        and $self!body.equal( :other($other!body) ));
}

###########################################################################

method heading of Muldis::DB::Engine::Example::PhysType::_TypeDict () {
    return $!heading;
}

method body of Muldis::DB::Engine::Example::PhysType::_ValueDict () {
    return $!body;
}

###########################################################################

method attr_count of Int () {
    return $!heading.elem_count();
}

method attr_exists of Bool (Str :$attr_name!) {
    return $!heading.elem_exists( :elem_name($attr_name) );
}

method attr_type of Muldis::DB::Engine::Example::PhysType::_TypeInvo
        (Str :$attr_name!) {
    return $!heading.elem_value( :elem_name($attr_name) );
}

method attr_value of Muldis::DB::Engine::Example::PhysType::Value
        (Str :$attr_name!) {
    return $!body.elem_value( :elem_name($attr_name) );
}

###########################################################################

} # role Muldis::DB::Engine::Example::PhysType::_Tuple

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::Tuple {
    does Muldis::DB::Engine::Example::PhysType::_Tuple;
    submethod BUILD {} # otherwise Pugs r16488 invo _Tuple.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_FALSE; }
} # class Muldis::DB::Engine::Example::PhysType::Tuple

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::QuasiTuple {
    does Muldis::DB::Engine::Example::PhysType::_Tuple;
    submethod BUILD {} # otherwise Pugs r16488 invo _Tuple.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_TRUE; }
} # class Muldis::DB::Engine::Example::PhysType::QuasiTuple

###########################################################################
###########################################################################

role Muldis::DB::Engine::Example::PhysType::_Relation {
    does Muldis::DB::Engine::Example::PhysType::Value;

    has Muldis::DB::Engine::Example::PhysType::_TypeDict $!heading;
    has Array                                           $!body;
    has Hash                                            $!key_over_all;

    has Str $!which;

###########################################################################

submethod BUILD
        (Muldis::DB::Engine::Example::PhysType::_TypeDict :$heading!,
        Array :$body!) {

    my $key_over_all = {$body.map:{ .which() => $_ }}; # elim dup tpl

    $!heading      = $heading;
    $!body         = [$!key_over_all.values]; # no dup in b
    $!key_over_all = $key_over_all;

    return;
}

###########################################################################

method root_type of Str () {
    return
        'sys.type.' ~ (self._allows_quasi() ?? 'Quasi' !! '') ~ 'Relation';
}

method which of Str () {
    if (!$!which.defined) {
        my Str $root_type = 'sys.type.'
            ~ (self._allows_quasi() ?? 'Quasi' !! '') ~ 'Relation';
        my Str $tpwl = $root_type.graphs ~ q{ } ~ $root_type;
        my Str $s = "H {$!heading.which()} B "
            ~ $!key_over_all.keys.sort.join( q{ } );
        $!which = "$tpwl {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

method as_ast of Muldis::DB::Literal::_Relation () {
    my $call_args = \( :heading($!heading.as_ast()),
        :body([$!body.map:{ .as_ast() }]) );
    return self._allows_quasi()
        ?? ::Muldis::DB::Literal::QuasiRelation.new.callwith( |$call_args )
        !! ::Muldis::DB::Literal::Relation.new.callwith( |$call_args );
}

###########################################################################

method _equal of Bool (::T $self: T $other!) {
    return $BOOL_FALSE
        if !$self!heading.equal( :other($other!heading) );
    return $BOOL_FALSE
        if $other!body.elems !=== $self!body.elems;
    my Hash $v1 = $self!key_over_all;
    my Hash $v2 = $other!key_over_all;
    for $v1.keys -> $ek {
        return $BOOL_FALSE
            if !$v2.exists($ek);
    }
    return $BOOL_TRUE;
}

###########################################################################

method heading of Muldis::DB::Engine::Example::PhysType::_TypeDict () {
    return $!heading;
}

method body of Array () {
    return $!body.values;
}

###########################################################################

method tuple_count of Int () {
    return $!body.elems;
}

###########################################################################

method attr_count of Int () {
    return $!heading.elem_count();
}

method attr_exists of Bool (Str :$attr_name!) {
    return $!heading.elem_exists( :elem_name($attr_name) );
}

method attr_type of Muldis::DB::Engine::Example::PhysType::_TypeInvo
        (Str :$attr_name!) {
    return $!heading.elem_value( :elem_name($attr_name) );
}

method attr_values of Array (Str :$attr_name!) {
    return [$!body.map:{ .elem_value( :elem_name($attr_name) ) }];
}

###########################################################################

} # role Muldis::DB::Engine::Example::PhysType::_Relation

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::Relation {
    does Muldis::DB::Engine::Example::PhysType::_Relation;
    submethod BUILD {} # otherwise Pugs r16488 invo _Relation.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_FALSE; }
} # class Muldis::DB::Engine::Example::PhysType::Relation

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::QuasiRelation {
    does Muldis::DB::Engine::Example::PhysType::_Relation;
    submethod BUILD {} # otherwise Pugs r16488 invo _Relation.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_TRUE; }
} # class Muldis::DB::Engine::Example::PhysType::QuasiRelation

###########################################################################
###########################################################################

role Muldis::DB::Engine::Example::PhysType::_TypeInvo {
    does Muldis::DB::Engine::Example::PhysType::Value;

    has Str $!kind;
    has Any $!spec;

    has Str $!which;

###########################################################################

submethod BUILD (Str :$kind!, Any :$spec!) {
    $!kind = $kind;
    $!spec = $spec;
    return;
}

###########################################################################

method root_type of Str () {
    return 'sys.type._TypeInvo' ~ (self._allows_quasi() ?? 'AQ' !! 'NQ');
}

method which of Str () {
    if (!$!which.defined) {
        my Str $tpwl = '20 sys.type._TypeInvo'
            ~ (self._allows_quasi() ?? 'AQ' !! 'NQ');
        my Str $sk = $!kind.graphs ~ q{ } ~ $!kind;
        my Str $ss = $!kind === 'Any'|'Scalar'
            ?? $!spec.graphs ~ q{ } ~ $!spec !! $!spec.which();
        my Str $s = "KIND $sk SPEC $ss";
        $!which = "$tpwl {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

method as_ast of Muldis::DB::Literal::_TypeInvo () {
    my $call_args = \( :kind($!kind),
        :spec($!kind === 'Any' ?? $!spec
            !! $!kind === 'Scalar' ?? ::Muldis::DB::Literal::EntityName.new( :text($!spec) )
            !! $!spec.as_ast()) );
    return self._allows_quasi()
        ?? ::Muldis::DB::Literal::QuasiTypeInvo.new.callwith( |$call_args )
        !! ::Muldis::DB::Literal::TypeInvo.new.callwith( |$call_args );
}

###########################################################################

method _equal of Bool (::T $self: T $other!) {
    return $BOOL_FALSE
        if $other!kind !=== $self!kind;
    return $self!kind === 'Any'|'Scalar' ?? $other!spec === $self!spec
        !! $self!spec.equal( :other($other!spec) );
}

###########################################################################

method kind of Str () {
    return $!kind;
}

method spec of Any () {
    return $!spec;
}

###########################################################################

} # role Muldis::DB::Engine::Example::PhysType::_TypeInvo

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::TypeInvo {
    does Muldis::DB::Engine::Example::PhysType::_TypeInvo;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeInvo.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_FALSE; }
} # class Muldis::DB::Engine::Example::PhysType::TypeInvo

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::QuasiTypeInvo {
    does Muldis::DB::Engine::Example::PhysType::_TypeInvo;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeInvo.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_TRUE; }
} # class Muldis::DB::Engine::Example::PhysType::QuasiTypeInvo

###########################################################################
###########################################################################

role Muldis::DB::Engine::Example::PhysType::_TypeDict {
    does Muldis::DB::Engine::Example::PhysType::Value;

    has Hash $!map;
        # A p6 Hash with 0..N elements:
            # Each Hash key is a Str; an attr name.
            # Each Hash value is a TypeInvo; an attr declared type.

    has Str $!which;

###########################################################################

submethod BUILD (Hash :$map!) {
    $!map = $map;
    return;
}

###########################################################################

method root_type of Str () {
    return 'sys.type._TypeDict' ~ (self._allows_quasi() ?? 'AQ' !! 'NQ');
}

method which of Str () {
    if (!$!which.defined) {
        my Str $tpwl = '20 sys.type._TypeDict'
            ~ (self._allows_quasi() ?? 'AQ' !! 'NQ');
        my Str $s = $!map.pairs.sort.map:{
                "K {.key.graphs} {.key} V {.value.which()}";
            }.join( q{ } );
        $!which = "$tpwl {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

method as_ast of Muldis::DB::Literal::_TypeDict () {
    my $call_args = \( :map([ $!map.pairs.map:{
            [::Muldis::DB::Literal::EntityName.new( :text(.key) ), .value.as_ast()],
        } ]) );
    return self._allows_quasi()
        ?? ::Muldis::DB::Literal::QuasiTypeDict.new.callwith( |$call_args )
        !! ::Muldis::DB::Literal::TypeDict.new.callwith( |$call_args );
}

###########################################################################

method _equal of Bool (::T $self: T $other!) {
    return $BOOL_FALSE
        if $other!map.elems !=== $self!map.elems;
    my Hash $v1 = $self!map;
    my Hash $v2 = $other!map;
    for $v1.pairs -> $e {
        return $BOOL_FALSE
            if !$v2.exists($e.key);
        return $BOOL_FALSE
            if !$e.value.equal( :other($v2.{$e.key}) );
    }
    return $BOOL_TRUE;
}

###########################################################################

method map of Hash () {
    return $!map;
}

###########################################################################

method elem_count of Int () {
    return $!map.elems;
}

method elem_exists of Bool (Str :$elem_name!) {
    return $!map.exists($elem_name);
}

method elem_value of Muldis::DB::Engine::Example::PhysType::_TypeInvo
        (Str :$elem_name!) {
    return $!map{$elem_name};
}

###########################################################################

} # role Muldis::DB::Engine::Example::PhysType::_TypeDict

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::TypeDict {
    does Muldis::DB::Engine::Example::PhysType::_TypeDict;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeDict.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_FALSE; }
} # class Muldis::DB::Engine::Example::PhysType::TypeDict

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::QuasiTypeDict {
    does Muldis::DB::Engine::Example::PhysType::_TypeDict;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeDict.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_TRUE; }
} # class Muldis::DB::Engine::Example::PhysType::QuasiTypeDict

###########################################################################
###########################################################################

role Muldis::DB::Engine::Example::PhysType::_ValueDict {
    does Muldis::DB::Engine::Example::PhysType::Value;

    has Hash $!map;

    has Str $!which;

###########################################################################

submethod BUILD (Hash :$map!) {
    $!map = $map;
    return;
}

###########################################################################

method root_type of Str () {
    return 'sys.type._ValueDict' ~ (self._allows_quasi() ?? 'AQ' !! 'NQ');
}

method which of Str () {
    if (!$!which.defined) {
        my Str $tpwl = '20 sys.type._ValueDict'
            ~ (self._allows_quasi() ?? 'AQ' !! 'NQ');
        my Str $s = $!map.pairs.sort.map:{
                "K {.key.graphs} {.key} V {.value.which()}";
            }.join( q{ } );
        $!which = "$tpwl {$s.graphs} $s";
    }
    return $!which;
}

###########################################################################

method as_ast of Muldis::DB::Literal::_ExprDict () {
    return ::Muldis::DB::Literal::_ExprDict.new( :map([ $!map.pairs.map:{
            [::Muldis::DB::Literal::EntityName.new( :text(.key) ), .value.as_ast()],
        } ]) );
}

###########################################################################

method _equal of Bool (::T $self: T $other!) {
    return $BOOL_FALSE
        if $other!map.elems !=== $self!map.elems;
    my Hash $v1 = $self!map;
    my Hash $v2 = $other!map;
    for $v1.pairs -> $e {
        return $BOOL_FALSE
            if !$v2.exists($e.key);
        return $BOOL_FALSE
            if !$e.value.equal( :other($v2.{$e.key}) );
    }
    return $BOOL_TRUE;
}

###########################################################################

method map of Hash () {
    return $!map;
}

###########################################################################

method elem_count of Int () {
    return $!map.elems;
}

method elem_exists of Bool (Str :$elem_name!) {
    return $!map.exists($elem_name);
}

method elem_value of Muldis::DB::Engine::Example::PhysType::_TypeInvo
        (Str :$elem_name!) {
    return $!map{$elem_name};
}

###########################################################################

} # role Muldis::DB::Engine::Example::PhysType::_ValueDict

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::ValueDict {
    does Muldis::DB::Engine::Example::PhysType::_ValueDict;
    submethod BUILD {} # otherwise Pugs r16488 invo ValueDict.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_FALSE; }
} # class Muldis::DB::Engine::Example::PhysType::ValueDict

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::PhysType::QuasiValueDict {
    does Muldis::DB::Engine::Example::PhysType::_ValueDict;
    submethod BUILD {} # otherwise Pugs r16488 invo ValueDict.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_TRUE; }
} # class Muldis::DB::Engine::Example::PhysType::QuasiValueDict

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Muldis::DB::Engine::Example::PhysType -
Physical representations of all core data types

=head1 VERSION

This document describes Muldis::DB::Engine::Example::PhysType version 0.3.0
for Perl 6.

It also describes the same-number versions for Perl 6 of [...].

=head1 DESCRIPTION

This file is used internally by L<Muldis::DB::Engine::Example>; it is not
intended to be used directly in user code.

It provides physical representations of data types that this Example Engine
uses to implement Muldis D.  The API of these is expressly not intended to
match the API that the language itself specifies as possible
representations for system-defined data types.

Specifically, this file represents the core system-defined data types that
all Muldis D implementations must have, namely: Bool, Text, Blob, Int, Num,
Tuple, Relation, and the Cat.* types.

By contrast, the optional data types are given physical representations by
other files: L<Muldis::DB::Engine::Example::PhysType::Temporal>,
L<Muldis::DB::Engine::Example::PhysType::Spatial>.

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

This file is part of the Muldis::DB framework.

Muldis::DB is Copyright © 2002-2007, Darren Duncan.

See the LICENSE AND COPYRIGHT of L<Muldis::DB> for details.

=cut
