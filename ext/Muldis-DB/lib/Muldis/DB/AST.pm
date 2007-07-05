use v6-alpha;

###########################################################################
###########################################################################

my Bool $BOOL_FALSE = Bool::False;
my Bool $BOOL_TRUE  = Bool::True;

my Order $ORDER_INCREASE = (1 <=> 2);
my Order $ORDER_SAME     = (1 <=> 1);
my Order $ORDER_DECREASE = (2 <=> 1);

my $TYNM_UINT = ::Muldis::DB::AST::EntityName.new( :text<sys.type.UInt> );
my $TYNM_PINT = ::Muldis::DB::AST::EntityName.new( :text<sys.type.PInt> );

my $ATNM_VALUE = ::Muldis::DB::AST::EntityName.new( :text<value> );
my $ATNM_INDEX = ::Muldis::DB::AST::EntityName.new( :text<index> );
my $ATNM_COUNT = ::Muldis::DB::AST::EntityName.new( :text<count> );

my $SCA_TYPE_UINT = ::Muldis::DB::AST::TypeInvoNQ.new(
    :kind<Scalar>, :spec($TYNM_UINT) );
my $SCA_TYPE_PINT = ::Muldis::DB::AST::TypeInvoNQ.new(
    :kind<Scalar>, :spec($TYNM_PINT) );

###########################################################################
###########################################################################

module Muldis::DB::AST-0.0.1 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub newBoolLit of Muldis::DB::AST::BoolLit (Bool :$v!) is export {
    return ::Muldis::DB::AST::BoolLit.new( :v($v) );
}

sub newOrderLit of Muldis::DB::AST::OrderLit (Order :$v!) is export {
    return ::Muldis::DB::AST::OrderLit.new( :v($v) );
}

sub newIntLit of Muldis::DB::AST::IntLit (Int :$v!) is export {
    return ::Muldis::DB::AST::IntLit.new( :v($v) );
}

sub newBlobLit of Muldis::DB::AST::BlobLit (Blob :$v!) is export {
    return ::Muldis::DB::AST::BlobLit.new( :v($v) );
}

sub newTextLit of Muldis::DB::AST::TextLit (Str :$v!) is export {
    return ::Muldis::DB::AST::TextLit.new( :v($v) );
}

sub newTupleSel of Muldis::DB::AST::TupleSel
        (Muldis::DB::AST::TypeDictNQ :$heading!,
        Muldis::DB::AST::ExprDict :$body!) is export {
    return ::Muldis::DB::AST::TupleSel.new(
        :heading($heading), :body($body) );
}

sub newQuasiTupleSel of Muldis::DB::AST::QuasiTupleSel
        (Muldis::DB::AST::TypeDictAQ :$heading!,
        Muldis::DB::AST::ExprDict :$body!) is export {
    return ::Muldis::DB::AST::QuasiTupleSel.new(
        :heading($heading), :body($body) );
}

sub newRelationSel of Muldis::DB::AST::RelationSel
        (Muldis::DB::AST::TypeDictNQ :$heading!, Array :$body!) is export {
    return ::Muldis::DB::AST::RelationSel.new(
        :heading($heading), :body($body) );
}

sub newQuasiRelationSel of Muldis::DB::AST::QuasiRelationSel
        (Muldis::DB::AST::TypeDictAQ :$heading!, Array :$body!) is export {
    return ::Muldis::DB::AST::QuasiRelationSel.new(
        :heading($heading), :body($body) );
}

sub newDefault of Muldis::DB::AST::Default
        (Muldis::DB::AST::TypeInvo :$of!) is export {
    return ::Muldis::DB::AST::Default.new( :of($of) );
}

sub newTreat of Muldis::DB::AST::Treat (Muldis::DB::AST::TypeInvo :$as!,
        Muldis::DB::AST::Expr :$v!) is export {
    return ::Muldis::DB::AST::Treat.new( :as($as), :v($v) );
}

sub newVarInvo of Muldis::DB::AST::VarInvo
        (Muldis::DB::AST::EntityName :$v!) is export {
    return ::Muldis::DB::AST::VarInvo.new( :v($v) );
}

sub newFuncInvo of Muldis::DB::AST::FuncInvo
        (Muldis::DB::AST::EntityName :$func!,
        Muldis::DB::AST::ExprDict :$ro_args!) is export {
    return ::Muldis::DB::AST::FuncInvo.new(
        :func($func), :ro_args($ro_args) );
}

sub newProcInvo of Muldis::DB::AST::ProcInvo
        (Muldis::DB::AST::EntityName :$proc!,
        Muldis::DB::AST::ExprDict :$upd_args!,
        Muldis::DB::AST::ExprDict :$ro_args!) is export {
    return ::Muldis::DB::AST::ProcInvo.new(
        :proc($proc), :upd_args($upd_args), :ro_args($ro_args) );
}

sub newFuncReturn of Muldis::DB::AST::FuncReturn
        (Muldis::DB::AST::Expr :$v!) is export {
    return ::Muldis::DB::AST::FuncReturn.new( :v($v) );
}

sub newProcReturn of Muldis::DB::AST::ProcReturn () is export {
    return ::Muldis::DB::AST::ProcReturn.new();
}

multi sub newEntityName of Muldis::DB::AST::EntityName
        (Str :$text!) is export {
    return ::Muldis::DB::AST::EntityName.new( :text($text) );
}

=pod
multi sub newEntityName of Muldis::DB::AST::EntityName
        (Array :$seq!) is export {
    return ::Muldis::DB::AST::EntityName.new( :seq($seq) );
}
=cut

sub newTypeInvoNQ of Muldis::DB::AST::TypeInvoNQ
        (Str :$kind!, Any :$spec!) is export {
    return ::Muldis::DB::AST::TypeInvoNQ.new( :kind($kind), :spec($spec) );
}

sub newTypeInvoAQ of Muldis::DB::AST::TypeInvoAQ
        (Str :$kind!, Any :$spec!) is export {
    return ::Muldis::DB::AST::TypeInvoAQ.new( :kind($kind), :spec($spec) );
}

sub newTypeDictNQ of Muldis::DB::AST::TypeDictNQ (Array :$map!) is export {
    return ::Muldis::DB::AST::TypeDictNQ.new( :map($map) );
}

sub newTypeDictAQ of Muldis::DB::AST::TypeDictAQ (Array :$map!) is export {
    return ::Muldis::DB::AST::TypeDictAQ.new( :map($map) );
}

sub newExprDict of Muldis::DB::AST::ExprDict (Array :$map!) is export {
    return ::Muldis::DB::AST::ExprDict.new( :map($map) );
}

sub newFuncDecl of Muldis::DB::AST::FuncDecl () is export {
    return ::Muldis::DB::AST::FuncDecl.new();
}

sub newProcDecl of Muldis::DB::AST::ProcDecl () is export {
    return ::Muldis::DB::AST::ProcDecl.new();
}

sub newHostGateRtn of Muldis::DB::AST::HostGateRtn
        (Muldis::DB::AST::TypeDict :$upd_params!,
        Muldis::DB::AST::TypeDict :$ro_params!,
        Muldis::DB::AST::TypeDict :$vars!, Array :$stmts!) is export {
    return ::Muldis::DB::AST::HostGateRtn.new( :upd_params($upd_params),
        :ro_params($ro_params), :vars($vars), :stmts($stmts) );
}

###########################################################################

sub newSetSel of Muldis::DB::AST::RelationSel
        (Muldis::DB::AST::TypeInvoNQ :$heading!, Array :$body!) is export {

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$body.defined or !$body.does(Array);

    return ::Muldis::DB::AST::RelationSel.new(
        :heading(::Muldis::DB::AST::TypeDictNQ.new( :map([
            [$ATNM_VALUE, $heading],
        ]) )),
        :body([$body.map:{
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$ATNM_VALUE, $_],
            ]) ),
        }]),
    );
}

sub newQuasiSetSel of Muldis::DB::AST::QuasiRelationSel
        (Muldis::DB::AST::TypeInvoAQ :$heading!, Array :$body!) is export {

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$body.defined or !$body.does(Array);

    return ::Muldis::DB::AST::QuasiRelationSel.new(
        :heading(::Muldis::DB::AST::TypeDictAQ.new( :map([
            [$ATNM_VALUE, $heading],
        ]) )),
        :body([$body.map:{
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$ATNM_VALUE, $_],
            ]) ),
        }]),
    );
}

sub newMaybeSel of Muldis::DB::AST::RelationSel
        (Muldis::DB::AST::TypeInvoNQ :$heading!, Array :$body!) is export {

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ Array-doing class, or it doesn't have 0..1 elements.}
        if !$body.defined or !$body.does(Array) or $body.elems > 1;

    return ::Muldis::DB::AST::RelationSel.new(
        :heading(::Muldis::DB::AST::TypeDictNQ.new( :map([
            [$ATNM_VALUE, $heading],
        ]) )),
        :body([$body.map:{
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$ATNM_VALUE, $_],
            ]) ),
        }]),
    );
}

sub newQuasiMaybeSel of Muldis::DB::AST::QuasiRelationSel
        (Muldis::DB::AST::TypeInvoNQ :$heading!, Array :$body!) is export {

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ Array-doing class, or it doesn't have 0..1 elements.}
        if !$body.defined or !$body.does(Array) or $body.elems > 1;

    return ::Muldis::DB::AST::QuasiRelationSel.new(
        :heading(::Muldis::DB::AST::TypeDictAQ.new( :map([
            [$ATNM_VALUE, $heading],
        ]) )),
        :body([$body.map:{
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$ATNM_VALUE, $_],
            ]) ),
        }]),
    );
}

sub newSeqSel of Muldis::DB::AST::RelationSel
        (Muldis::DB::AST::TypeInvoNQ :$heading!, Array :$body!) is export {

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$body.defined or !$body.does(Array);
    for $body -> $tbody {
        die q{new(): Bad :$body arg elem; it is not an object of a}
                ~ q{ Array-doing class, or it doesn't have 2 elements.}
            if !$tbody.defined or !$tbody.does(Array) or $tbody.elems != 2;
    }

    return ::Muldis::DB::AST::RelationSel.new(
        :heading(::Muldis::DB::AST::TypeDictNQ.new( :map([
            [$ATNM_INDEX, $SCA_TYPE_UINT],
            [$ATNM_VALUE, $heading],
        ]) )),
        :body([$body.map:{
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$ATNM_INDEX, .[0]],
                [$ATNM_VALUE, .[1]],
            ]) ),
        }]),
    );
}

sub newQuasiSeqSel of Muldis::DB::AST::QuasiRelationSel
        (Muldis::DB::AST::TypeInvoAQ :$heading!, Array :$body!) is export {

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$body.defined or !$body.does(Array);
    for $body -> $tbody {
        die q{new(): Bad :$body arg elem; it is not an object of a}
                ~ q{ Array-doing class, or it doesn't have 2 elements.}
            if !$tbody.defined or !$tbody.does(Array) or $tbody.elems != 2;
    }

    return ::Muldis::DB::AST::QuasiRelationSel.new(
        :heading(::Muldis::DB::AST::TypeDictAQ.new( :map([
            [$ATNM_INDEX, $SCA_TYPE_UINT],
            [$ATNM_VALUE, $heading],
        ]) )),
        :body([$body.map:{
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$ATNM_INDEX, .[0]],
                [$ATNM_VALUE, .[1]],
            ]) ),
        }]),
    );
}

sub newBagSel of Muldis::DB::AST::RelationSel
        (Muldis::DB::AST::TypeInvoNQ :$heading!, Array :$body!) is export {

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$body.defined or !$body.does(Array);
    for $body -> $tbody {
        die q{new(): Bad :$body arg elem; it is not an object of a}
                ~ q{ Array-doing class, or it doesn't have 2 elements.}
            if !$tbody.defined or !$tbody.does(Array) or $tbody.elems != 2;
    }

    return ::Muldis::DB::AST::RelationSel.new(
        :heading(::Muldis::DB::AST::TypeDictNQ.new( :map([
            [$ATNM_VALUE, $heading],
            [$ATNM_COUNT, $SCA_TYPE_PINT],
        ]) )),
        :body([$body.map:{
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$ATNM_VALUE, .[0]],
                [$ATNM_COUNT, .[1]],
            ]) ),
        }]),
    );
}

sub newQuasiBagSel of Muldis::DB::AST::QuasiRelationSel
        (Muldis::DB::AST::TypeInvoAQ :$heading!, Array :$body!) is export {

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$body.defined or !$body.does(Array);
    for $body -> $tbody {
        die q{new(): Bad :$body arg elem; it is not an object of a}
                ~ q{ Array-doing class, or it doesn't have 2 elements.}
            if !$tbody.defined or !$tbody.does(Array) or $tbody.elems != 2;
    }

    return ::Muldis::DB::AST::QuasiRelationSel.new(
        :heading(::Muldis::DB::AST::TypeDictAQ.new( :map([
            [$ATNM_VALUE, $heading],
            [$ATNM_COUNT, $SCA_TYPE_PINT],
        ]) )),
        :body([$body.map:{
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$ATNM_VALUE, .[0]],
                [$ATNM_COUNT, .[1]],
            ]) ),
        }]),
    );
}

###########################################################################

} # module Muldis::DB::AST

###########################################################################
###########################################################################

role Muldis::DB::AST::Node {

###########################################################################

method as_perl {
    die q{not implemented by subclass } ~ self.WHAT;
}

###########################################################################

method equal_repr of Bool (Muldis::DB::AST::Node :$other!) {

    die q{equal_repr(): Bad :$other arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::Node-doing class.}
        if !$other.defined or !$other.does(::Muldis::DB::AST::Node);

    return $BOOL_FALSE
        if $other.WHAT !=== self.WHAT;

    return self._equal_repr( $other );
}

method _equal_repr {
    die q{not implemented by subclass } ~ self.WHAT;
}

###########################################################################

} # role Muldis::DB::AST::Node

###########################################################################
###########################################################################

role Muldis::DB::AST::Expr {
    does Muldis::DB::AST::Node;
} # role Muldis::DB::AST::Expr

###########################################################################
###########################################################################

role Muldis::DB::AST::Lit {
    does Muldis::DB::AST::Expr;
} # role Muldis::DB::AST::Lit

###########################################################################
###########################################################################

class Muldis::DB::AST::BoolLit {
    does Muldis::DB::AST::Lit;

    has Bool $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Bool :$v!) {

    die q{new(): Bad :$v arg; it is not an object of a Bool-doing class.}
        if !$v.defined or !$v.does(Bool);

    $!v = ?$v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = $!v ?? 'Bool::True' !! 'Bool::False';
        $!as_perl = "Muldis::DB::AST::BoolLit.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Bool () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::AST::BoolLit

###########################################################################
###########################################################################

class Muldis::DB::AST::OrderLit {
    does Muldis::DB::AST::Lit;

    has Order $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Order :$v!) {

    die q{new(): Bad :$v arg; it is not an object of a Order-doing class.}
        if !$v.defined or !$v.does(Order);

    $!v = $v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = $!v === $ORDER_INCREASE ?? '(1 <=> 2)'
            !! $!v === $ORDER_SAME ?? '(1 <=> 1)' !! '(2 <=> 1)';
        $!as_perl = "Muldis::DB::AST::OrderLit.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Order (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Order () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::AST::OrderLit

###########################################################################
###########################################################################

class Muldis::DB::AST::IntLit {
    does Muldis::DB::AST::Lit;

    has Int $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Int :$v!) {

    die q{new(): Bad :$v arg; it is not an object of a Int-doing class.}
        if !$v.defined or !$v.does(Int);

    $!v = +$v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = ~$!v;
        $!as_perl = "Muldis::DB::AST::IntLit.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Int () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::AST::IntLit

###########################################################################
###########################################################################

class Muldis::DB::AST::BlobLit {
    does Muldis::DB::AST::Lit;

    has Blob $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Blob :$v!) {

    die q{new(): Bad :$v arg; it is not an object of a Blob-doing class.}
        if !$v.defined or !$v.does(Blob);

    $!v = $v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        # TODO: A proper job of encoding/decoding the bit string payload.
        # What you see below is more symbolic of what to do than correct.
        my Str $hex_digit_text = join q{}, map { unpack 'H2', $_ }
            split q{}, $!v;
        my Str $s = q[(join q{}, map { pack 'H2', $_ }
            split rx/<?null>/, ] ~ $hex_digit_text ~ q[)];
        $!as_perl = "Muldis::DB::AST::BlobLit.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Blob () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::AST::BlobLit

###########################################################################
###########################################################################

class Muldis::DB::AST::TextLit {
    does Muldis::DB::AST::Lit;

    has Str $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Str :$v!) {

    die q{new(): Bad :$v arg; it is not an object of a Str-doing class.}
        if !$v.defined or !$v.does(Str);

    $!v = ~$v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s
            = q{'} ~ $!v.trans( q{\\} => q{\\\\}, q{'} => q{\\'} ) ~ q{'};
        $!as_perl = "Muldis::DB::AST::TextLit.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $other!v === $self!v;
}

###########################################################################

method v of Str () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::AST::TextLit

###########################################################################
###########################################################################

role Muldis::DB::AST::_Tuple {
    does Muldis::DB::AST::Expr;

    has Muldis::DB::AST::TypeDict $!heading;
    has Muldis::DB::AST::ExprDict $!body;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Muldis::DB::AST::TypeDict :$heading!,
        Muldis::DB::AST::ExprDict :$body!) {

    if self._allows_quasi() {
        die q{new(): Bad :$heading arg; it is not an object of a}
                ~ q{ Muldis::DB::AST::TypeDictAQ-doing class.}
            if !$heading.defined
                or !$heading.does(::Muldis::DB::AST::TypeDictAQ);
    }
    else {
        die q{new(): Bad :$heading arg; it is not an object of a}
                ~ q{ Muldis::DB::AST::TypeDictNQ-doing class.}
            if !$heading.defined
                or !$heading.does(::Muldis::DB::AST::TypeDictNQ);
    }
    my Int $heading_attrs_count = $heading.elem_count();
    my Hash $heading_attrs_map_hoa = $heading!map_hoa;

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::ExprDict-doing class.}
        if !$body.defined or !$body.does(::Muldis::DB::AST::ExprDict);
    die q{new(): new(): Bad :$body arg; it does not have the}
            ~ q{ same attr count as :$heading.}
        if $body.elem_count() !=== $heading_attrs_count;
    for $body!map_hoa.keys -> $attr_name_text {
        die q{new(): Bad :$body arg; at least one its attrs}
                ~ q{ does not have a corresponding attr in :$heading.}
            if !$heading_attrs_map_hoa.exists($attr_name_text);
    }

    $!heading = $heading;
    $!body    = $body;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $sh = $!heading.as_perl();
        my Str $sb = $!body.as_perl();
        $!as_perl = "{self.WHAT}.new( :heading($sh), :body($sb) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return ($self!heading.equal_repr( :other($other!heading) )
        and $self!body.equal_repr( :other($other!body) ));
}

###########################################################################

method heading of Muldis::DB::AST::TypeDict () {
    return $!heading;
}

method body of Muldis::DB::AST::ExprDict () {
    return $!body;
}

###########################################################################

method attr_count of Int () {
    return $!heading.elem_count();
}

method attr_exists of Bool (Muldis::DB::AST::EntityName :$attr_name!) {
    return $!heading.elem_exists( :elem_name($attr_name) );
}

method attr_type of Muldis::DB::AST::TypeInvo
        (Muldis::DB::AST::EntityName :$attr_name!) {
    return $!heading.elem_value( :elem_name($attr_name) );
}

method attr_value of Muldis::DB::AST::Expr
        (Muldis::DB::AST::EntityName :$attr_name!) {
    return $!body.elem_value( :elem_name($attr_name) );
}

###########################################################################

} # role Muldis::DB::AST::_Tuple

###########################################################################
###########################################################################

class Muldis::DB::AST::TupleSel {
    does Muldis::DB::AST::_Tuple;
    submethod BUILD {} # otherwise Pugs r16488 invo _Tuple.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_FALSE; }
} # class Muldis::DB::AST::TupleSel

###########################################################################
###########################################################################

class Muldis::DB::AST::QuasiTupleSel {
    does Muldis::DB::AST::_Tuple;
    submethod BUILD {} # otherwise Pugs r16488 invo _Tuple.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_TRUE; }
} # class Muldis::DB::AST::QuasiTupleSel

###########################################################################
###########################################################################

role Muldis::DB::AST::_Relation {
    does Muldis::DB::AST::Expr;

    has Muldis::DB::AST::TypeDict $!heading;
    has Array                     $!body;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Muldis::DB::AST::TypeDict :$heading!, Array :$body!) {

    if self._allows_quasi() {
        die q{new(): Bad :$heading arg; it is not an object of a}
                ~ q{ Muldis::DB::AST::TypeDictAQ-doing class.}
            if !$heading.defined
                or !$heading.does(::Muldis::DB::AST::TypeDictAQ);
    }
    else {
        die q{new(): Bad :$heading arg; it is not an object of a}
                ~ q{ Muldis::DB::AST::TypeDictNQ-doing class.}
            if !$heading.defined
                or !$heading.does(::Muldis::DB::AST::TypeDictNQ);
    }
    my Int $heading_attrs_count = $heading.elem_count();
    my Hash $heading_attrs_map_hoa = $heading!map_hoa;

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$body.defined or !$body.does(Array);
    for $body -> $tupb {
        die q{new(): Bad :$body arg elem; it is not an object of a}
                ~ q{ Muldis::DB::AST::ExprDict-doing class.}
            if !$tupb.defined or !$tupb.does(::Muldis::DB::AST::ExprDict);
        die q{new(): new(): Bad :$body arg elem; it does not have the}
                ~ q{ same attr count as :$heading.}
            if $tupb.elem_count() !=== $heading_attrs_count;
        for $tupb!map_hoa.keys -> $attr_name_text {
            die q{new(): Bad :$body arg elem; at least one its attrs}
                    ~ q{ does not have a corresponding attr in :$heading.}
                if !$heading_attrs_map_hoa.exists($attr_name_text);
        }
    }

    $!heading = $heading;
    $!body    = [$body.values];

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $sh = $!heading.as_perl();
        my Str $sb = q{[} ~ $!body.map:{ .as_perl() }.join( q{, } ) ~ q{]};
        $!as_perl = "{self.WHAT}.new( :heading($sh), :body($sb) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $BOOL_FALSE
        if !$self!heading.equal_repr( :other($other!heading) );
    my Array $v1 = $self!body;
    my Array $v2 = $other!body;
    return $BOOL_FALSE
        if $v2.elems !=== $v1.elems;
    for 0..^$v1.elems -> $i {
        return $BOOL_FALSE
            if !$v1.[$i].equal_repr( :other($v2.[$i]) );
    }
    return $BOOL_TRUE;
}

###########################################################################

method heading of Muldis::DB::AST::TypeDict () {
    return $!heading;
}

method body of Array () {
    return [$!body.values];
}

###########################################################################

method body_repr_elem_count of Int () {
    return $!body.elems;
}

###########################################################################

method attr_count of Int () {
    return $!heading.elem_count();
}

method attr_exists of Bool (Muldis::DB::AST::EntityName :$attr_name!) {
    return $!heading.elem_exists( :elem_name($attr_name) );
}

method attr_type of Muldis::DB::AST::TypeInvo
        (Muldis::DB::AST::EntityName :$attr_name!) {
    return $!heading.elem_value( :elem_name($attr_name) );
}

method attr_values of Array (Muldis::DB::AST::EntityName :$attr_name!) {
    return [$!body.map:{ .elem_value( :elem_name($attr_name) ) }];
}

###########################################################################

method heading_of_SSBM of Muldis::DB::AST::TypeInvo () {
    return $!heading.elem_value( :elem_name($ATNM_VALUE) );
}

method body_of_Set of Array () {
    return [$!body.map:{ .elem_value( :elem_name($ATNM_VALUE) ) }];
}

method body_of_Seq of Array () {
    return [$!body.map:{ [
            .elem_value( :elem_name($ATNM_INDEX) ),
            .elem_value( :elem_name($ATNM_VALUE) ),
        ] }];
}

method body_of_Bag of Array () {
    return [$!body.map:{ [
            .elem_value( :elem_name($ATNM_VALUE) ),
            .elem_value( :elem_name($ATNM_COUNT) ),
        ] }];
}

method body_of_Maybe of Array () {
    return [$!body.map:{ .elem_value( :elem_name($ATNM_VALUE) ) }];
}

###########################################################################

} # role Muldis::DB::AST::_Relation

###########################################################################
###########################################################################

class Muldis::DB::AST::RelationSel {
    does Muldis::DB::AST::_Relation;
    submethod BUILD {} # otherwise Pugs r16488 invo _Relation.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_FALSE; }
} # class Muldis::DB::AST::RelationSel

###########################################################################
###########################################################################

class Muldis::DB::AST::QuasiRelationSel {
    does Muldis::DB::AST::_Relation;
    submethod BUILD {} # otherwise Pugs r16488 invo _Relation.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_TRUE; }
} # class Muldis::DB::AST::QuasiRelationSel

###########################################################################
###########################################################################

class Muldis::DB::AST::Default {
    does Muldis::DB::AST::Expr;

    has Muldis::DB::AST::TypeInvo $!of;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Muldis::DB::AST::TypeInvo :$of!) {

    die q{new(): Bad :$of arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::TypeInvo-doing class.}
        if !$of.defined or !$of.does(::Muldis::DB::AST::TypeInvo);

    $!of = $of;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $so = $!of.as_perl();
        $!as_perl = "Muldis::DB::AST::Default.new( :of($so) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $self!of.equal_repr( :other($other!of) );
}

###########################################################################

method of () of Muldis::DB::AST::TypeInvo {
    return $!of;
}

###########################################################################

} # class Muldis::DB::AST::Default

###########################################################################
###########################################################################

class Muldis::DB::AST::Treat {
    does Muldis::DB::AST::Expr;

    has Muldis::DB::AST::TypeInvo $!as;
    has Muldis::DB::AST::Expr     $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Muldis::DB::AST::TypeInvo :$as!,
        Muldis::DB::AST::Expr :$v!) {

    die q{new(): Bad :$as arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::TypeInvo-doing class.}
        if !$as.defined or !$as.does(::Muldis::DB::AST::TypeInvo);

    die q{new(): Bad :$v arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::Expr-doing class.}
        if !$v.defined or !$v.does(::Muldis::DB::AST::Expr);

    $!as = $as;
    $!v    = $v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $sa = $!as.as_perl();
        my Str $sv = $!v.as_perl();
        $!as_perl = "Muldis::DB::AST::Treat.new( :as($sa), :v($sv) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return ($self!as.equal_repr( :other($other!as) )
        and $self!v.equal_repr( :other($other!v) ));
}

###########################################################################

method as () of Muldis::DB::AST::TypeInvo {
    return $!as;
}

method v of Muldis::DB::AST::Expr () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::AST::Treat

###########################################################################
###########################################################################

class Muldis::DB::AST::VarInvo {
    does Muldis::DB::AST::Expr;

    has Muldis::DB::AST::EntityName $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Muldis::DB::AST::EntityName :$v!) {

    die q{new(): Bad :$v arg; it is not a valid object}
            ~ q{ of a Muldis::DB::AST::EntityName-doing class.}
        if !$v.defined or !$v.does(::Muldis::DB::AST::EntityName);

    $!v = $v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = $!v.as_perl();
        $!as_perl = "Muldis::DB::AST::VarInvo.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $self!v.equal_repr( :other($other!v) );
}

###########################################################################

method v of Muldis::DB::AST::EntityName () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::AST::VarInvo

###########################################################################
###########################################################################

class Muldis::DB::AST::FuncInvo {
    does Muldis::DB::AST::Expr;

    has Muldis::DB::AST::EntityName $!func;
    has Muldis::DB::AST::ExprDict   $!ro_args;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Muldis::DB::AST::EntityName :$func!,
        Muldis::DB::AST::ExprDict :$ro_args!) {

    die q{new(): Bad :$func arg; it is not a valid object}
            ~ q{ of a Muldis::DB::AST::EntityName-doing class.}
        if !$func.defined or !$func.does(::Muldis::DB::AST::EntityName);

    die q{new(): Bad :$ro_args arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::ExprDict-doing class.}
        if !$ro_args.defined
            or !$ro_args.does(::Muldis::DB::AST::ExprDict);

    $!func    = $func;
    $!ro_args = $ro_args;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $sf = $!func.as_perl();
        my Str $sra = $!ro_args.as_perl();
        $!as_perl = "Muldis::DB::AST::FuncInvo.new("
            ~ " :func($sf), :ro_args($sra) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $self!func.equal_repr( :other($other!func) )
        and $self!ro_args.equal_repr( :other($other!ro_args) );
}

###########################################################################

method func of Muldis::DB::AST::EntityName () {
    return $!func;
}

method ro_args of Muldis::DB::AST::ExprDict () {
    return $!ro_args;
}

###########################################################################

} # class Muldis::DB::AST::FuncInvo

###########################################################################
###########################################################################

role Muldis::DB::AST::Stmt {
    does Muldis::DB::AST::Node;
} # role Muldis::DB::AST::Stmt

###########################################################################
###########################################################################

class Muldis::DB::AST::ProcInvo {
    does Muldis::DB::AST::Stmt;

    has Muldis::DB::AST::EntityName $!proc;
    has Muldis::DB::AST::ExprDict   $!upd_args;
    has Muldis::DB::AST::ExprDict   $!ro_args;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Muldis::DB::AST::EntityName :$proc!,
        Muldis::DB::AST::ExprDict :$upd_args!,
        Muldis::DB::AST::ExprDict :$ro_args!) {

    die q{new(): Bad :$proc arg; it is not a valid object}
            ~ q{ of a Muldis::DB::AST::EntityName-doing class.}
        if !$proc.defined or !$proc.does(::Muldis::DB::AST::EntityName);

    die q{new(): Bad :$upd_args arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::ExprDict-doing class.}
        if !$upd_args.defined
            or !$upd_args.does(::Muldis::DB::AST::ExprDict);
    die q{new(): Bad :$ro_args arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::ExprDict-doing class.}
        if !$ro_args.defined
            or !$ro_args.does(::Muldis::DB::AST::ExprDict);
    my Hash $upd_args_map_hoa = $upd_args!map_hoa;
    for $upd_args_map_hoa.values -> $an_and_vn {
        die q{new(): Bad :$upd_args arg elem expr; it is not}
                ~ q{ an object of a Muldis::DB::AST::VarInvo-doing class.}
            if !$an_and_vn.[1].does(::Muldis::DB::AST::VarInvo);
    }
    confess q{new(): Bad :$upd_args or :$ro_args arg;}
            ~ q{ they both reference at least 1 same procedure param.}
        if any($ro_args!map_hoa.keys) === any($upd_args_map_hoa.keys);

    $!proc     = $proc;
    $!upd_args = $upd_args;
    $!ro_args  = $ro_args;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $sp = $!proc.as_perl();
        my Str $sua = $!upd_args.as_perl();
        my Str $sra = $!ro_args.as_perl();
        $!as_perl = "Muldis::DB::AST::ProcInvo.new("
            ~ " :proc($sp), :upd_args($sua), :ro_args($sra) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $self!proc.equal_repr( :other($other!proc) )
        and $self!upd_args.equal_repr( :other($other!upd_args) )
        and $self!ro_args.equal_repr( :other($other!ro_args) );
}

###########################################################################

method proc of Muldis::DB::AST::EntityName () {
    return $!proc;
}

method upd_args of Muldis::DB::AST::ExprDict () {
    return $!upd_args;
}

method ro_args of Muldis::DB::AST::ExprDict () {
    return $!ro_args;
}

###########################################################################

} # class Muldis::DB::AST::ProcInvo

###########################################################################
###########################################################################

class Muldis::DB::AST::FuncReturn {
    does Muldis::DB::AST::Stmt;

    has Muldis::DB::AST::Expr $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Muldis::DB::AST::Expr :$v!) {

    die q{new(): Bad :$v arg; it is not a valid object}
            ~ q{ of a Muldis::DB::AST::Expr-doing class.}
        if !$v.defined or !$v.does(::Muldis::DB::AST::Expr);

    $!v = $v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = $!v.as_perl();
        $!as_perl = "Muldis::DB::AST::FuncReturn.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $self!v.equal_repr( :other($other!v) );
}

###########################################################################

method v of Muldis::DB::AST::Expr () {
    return $!v;
}

###########################################################################

} # class Muldis::DB::AST::FuncReturn

###########################################################################
###########################################################################

class Muldis::DB::AST::ProcReturn {
    does Muldis::DB::AST::Stmt;

###########################################################################

method as_perl of Str () {
    return 'Muldis::DB::AST::ProcReturn.new()';
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $BOOL_TRUE;
}

###########################################################################

} # class Muldis::DB::AST::ProcReturn

###########################################################################
###########################################################################

class Muldis::DB::AST::EntityName {
    does Muldis::DB::AST::Node;

    has Str   $!text_possrep;
    has Array $!seq_possrep;

    has Str $!as_perl;

###########################################################################

multi submethod BUILD (Str :$text!) {

    die q{new(): Bad :$text arg; it is not an object of a Str-doing class.}
        if !$text.defined or !$text.does(Str);
    die q{new(): Bad :$text arg; it contains character sequences that}
            ~ q{ are invalid within the Text possrep of an EntityName.}
        if $text.match( rx/ \\ $/ ) or $text.match( rx/ \\ <-[bp]> / ); #/

    $!text_possrep = ~$text;
    $!seq_possrep = [(~$text).split( rx/\./ ).map:{ #/
            .trans( < \\p \\b >
                 => < .   \\  > )
        }];

    return;
}

=pod
multi submethod BUILD (Array :$seq!) {

    die q{new(): Bad :$seq arg; it is not an object of a}
            ~ q{ Array-doing class, or it has < 1 elem.}
        if !$seq.defined or !$seq.does(Array) or $seq.elems === 0;
    for $seq -> $seq_e {
        die q{new(): Bad :$seq arg elem;}
                ~ q{ it is not an object of a Str-doing class.}
            if !$seq_e.defined or !$seq_e.does(Str);
    }

    $!text_possrep = $seq.map:{
            (~$_).trans( < \\  .   >
                 => < \\b \\p > )
        }.join( q{.} );
    $!seq_possrep = [$seq.values];

    return;
}
=cut

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s
            = q{'} ~ $!v.trans( q{\\} => q{\\\\}, q{'} => q{\\'} ) ~ q{'};
        $!as_perl = "Muldis::DB::AST::EntityName.new( :text($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $other!text_possrep === $self!text_possrep;
}

###########################################################################

method text of Str () {
    return $!text_possrep;
}

method seq of Array () {
    return [$!seq_possrep.values];
}

###########################################################################

} # class Muldis::DB::AST::EntityName

###########################################################################
###########################################################################

role Muldis::DB::AST::TypeInvo {
    does Muldis::DB::AST::Node;

    has Str $!kind;
    has Any $!spec;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Str :$kind!, Any :$spec!) {

    die q{new(): Bad :$kind arg; it is not an object of a Str-doing class.}
        if !$kind.defined or !$kind.does(Str);

    if $kind === 'Scalar' {
        die q{new(): Bad :$spec arg; it needs to be a valid object}
                ~ q{ of a Muldis::DB::AST::EntityName-doing class}
                ~ q{ when the :$kind arg is 'Scalar'.}
            if !$spec.defined
                or !$spec.does(::Muldis::DB::AST::EntityName);
    }

    elsif $kind === 'Tuple'|'Relation' {
        die q{new(): Bad :$spec arg; it needs to be a valid object}
                ~ q{ of a Muldis::DB::AST::TypeDictNQ-doing class}
                ~ q{ when the :$kind arg is 'Tuple'|'Relation'.}
            if !$spec.defined
                or !$spec.does(::Muldis::DB::AST::TypeDictNQ);
    }

    elsif (!self._allows_quasi()) {
        die q{new(): Bad :$kind arg; it needs to be one of}
            ~ q{ 'Scalar'|'Tuple'|'Relation'.};
    }

    elsif $kind === 'QTuple'|'QRelation' {
        die q{new(): Bad :$spec arg; it needs to be a valid object}
                ~ q{ of a Muldis::DB::AST::TypeDictAQ-doing class}
                ~ q{ when the :$kind arg is 'QTuple'|'QRelation'.}
            if !$spec.defined
                or !$spec.does(::Muldis::DB::AST::TypeDictAQ);
    }

    elsif $kind === 'Any' {
        die q{new(): Bad :$spec arg; it needs to be one of}
                ~ q{ 'Tuple'|'Relation'|'QTuple'|'QRelation'|'Universal'}
                ~ q{ when the :$kind arg is 'Any'.}
            if !$spec.defined or !$spec.does(Str)
                or $spec === none(<Tuple Relation
                    QTuple QRelation Universal>);
    }

    else {
        die q{new(): Bad :$kind arg; it needs to be}
            ~ q{ 'Scalar'|'Tuple'|'Relation'|'QTuple'|'QRelation'|'Any'.};
    }

    $!kind = $kind;
    $!spec = $spec;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $sk = q{'} ~ $!kind ~ q{'};
        my Str $ss
            = $!kind === 'Any' ?? q{'} ~ $!spec ~ q{'} !! $!spec.as_perl();
        $!as_perl = "{self.WHAT}.new( :kind($sk), :spec($ss) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $BOOL_FALSE
        if $other!kind !=== $self!kind;
    return $self!kind === 'Any' ?? $other!spec === $self!spec
        !! $self!spec.equal_repr( :other($other!spec) );
}

###########################################################################

method kind of Str () {
    return $!kind;
}

method spec of Any () {
    return $!spec;
}

###########################################################################

} # role Muldis::DB::AST::TypeInvo

###########################################################################
###########################################################################

class Muldis::DB::AST::TypeInvoNQ {
    does Muldis::DB::AST::TypeInvo;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeInvo.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_FALSE; }
} # class Muldis::DB::AST::TypeInvoNQ

###########################################################################
###########################################################################

class Muldis::DB::AST::TypeInvoAQ {
    does Muldis::DB::AST::TypeInvo;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeInvo.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_TRUE; }
} # class Muldis::DB::AST::TypeInvoAQ

###########################################################################
###########################################################################

role Muldis::DB::AST::TypeDict {
    does Muldis::DB::AST::Node;

    has Array $!map_aoa;
    has Hash  $!map_hoa;

    has Str $!as_perl;

    trusts Muldis::DB::AST::_Tuple;
    trusts Muldis::DB::AST::_Relation;
    trusts Muldis::DB::AST::HostGateRtn;
    trusts Muldis::DB::Interface::HostGateRtn;

###########################################################################

submethod BUILD (Array :$map!) {

    my Bool $allows_quasi = self._allows_quasi();

    die q{new(): Bad :$map arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$map.defined or !$map.does(Array);
    my Array $map_aoa = [];
    my Hash  $map_hoa = {};
    for $map -> $elem {
        die q{new(): Bad :$map arg elem; it is not an object of a}
                ~ q{ Array-doing class, or it doesn't have 2 elements.}
            if !$elem.defined or !$elem.does(Array) or $elem.elems != 2;
        my ($entity_name, $type_invo) = $elem.values;
        die q{new(): Bad :$map arg elem; its first elem is not an}
                ~ q{ object of a Muldis::DB::AST::EntityName-doing class.}
            if !$entity_name.defined
                or !$entity_name.does(::Muldis::DB::AST::EntityName);
        my Str $entity_name_text = $entity_name.text();
        die q{new(): Bad :$map arg elem; its first elem is not}
                ~ q{ distinct between the arg elems.}
            if $map_hoa.exists($entity_name_text);
        if $allows_quasi {
            die q{new(): Bad :$map arg elem; its second elem is not an}
                    ~ q{ object of a Muldis::DB::AST::TypeInvoAQ-doing}
                    ~ q{ class.}
                if !$type_invo.defined
                    or !$type_invo.does(::Muldis::DB::AST::TypeInvoAQ);
        }
        else {
            die q{new(): Bad :$map arg elem; its second elem is not an}
                    ~ q{ object of a Muldis::DB::AST::TypeInvoNQ-doing}
                    ~ q{ class.}
                if !$type_invo.defined
                    or !$type_invo.does(::Muldis::DB::AST::TypeInvoNQ);
        }
        my Array $elem_cpy = [$entity_name, $type_invo];
        $map_aoa.push( $elem_cpy );
        $map_hoa{$entity_name_text} = $elem_cpy;
    }

    $!map_aoa = $map_aoa;
    $!map_hoa = $map_hoa;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = q{[} ~ $!map_aoa.map:{
                q{[} ~ .[0].as_perl() ~ q{, } ~ .[1].as_perl() ~ q{]}
            }.join( q{, } ) ~ q{]};
        $!as_perl = "Muldis::DB::AST::TypeDict.new( :map($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $BOOL_FALSE
        if $other!map_aoa.elems !=== $self!map_aoa.elems;
    my Hash $v1 = $self!map_hoa;
    my Hash $v2 = $other!map_hoa;
    for $v1.pairs -> $e {
        return $BOOL_FALSE
            if !$v2.exists($e.key);
        return $BOOL_FALSE
            if !$e.value.[1].equal_repr( :other($v2.{$e.key}.[1]) );
    }
    return $BOOL_TRUE;
}

###########################################################################

method map of Array () {
    return [$!map_aoa.map:{ [.values] }];
}

method map_hoa of Hash () {
    return {$!map_hoa.pairs.map:{ .key => [.value.values] }};
}

###########################################################################

method elem_count of Int () {
    return $!map_aoa.elems;
}

method elem_exists of Bool (Muldis::DB::AST::EntityName :$elem_name!) {

    die q{elem_exists(): Bad :$elem_name arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::EntityName-doing class.}
        if !$elem_name.defined
            or !$elem_name.does(::Muldis::DB::AST::EntityName);

    return $!map_hoa.exists($elem_name.text());
}

method elem_value of Muldis::DB::AST::TypeInvo
        (Muldis::DB::AST::EntityName :$elem_name!) {

    die q{elem_value(): Bad :$elem_name arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::EntityName-doing class.}
        if !$elem_name.defined
            or !$elem_name.does(::Muldis::DB::AST::EntityName);
    my Str $elem_name_text = $elem_name.text();

    die q{elem_value(): Bad :$elem_name arg; it matches no dict elem.}
        if !$!map_hoa.exists($elem_name_text);

    return $!map_hoa{$elem_name_text};
}

###########################################################################

} # role Muldis::DB::AST::TypeDict

###########################################################################
###########################################################################

class Muldis::DB::AST::TypeDictNQ {
    does Muldis::DB::AST::TypeDict;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeDict.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_FALSE; }
} # class Muldis::DB::AST::TypeDictNQ

###########################################################################
###########################################################################

class Muldis::DB::AST::TypeDictAQ {
    does Muldis::DB::AST::TypeDict;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeDict.BUILD twice
    method _allows_quasi of Bool () { return $BOOL_TRUE; }
} # class Muldis::DB::AST::TypeDictAQ

###########################################################################
###########################################################################

class Muldis::DB::AST::ExprDict {
    does Muldis::DB::AST::Node;

    has Array $!map_aoa;
    has Hash  $!map_hoa;

    # Note: This type is specific such that values are always some ::Expr,
    # but this type may be later generalized to hold ::Node instead.

    has Str $!as_perl;

    trusts Muldis::DB::AST::_Tuple;
    trusts Muldis::DB::AST::_Relation;
    trusts Muldis::DB::AST::ProcInvo;

###########################################################################

submethod BUILD (Array :$map!) {

    die q{new(): Bad :$map arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$map.defined or !$map.does(Array);
    my Array $map_aoa = [];
    my Hash  $map_hoa = {};
    for $map -> $elem {
        die q{new(): Bad :$map arg elem; it is not an object of a}
                ~ q{ Array-doing class, or it doesn't have 2 elements.}
            if !$elem.defined or !$elem.does(Array) or $elem.elems != 2;
        my ($entity_name, $expr) = $elem.values;
        die q{new(): Bad :$map arg elem; its first elem is not an}
                ~ q{ object of a Muldis::DB::AST::EntityName-doing class.}
            if !$entity_name.defined
                or !$entity_name.does(::Muldis::DB::AST::EntityName);
        my Str $entity_name_text = $entity_name.text();
        die q{new(): Bad :$map arg elem; its first elem is not}
                ~ q{ distinct between the arg elems.}
            if $map_hoa.exists($entity_name_text);
        die q{new(): Bad :$map arg elem; its second elem is not}
                ~ q{ an object of a Muldis::DB::AST::Expr-doing class.}
            if !$expr.defined or !$expr.does(::Muldis::DB::AST::Expr);
        my Array $elem_cpy = [$entity_name, $expr];
        $map_aoa.push( $elem_cpy );
        $map_hoa{$entity_name_text} = $elem_cpy;
    }

    $!map_aoa = $map_aoa;
    $!map_hoa = $map_hoa;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = q{[} ~ $!map_aoa.map:{
                q{[} ~ .[0].as_perl() ~ q{, } ~ .[1].as_perl() ~ q{]}
            }.join( q{, } ) ~ q{]};
        $!as_perl = "Muldis::DB::AST::ExprDict.new( :map($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $BOOL_FALSE
        if $other!map_aoa.elems !=== $self!map_aoa.elems;
    my Hash $v1 = $self!map_hoa;
    my Hash $v2 = $other!map_hoa;
    for $v1.pairs -> $e {
        return $BOOL_FALSE
            if !$v2.exists($e.key);
        return $BOOL_FALSE
            if !$e.value.[1].equal_repr( :other($v2.{$e.key}.[1]) );
    }
    return $BOOL_TRUE;
}

###########################################################################

method map of Array () {
    return [$!map_aoa.map:{ [.values] }];
}

method map_hoa of Hash () {
    return {$!map_hoa.pairs.map:{ .key => [.value.values] }};
}

###########################################################################

method elem_count of Int () {
    return $!map_aoa.elems;
}

method elem_exists of Bool (Muldis::DB::AST::EntityName :$elem_name!) {

    die q{elem_exists(): Bad :$elem_name arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::EntityName-doing class.}
        if !$elem_name.defined
            or !$elem_name.does(::Muldis::DB::AST::EntityName);

    return $!map_hoa.exists($elem_name.text());
}

method elem_value of Muldis::DB::AST::Expr
        (Muldis::DB::AST::EntityName :$elem_name!) {

    die q{elem_value(): Bad :$elem_name arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::EntityName-doing class.}
        if !$elem_name.defined
            or !$elem_name.does(::Muldis::DB::AST::EntityName);
    my Str $elem_name_text = $elem_name.text();

    die q{elem_value(): Bad :$elem_name arg; it matches no dict elem.}
        if !$!map_hoa.exists($elem_name_text);

    return $!map_hoa{$elem_name_text};
}

###########################################################################

} # class Muldis::DB::AST::ExprDict

###########################################################################
###########################################################################

class Muldis::DB::AST::FuncDecl {
    does Muldis::DB::AST::Node;

###########################################################################

submethod BUILD {
    die q{not implemented};
}

###########################################################################

} # class Muldis::DB::AST::FuncDecl

###########################################################################
###########################################################################

class Muldis::DB::AST::ProcDecl {
    does Muldis::DB::AST::Node;

###########################################################################

submethod BUILD {
    die q{not implemented};
}

###########################################################################

} # class Muldis::DB::AST::ProcDecl

###########################################################################
###########################################################################

class Muldis::DB::AST::HostGateRtn {
    does Muldis::DB::AST::Node;

    has Muldis::DB::AST::TypeDict $!upd_params;
    has Muldis::DB::AST::TypeDict $!ro_params;
    has Muldis::DB::AST::TypeDict $!vars;
    has Array                     $!stmts;

    has Str $!as_perl;

    trusts Muldis::DB::Interface::HostGateRtn;

###########################################################################

submethod BUILD (Muldis::DB::AST::TypeDict :$upd_params!,
        Muldis::DB::AST::TypeDict :$ro_params!,
        Muldis::DB::AST::TypeDict :$vars!, Array :$stmts!) {

    die q{new(): Bad :$upd_params arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::TypeDict-doing class.}
        if !$upd_params.defined
            or !$upd_params.does(::Muldis::DB::AST::TypeDict);
    die q{new(): Bad :$ro_params arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::TypeDict-doing class.}
        if !$ro_params.defined
            or !$ro_params.does(::Muldis::DB::AST::TypeDict);
    die q{new(): Bad :$upd_params or :$ro_params arg;}
            ~ q{ they both reference at least 1 same procedure param.}
        if any($ro_params!map_hoa.keys) === any($upd_params!map_hoa.keys);

    die q{new(): Bad :$vars arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::TypeDict-doing class.}
        if !$vars.defined or !$vars.does(::Muldis::DB::AST::TypeDict);

    die q{new(): Bad :$stmts arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$stmts.defined or !$stmts.does(Array);
    for $stmts -> $stmt {
        die q{new(): Bad :$stmts arg elem; it is not}
                ~ q{ an object of a Muldis::DB::AST::Stmt-doing class.}
            if !$stmt.defined or !$stmt.does(::Muldis::DB::AST::Stmt);
    }

    $!upd_params = $upd_params;
    $!ro_params  = $ro_params;
    $!vars       = $vars;
    $!stmts      = [$stmts.values];

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $sup = $!upd_params.as_perl();
        my Str $srp = $!ro_params.as_perl();
        my Str $sv = $!vars.as_perl();
        my Str $ss
            = q{[} ~ $!stmts.map:{ .as_perl() }.join( q{, } ) ~ q{]};
        $!as_perl = "Muldis::DB::AST::HostGateRtn.new( :upd_params($sup)"
            ~ ", :ro_params($srp), :vars($sv), :stmts($ss) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $BOOL_FALSE
        if !$self!upd_params.equal_repr( :other($other!upd_params) )
            or !$self!ro_params.equal_repr( :other($other!ro_params) )
            or !$self!vars.equal_repr( :other($other!vars) );
    my Array $v1 = $self!stmts;
    my Array $v2 = $other!stmts;
    return $BOOL_FALSE
        if $v2.elems !=== $v1.elems;
    for 0..^$v1.elems -> $i {
        return $BOOL_FALSE
            if !$v1.[$i].equal_repr( :other($v2.[$i]) );
    }
    return $BOOL_TRUE;
}

###########################################################################

method upd_params of Muldis::DB::AST::TypeDict () {
    return $!upd_params;
}

method ro_params of Muldis::DB::AST::TypeDict () {
    return $!ro_params;
}

method vars of Muldis::DB::AST::TypeDict () {
    return $!vars;
}

method stmts of Muldis::DB::AST::EntityName () {
    return [$!stmts.values];
}

###########################################################################

} # class Muldis::DB::AST::HostGateRtn

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Muldis::DB::AST -
Abstract syntax tree for the Muldis D language

=head1 VERSION

This document describes Muldis::DB::AST version 0.0.1 for Perl 6.

It also describes the same-number versions for Perl 6 of [...].

=head1 SYNOPSIS

I<This documentation is pending.>

    use Muldis::DB::AST <newBoolLit newOrderLit newIntLit newBlobLit
        newTextLit newTupleSel newQuasiTupleSel newRelationSel
        newQuasiRelationSel newDefault newTreat newVarInvo newFuncInvo
        newProcInvo newFuncReturn newProcReturn newEntityName newTypeInvoNQ
        newTypeInvoAQ newTypeDictNQ newTypeDictAQ newExprDict newFuncDecl
        newProcDecl newHostGateRtn newSetSel newQuasiSetSel newMaybeSel
        newQuasiMaybeSel newSeqSel newQuasiSeqSel newBagSel
        newQuasiBagSel>;

    my $truth_value = newBoolLit( :v(2 + 2 == 4) );
    my $direction = newOrderLit( :v(5 <=> 7) );
    my $answer = newIntLit( :v(42) );
    my $package = newBlobLit( :v(pack 'H2', 'P') );
    my $planetoid = newTextLit( :v<Ceres> );

I<This documentation is pending.>

=head1 DESCRIPTION

The native command language of a L<Muldis::DB> DBMS (database management
system) / virtual machine is called B<Muldis D>; see
L<Muldis::DB::Language> for the language's human readable authoritative
design document.

This library, Muldis::DB::AST ("AST"), provides a few dozen container
classes which collectively implement the I<Abstract> representation format
of Muldis D; each class is called an I<AST node type> or I<node type>, and
an object of one of these classes is called an I<AST node> or I<node>.

These are all of the roles and classes that Muldis::DB::AST defines (more
will be added in the future), which are visually arranged here in their
"does" or "isa" hierarchy, children indented under parents:

    Muldis::DB::AST::Node (dummy role)
        Muldis::DB::AST::Expr (dummy role)
            Muldis::DB::AST::Lit (dummy role)
                Muldis::DB::AST::BoolLit
                Muldis::DB::AST::OrderLit
                Muldis::DB::AST::IntLit
                Muldis::DB::AST::BlobLit
                Muldis::DB::AST::TextLit
            Muldis::DB::AST::_Tuple (implementing role)
                Muldis::DB::AST::TupleSel
                Muldis::DB::AST::QuasiTupleSel
            Muldis::DB::AST::_Relation (implementing role)
                Muldis::DB::AST::RelationSel
                Muldis::DB::AST::QuasiRelationSel
            Muldis::DB::AST::Default
            Muldis::DB::AST::Treat
            Muldis::DB::AST::VarInvo
            Muldis::DB::AST::FuncInvo
        Muldis::DB::AST::Stmt (dummy role)
            Muldis::DB::AST::ProcInvo
            Muldis::DB::AST::FuncReturn
            Muldis::DB::AST::ProcReturn
            # more control-flow statement types would go here
        Muldis::DB::AST::EntityName
        Muldis::DB::AST::TypeInvo (implementing role)
            Muldis::DB::AST::TypeInvoNQ
            Muldis::DB::AST::TypeInvoAQ
        Muldis::DB::AST::TypeDict (implementing role)
            Muldis::DB::AST::TypeDictNQ
            Muldis::DB::AST::TypeDictAQ
        Muldis::DB::AST::ExprDict
        Muldis::DB::AST::FuncDecl
        Muldis::DB::AST::ProcDecl
        # more routine declaration types would go here
        Muldis::DB::AST::HostGateRtn

All Muldis D abstract syntax trees are such in the compositional sense;
that is, every AST node is composed primarily of zero or more other AST
nodes, and so a node is a child of another iff the former is composed into
the latter.  All AST nodes are immutable objects; their values are
determined at construction time, and they can't be changed afterwards.
Therefore, constructing a tree is a bottom-up process, such that all child
objects have to be constructed prior to, and be passed in as constructor
arguments of, their parents.  The process is like declaring an entire
multi-dimensional Perl data structure at the time the variable holding it
is declared; the data structure is actually built from the inside to the
outside.  A consequence of the immutability is that it is feasible to
reuse AST nodes many times, since they won't change out from under you.

An AST node denotes an arbitrarily complex value, that value being defined
by the type of the node and what its attributes are (some of which are
themselves nodes, and some of which aren't).  A node can denote either a
scalar value, or a collection value, or an expression that would evaluate
into a value, or a statement or routine definition that could be later
executed to either return a value or have some side effect.  For all
intents and purposes, a node is a program, and can represent anything that
program code can represent, both values and actions.

The Muldis::DB framework uses Muldis::DB AST nodes for the dual purpose of
defining routines to execute and defining values to use as arguments to and
return values from the execution of said routines.  The C<prepare()> method
of a C<Muldis::DB::Interface::DBMS> object, and by extension the
C<Muldis::DB::Interface::HostGateRtn->new()> constructor function, takes a
C<Muldis::DB::AST::HostGateRtn> node as its primary argument, such that the
AST object defines the source code that is compiled to become the Interface
object.  The C<fetch_ast()> and C<store_ast()> methods of a
C<Muldis::DB::Interface::HostGateVar> object will get or set that object's
primary value attribute, which is any C<Muldis::DB::AST::Node>.  The C<Var>
objects are bound to C<Rtn> objects, and they are the means by which an
executed routine accepts input or provides output at C<execute()> time.

=head2 AST Node Values Versus Representations

In the general case, Muldis::DB AST nodes do not maintain canonical
representations of all Muldis D values, meaning that it is possible and
common to have 2 given AST nodes that logically denote the same value, but
they have different actual compositions.  (Some node types are special
cases for which the aforementioned isn't true; see below.)

For example, a node whose value is just the number 5 can have any number of
representations, each of which is an expression that evaluates to the
number 5 (such as [C<5>, C<2+3>, C<10/2>]).  Another example is a node
whose value is the set C<{3,5,7}>; it can be represented, for example,
either by C<Set(5,3,7,7,7)> or C<Union(Set(3,5),Set(5,7))> or
C<Set(7,5,3)>.  I<These examples aren't actual Muldis::DB AST syntax.>

For various reasons, the Muldis::DB::AST classes themselves do not do any
node refactoring, and their representations differ little if any from the
format of their constructor arguments, which can contain extra information
that is not logically significant in determining the node value.  One
reason is that this allows a semblance of maintaining the actual syntax
that the user specified, which is useful for their debugging purposes.
Another reason is the desire to keep this library as light-weight as
possible, such that it just implements the essentials; doing refactoring
can require a code size and complexity that is orders of magnitude larger
than these essentials, and that work isn't always helpful.  It should also
be noted that any nodes having references to externally user-defined
entities can't be fully refactored as each of those represents a free
variable that a static node analysis can't decompose; only nodes consisting
of just system-defined or literal entities (meaning zero free variables)
can be fully refactored in a static node analysis (though there are a fair
number of those in practice, particularly as C<Var> values).

A consequence of this is that the Muldis::DB::AST classes in general do not
include do not include any methods for comparing that 2 nodes denote the
same value; to reliably do that, you will have to use means not provided by
this library.  However, each class I<does> provide a C<equal_repr> method,
which compares that 2 nodes have the same representation.

It should be noted that a serialize/unserialize cycle on a node that is
done using the C<as_perl> routine to serialize, and having Perl eval that
to unserialize, is guaranteed to preserve the representation, so
C<equal_repr> will work as expected in that situation.

As an exception to the general case about nodes, the node classes
[C<BoolLit>, C<TextLit>, C<BlobLit>, C<IntLit>, C<EntityName>, C<VarInvo>,
C<ProcReturn>] are guaranteed to only ever have a single representation per
value, and so C<equal_repr> is guaranteed to indicate value equality of 2
nodes of those types.  In fact, to assist the consequence this point, these
node classes also have the C<equal_value> method which is an alias for
C<equal_repr>, so you can use C<equal_value> in your use code to make it
better self documenting; C<equal_repr> is still available for all node
types to assist automated use code that wants to treat all node types the
same.  It should also be noted that a C<BoolLit> node can only possibly be
of one of 2 values, and C<ProcReturn> is a singleton.

It is expected that multiple third party utility modules will become
available over time whose purpose is to refactor a Muldis::DB AST node,
either as part of a static analysis that considers only the node in
isolation (and any user-defined entity references have to be treated as
free variables and not generally be factored out), or as part of an Engine
implementation that also considers the current virtual machine environment
and what user-defined entities exist there (and depending on the context,
user-defined entity references don't have to be free variables).

=head1 INTERFACE

The interface of Muldis::DB::AST is fundamentally object-oriented; you use
it by creating objects from its member classes, usually invoking C<new()>
on the appropriate class name, and then invoking methods on those objects.
All of their attributes are private, so you must use accessor methods.

Muldis::DB::AST also provides wrapper subroutines for all member class
constructors, 1 per each, where each subroutine has identical parameters to
the constructor it wraps, and the name of each subroutine is equal to the
trailing part of the class name, specifically the C<Foo> of
C<Muldis::DB::AST::Foo>, but with a C<new> prefix (so that Perl doesn't
confuse a fully-qualified sub name with a class name).  All of these
subroutines are exportable, but are not exported by default, and exist
solely as syntactic sugar to allow user code to have more brevity.  I<TODO:
Reimplement these as lexical aliases or compile-time macros instead, to
avoid the overhead of extra routine calls.>

The usual way that Muldis::DB::AST indicates a failure is to throw an
exception; most often this is due to invalid input.  If an invoked routine
simply returns, you can assume that it has succeeded, even if the return
value is undefined.

=head2 The Muldis::DB::AST::BoolLit Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::OrderLit Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::IntLit Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::BlobLit Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::TextLit Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::TupleSel Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::QuasiTupleSel Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::RelationSel Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::QuasiRelationSel Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::Default Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::Treat Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::VarInvo Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::FuncInvo Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::ProcInvo Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::FuncReturn Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::ProcReturn Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::EntityName Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::TypeInvoNQ Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::TypeInvoAQ Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::TypeDictNQ Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::TypeDictAQ Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::ExprDict Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::FuncDecl Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::ProcDecl Class

I<This documentation is pending.>

=head2 The Muldis::DB::AST::HostGateRtn Class

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

Go to L<Muldis::DB> for the majority of distribution-internal references,
and L<Muldis::DB::SeeAlso> for the majority of distribution-external
references.

=head1 BUGS AND LIMITATIONS

For design simplicity in the short term, all AST arguments that are
applicable must be explicitly defined by the user, even if it might be
reasonable for Muldis::DB to figure out a default value for them, such as
"same as self".  This limitation will probably be removed in the future.
All that said, a few arguments may be exempted from this limitation.

I<This documentation is pending.>

=head1 AUTHOR

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENSE AND COPYRIGHT

This file is part of the Muldis::DB framework.

Muldis::DB is Copyright © 2002-2007, Darren Duncan.

See the LICENSE AND COPYRIGHT of L<Muldis::DB> for details.

=head1 ACKNOWLEDGEMENTS

The ACKNOWLEDGEMENTS in L<Muldis::DB> apply to this file too.

=cut
