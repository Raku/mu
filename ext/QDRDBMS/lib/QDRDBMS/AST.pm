use v6-alpha;

###########################################################################
###########################################################################

my $FALSE = Bool::False;
my $TRUE  = Bool::True;

###########################################################################
###########################################################################

module QDRDBMS::AST-0.0.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub newBoolLit of QDRDBMS::AST::BoolLit (Bool :$v!) is export {
    return ::QDRDBMS::AST::BoolLit.new( :v($v) );
}

sub newTextLit of QDRDBMS::AST::TextLit (Str :$v!) is export {
    return ::QDRDBMS::AST::TextLit.new( :v($v) );
}

sub newBlobLit of QDRDBMS::AST::BlobLit (Blob :$v!) is export {
    return ::QDRDBMS::AST::BlobLit.new( :v($v) );
}

sub newIntLit of QDRDBMS::AST::IntLit (Int :$v!) is export {
    return ::QDRDBMS::AST::IntLit.new( :v($v) );
}

sub newTupleSel of QDRDBMS::AST::TupleSel
        (QDRDBMS::AST::TypeDictNQ :$heading!,
        QDRDBMS::AST::ExprDict :$body!) is export {
    return ::QDRDBMS::AST::TupleSel.new(
        :heading($heading), :body($body) );
}

sub newQuasiTupleSel of QDRDBMS::AST::QuasiTupleSel
        (QDRDBMS::AST::TypeDictAQ :$heading!,
        QDRDBMS::AST::ExprDict :$body!) is export {
    return ::QDRDBMS::AST::QuasiTupleSel.new(
        :heading($heading), :body($body) );
}

sub newRelationSel of QDRDBMS::AST::RelationSel
        (QDRDBMS::AST::TypeDictNQ :$heading!, Array :$body!) is export {
    return ::QDRDBMS::AST::RelationSel.new(
        :heading($heading), :body($body) );
}

sub newQuasiRelationSel of QDRDBMS::AST::QuasiRelationSel
        (QDRDBMS::AST::TypeDictAQ :$heading!, Array :$body!) is export {
    return ::QDRDBMS::AST::QuasiRelationSel.new(
        :heading($heading), :body($body) );
}

sub newVarInvo of QDRDBMS::AST::VarInvo
        (QDRDBMS::AST::EntityName :$v!) is export {
    return ::QDRDBMS::AST::VarInvo.new( :v($v) );
}

sub newFuncInvo of QDRDBMS::AST::FuncInvo
        (QDRDBMS::AST::EntityName :$func!,
        QDRDBMS::AST::ExprDict :$ro_args!) is export {
    return ::QDRDBMS::AST::FuncInvo.new(
        :func($func), :ro_args($ro_args) );
}

sub newProcInvo of QDRDBMS::AST::ProcInvo
        (QDRDBMS::AST::EntityName :$proc!,
        QDRDBMS::AST::ExprDict :$upd_args!,
        QDRDBMS::AST::ExprDict :$ro_args!) is export {
    return ::QDRDBMS::AST::ProcInvo.new(
        :proc($proc), :upd_args($upd_args), :ro_args($ro_args) );
}

sub newFuncReturn of QDRDBMS::AST::FuncReturn
        (QDRDBMS::AST::Expr :$v!) is export {
    return ::QDRDBMS::AST::FuncReturn.new( :v($v) );
}

sub newProcReturn of QDRDBMS::AST::ProcReturn () is export {
    return ::QDRDBMS::AST::ProcReturn.new();
}

multi sub newEntityName of QDRDBMS::AST::EntityName
        (Str :$text!) is export {
    return ::QDRDBMS::AST::EntityName.new( :text($text) );
}

=pod
multi sub newEntityName of QDRDBMS::AST::EntityName
        (Array :$seq!) is export {
    return ::QDRDBMS::AST::EntityName.new( :seq($seq) );
}
=cut

sub newTypeInvoNQ of QDRDBMS::AST::TypeInvoNQ
        (Str :$kind!, Any :$spec!) is export {
    return ::QDRDBMS::AST::TypeInvoNQ.new( :kind($kind), :spec($spec) );
}

sub newTypeInvoAQ of QDRDBMS::AST::TypeInvoAQ
        (Str :$kind!, Any :$spec!) is export {
    return ::QDRDBMS::AST::TypeInvoAQ.new( :kind($kind), :spec($spec) );
}

sub newTypeDictNQ of QDRDBMS::AST::TypeDictNQ (Array :$map!) is export {
    return ::QDRDBMS::AST::TypeDictNQ.new( :map($map) );
}

sub newTypeDictAQ of QDRDBMS::AST::TypeDictAQ (Array :$map!) is export {
    return ::QDRDBMS::AST::TypeDictAQ.new( :map($map) );
}

sub newExprDict of QDRDBMS::AST::ExprDict (Array :$map!) is export {
    return ::QDRDBMS::AST::ExprDict.new( :map($map) );
}

sub newFuncDecl of QDRDBMS::AST::FuncDecl () is export {
    return ::QDRDBMS::AST::FuncDecl.new();
}

sub newProcDecl of QDRDBMS::AST::ProcDecl () is export {
    return ::QDRDBMS::AST::ProcDecl.new();
}

sub newHostGateRtn of QDRDBMS::AST::HostGateRtn
        (QDRDBMS::AST::TypeDict :$upd_params!,
        QDRDBMS::AST::TypeDict :$ro_params!,
        QDRDBMS::AST::TypeDict :$vars!, Array :$stmts!) is export {
    return ::QDRDBMS::AST::HostGateRtn.new( :upd_params($upd_params),
        :ro_params($ro_params), :vars($vars), :stmts($stmts) );
}

###########################################################################

} # module QDRDBMS::AST

###########################################################################
###########################################################################

role QDRDBMS::AST::Node {

###########################################################################

method as_perl {
    die q{not implemented by subclass } ~ self.WHAT;
}

###########################################################################

method equal_repr of Bool (QDRDBMS::AST::Node :$other!) {

    die q{equal_repr(): Bad :$other arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::Node-doing class.}
        if !$other.defined or !$other.does(::QDRDBMS::AST::Node);

    return $FALSE
        if $other.WHAT !=== self.WHAT;

    return self._equal_repr( $other );
}

method _equal_repr {
    die q{not implemented by subclass } ~ self.WHAT;
}

###########################################################################

} # role QDRDBMS::AST::Node

###########################################################################
###########################################################################

role QDRDBMS::AST::Expr {
    does QDRDBMS::AST::Node;
} # role QDRDBMS::AST::Expr

###########################################################################
###########################################################################

role QDRDBMS::AST::Lit {
    does QDRDBMS::AST::Expr;
} # role QDRDBMS::AST::Lit

###########################################################################
###########################################################################

class QDRDBMS::AST::BoolLit {
    does QDRDBMS::AST::Lit;

    has Bool $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Bool :$v!) {

    die q{new(): Bad :$v arg; it is not an object of a Bool-doing class.}
        if !$v.defined or !$v.does(Bool);

    $!v = $v ?? $TRUE !! $FALSE;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = $!v ?? 'Bool::True' !! 'Bool::False';
        $!as_perl = "QDRDBMS::AST::BoolLit.new( :v($s) )";
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

} # class QDRDBMS::AST::BoolLit

###########################################################################
###########################################################################

class QDRDBMS::AST::TextLit {
    does QDRDBMS::AST::Lit;

    has Str $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Str :$v!) {

    die q{new(): Bad :$v arg; it is not an object of a Str-doing class.}
        if !$v.defined or !$v.does(Str);

    $!v = $v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = q{'} ~ $!v.trans( q{'} => q{\\'} ) ~ q{'};
        $!as_perl = "QDRDBMS::AST::TextLit.new( :v($s) )";
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

} # class QDRDBMS::AST::TextLit

###########################################################################
###########################################################################

class QDRDBMS::AST::BlobLit {
    does QDRDBMS::AST::Lit;

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
        $!as_perl = "QDRDBMS::AST::BlobLit.new( :v($s) )";
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

} # class QDRDBMS::AST::BlobLit

###########################################################################
###########################################################################

class QDRDBMS::AST::IntLit {
    does QDRDBMS::AST::Lit;

    has Int $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Int :$v!) {

    die q{new(): Bad :$v arg; it is not an object of a Int-doing class.}
        if !$v.defined or !$v.does(Int);

    $!v = $v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = ~$!v;
        $!as_perl = "QDRDBMS::AST::IntLit.new( :v($s) )";
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

} # class QDRDBMS::AST::IntLit

###########################################################################
###########################################################################

role QDRDBMS::AST::_Tuple {
    does QDRDBMS::AST::Expr;

    has QDRDBMS::AST::TypeDict $!heading;
    has QDRDBMS::AST::ExprDict $!body;

    has Str $!as_perl;

###########################################################################

submethod BUILD (QDRDBMS::AST::TypeDict :$heading!,
        QDRDBMS::AST::ExprDict :$body!) {

    if self._allows_quasi() {
        die q{new(): Bad :$heading arg; it is not an object of a}
                ~ q{ QDRDBMS::AST::TypeDictAQ-doing class.}
            if !$heading.defined
                or !$heading.does(::QDRDBMS::AST::TypeDictAQ);
    }
    else {
        die q{new(): Bad :$heading arg; it is not an object of a}
                ~ q{ QDRDBMS::AST::TypeDictNQ-doing class.}
            if !$heading.defined
                or !$heading.does(::QDRDBMS::AST::TypeDictNQ);
    }
    my Hash $heading_attrs_map_hoa = $heading!map_hoa;

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::ExprDict-doing class.}
        if !$body.defined or !$body.does(::QDRDBMS::AST::ExprDict);
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

method heading of QDRDBMS::AST::TypeDict () {
    return $!heading;
}

###########################################################################

method body of QDRDBMS::AST::ExprDict () {
    return $!body;
}

###########################################################################

} # role QDRDBMS::AST::_Tuple

###########################################################################
###########################################################################

class QDRDBMS::AST::TupleSel {
    does QDRDBMS::AST::_Tuple;
    submethod BUILD {} # otherwise Pugs r16488 invo _Tuple.BUILD twice
    method _allows_quasi of Bool () { return $FALSE; }
} # class QDRDBMS::AST::TupleSel

###########################################################################
###########################################################################

class QDRDBMS::AST::QuasiTupleSel {
    does QDRDBMS::AST::_Tuple;
    submethod BUILD {} # otherwise Pugs r16488 invo _Tuple.BUILD twice
    method _allows_quasi of Bool () { return $TRUE; }
} # class QDRDBMS::AST::QuasiTupleSel

###########################################################################
###########################################################################

role QDRDBMS::AST::_Relation {
    does QDRDBMS::AST::Expr;

    has QDRDBMS::AST::TypeDict $!heading;
    has Array                  $!body;

    has Str $!as_perl;

###########################################################################

submethod BUILD (QDRDBMS::AST::TypeDict :$heading!, Array :$body!) {

    if self._allows_quasi() {
        die q{new(): Bad :$heading arg; it is not an object of a}
                ~ q{ QDRDBMS::AST::TypeDictAQ-doing class.}
            if !$heading.defined
                or !$heading.does(::QDRDBMS::AST::TypeDictAQ);
    }
    else {
        die q{new(): Bad :$heading arg; it is not an object of a}
                ~ q{ QDRDBMS::AST::TypeDictNQ-doing class.}
            if !$heading.defined
                or !$heading.does(::QDRDBMS::AST::TypeDictNQ);
    }
    my Hash $heading_attrs_map_hoa = $heading!map_hoa;

    die q{new(): Bad :$body arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$body.defined or !$body.does(Array);
    for $body -> $tupb {
        die q{new(): Bad :$body arg elem; it is not an object of a}
                ~ q{ QDRDBMS::AST::ExprDict-doing class.}
            if !$tupb.defined or !$tupb.does(::QDRDBMS::AST::ExprDict);
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
    return $FALSE
        if !$self!heading.equal_repr( :other($other!heading) );
    my Array $v1 = $self!body;
    my Array $v2 = $other!body;
    return $FALSE
        if $v2.elems !=== $v1.elems;
    for 0..^$v1.elems -> $i {
        return $FALSE
            if !$v1.[$i].equal_repr( :other($v2.[$i]) );
    }
    return $TRUE;
}

###########################################################################

method heading of QDRDBMS::AST::TypeDict () {
    return $!heading;
}

###########################################################################

method body of Array () {
    return [$!body.values];
}

###########################################################################

} # role QDRDBMS::AST::_Relation

###########################################################################
###########################################################################

class QDRDBMS::AST::RelationSel {
    does QDRDBMS::AST::_Relation;
    submethod BUILD {} # otherwise Pugs r16488 invo _Relation.BUILD twice
    method _allows_quasi of Bool () { return $FALSE; }
} # class QDRDBMS::AST::RelationSel

###########################################################################
###########################################################################

class QDRDBMS::AST::QuasiRelationSel {
    does QDRDBMS::AST::_Relation;
    submethod BUILD {} # otherwise Pugs r16488 invo _Relation.BUILD twice
    method _allows_quasi of Bool () { return $TRUE; }
} # class QDRDBMS::AST::QuasiRelationSel

###########################################################################
###########################################################################

class QDRDBMS::AST::VarInvo {
    does QDRDBMS::AST::Expr;

    has QDRDBMS::AST::EntityName $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (QDRDBMS::AST::EntityName :$v!) {

    die q{new(): Bad :$v arg; it is not a valid object}
            ~ q{ of a QDRDBMS::AST::EntityName-doing class.}
        if !$v.defined or !$v.does(::QDRDBMS::AST::EntityName);

    $!v = $v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = $!v.as_perl();
        $!as_perl = "QDRDBMS::AST::VarInvo.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $self!v.equal_repr( :other($other!v) );
}

###########################################################################

method v of QDRDBMS::AST::EntityName () {
    return $!v;
}

###########################################################################

} # class QDRDBMS::AST::VarInvo

###########################################################################
###########################################################################

class QDRDBMS::AST::FuncInvo {
    does QDRDBMS::AST::Expr;

    has QDRDBMS::AST::EntityName $!func;
    has QDRDBMS::AST::ExprDict   $!ro_args;

    has Str $!as_perl;

###########################################################################

submethod BUILD (QDRDBMS::AST::EntityName :$func!,
        QDRDBMS::AST::ExprDict :$ro_args!) {

    die q{new(): Bad :$func arg; it is not a valid object}
            ~ q{ of a QDRDBMS::AST::EntityName-doing class.}
        if !$func.defined or !$func.does(::QDRDBMS::AST::EntityName);

    die q{new(): Bad :$ro_args arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::ExprDict-doing class.}
        if !$ro_args.defined or !$ro_args.does(::QDRDBMS::AST::ExprDict);

    $!func    = $func;
    $!ro_args = $ro_args;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $sf = $!func.as_perl();
        my Str $sra = $!ro_args.as_perl();
        $!as_perl = "QDRDBMS::AST::FuncInvo.new("
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

method func of QDRDBMS::AST::EntityName () {
    return $!func;
}

method ro_args of QDRDBMS::AST::ExprDict () {
    return $!ro_args;
}

###########################################################################

} # class QDRDBMS::AST::FuncInvo

###########################################################################
###########################################################################

role QDRDBMS::AST::Stmt {
    does QDRDBMS::AST::Node;
} # role QDRDBMS::AST::Stmt

###########################################################################
###########################################################################

class QDRDBMS::AST::ProcInvo {
    does QDRDBMS::AST::Stmt;

    has QDRDBMS::AST::EntityName $!proc;
    has QDRDBMS::AST::ExprDict   $!upd_args;
    has QDRDBMS::AST::ExprDict   $!ro_args;

    has Str $!as_perl;

###########################################################################

submethod BUILD (QDRDBMS::AST::EntityName :$proc!,
        QDRDBMS::AST::ExprDict :$upd_args!,
        QDRDBMS::AST::ExprDict :$ro_args!) {

    die q{new(): Bad :$proc arg; it is not a valid object}
            ~ q{ of a QDRDBMS::AST::EntityName-doing class.}
        if !$proc.defined or !$proc.does(::QDRDBMS::AST::EntityName);

    die q{new(): Bad :$upd_args arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::ExprDict-doing class.}
        if !$upd_args.defined or !$upd_args.does(::QDRDBMS::AST::ExprDict);
    die q{new(): Bad :$ro_args arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::ExprDict-doing class.}
        if !$ro_args.defined or !$ro_args.does(::QDRDBMS::AST::ExprDict);
    my Hash $upd_args_map_hoa = $upd_args!map_hoa;
    for $upd_args_map_hoa.values -> $an_and_vn {
        die q{new(): Bad :$upd_args arg elem expr; it is not}
                ~ q{ an object of a QDRDBMS::AST::VarInvo-doing class.}
            if !$an_and_vn.[1].does(::QDRDBMS::AST::VarInvo);
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
        $!as_perl = "QDRDBMS::AST::ProcInvo.new("
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

method proc of QDRDBMS::AST::EntityName () {
    return $!proc;
}

method upd_args of QDRDBMS::AST::ExprDict () {
    return $!upd_args;
}

method ro_args of QDRDBMS::AST::ExprDict () {
    return $!ro_args;
}

###########################################################################

} # class QDRDBMS::AST::ProcInvo

###########################################################################
###########################################################################

class QDRDBMS::AST::FuncReturn {
    does QDRDBMS::AST::Stmt;

    has QDRDBMS::AST::Expr $!v;

    has Str $!as_perl;

###########################################################################

submethod BUILD (QDRDBMS::AST::Expr :$v!) {

    die q{new(): Bad :$v arg; it is not a valid object}
            ~ q{ of a QDRDBMS::AST::Expr-doing class.}
        if !$v.defined or !$v.does(::QDRDBMS::AST::Expr);

    $!v = $v;

    return;
}

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = $!v.as_perl();
        $!as_perl = "QDRDBMS::AST::FuncReturn.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $self!v.equal_repr( :other($other!v) );
}

###########################################################################

method v of QDRDBMS::AST::Expr () {
    return $!v;
}

###########################################################################

} # class QDRDBMS::AST::FuncReturn

###########################################################################
###########################################################################

class QDRDBMS::AST::ProcReturn {
    does QDRDBMS::AST::Stmt;

###########################################################################

method as_perl of Str () {
    return 'QDRDBMS::AST::ProcReturn.new()';
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $TRUE;
}

###########################################################################

} # class QDRDBMS::AST::ProcReturn

###########################################################################
###########################################################################

class QDRDBMS::AST::EntityName {
    does QDRDBMS::AST::Node;

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

    $!text_possrep = $text;
    $!seq_possrep = [$text.split( rx/\./ ).map:{ #/
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
            .trans( < \\  .   >
                 => < \\b \\p > )
        }.join( q{.} );
    $!seq_possrep = [$seq.values];

    return;
}
=cut

###########################################################################

method as_perl of Str () {
    if (!$!as_perl.defined) {
        my Str $s = q{'} ~ $!text_possrep.trans( q{'} => q{\\'} ) ~ q{'};
        $!as_perl = "QDRDBMS::AST::EntityName.new( :text($s) )";
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

###########################################################################

method seq of Array () {
    return [$!seq_possrep.values];
}

###########################################################################

} # class QDRDBMS::AST::EntityName

###########################################################################
###########################################################################

role QDRDBMS::AST::TypeInvo {
    does QDRDBMS::AST::Node;

    has Str $!kind;
    has Any $!spec;

    has Str $!as_perl;

###########################################################################

submethod BUILD (Str :$kind!, Any :$spec!) {

    die q{new(): Bad :$kind arg; it is not an object of a Str-doing class.}
        if !$kind.defined or !$kind.does(Str);

    if $kind === 'Scalar' {
        die q{new(): Bad :$spec arg; it needs to be a valid object}
                ~ q{ of a QDRDBMS::AST::EntityName-doing class}
                ~ q{ when the :$kind arg is 'Scalar'.}
            if !$spec.defined or !$spec.does(::QDRDBMS::AST::EntityName);
    }

    elsif $kind === 'Tuple'|'Relation' {
        die q{new(): Bad :$spec arg; it needs to be a valid object}
                ~ q{ of a QDRDBMS::AST::TypeDictNQ-doing class}
                ~ q{ when the :$kind arg is 'Tuple'|'Relation'.}
            if !$spec.defined or !$spec.does(::QDRDBMS::AST::TypeDictNQ);
    }

    elsif (!self._allows_quasi()) {
        die q{new(): Bad :$kind arg; it needs to be one of}
            ~ q{ 'Scalar'|'Tuple'|'Relation'.};
    }

    elsif $kind === 'QTuple'|'QRelation' {
        die q{new(): Bad :$spec arg; it needs to be a valid object}
                ~ q{ of a QDRDBMS::AST::TypeDictAQ-doing class}
                ~ q{ when the :$kind arg is 'QTuple'|'QRelation'.}
            if !$spec.defined or !$spec.does(::QDRDBMS::AST::TypeDictAQ);
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
    return $FALSE
        if $other!kind !=== $self!kind;
    return $self!kind === 'Any' ?? $other!spec === $self!spec
        !! $self!spec.equal_repr( :other($other!spec) );
}

###########################################################################

method kind of Str () {
    return $!kind;
}

###########################################################################

method spec of Any () {
    return $!spec;
}

###########################################################################

} # role QDRDBMS::AST::TypeInvo

###########################################################################
###########################################################################

class QDRDBMS::AST::TypeInvoNQ {
    does QDRDBMS::AST::TypeInvo;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeInvo.BUILD twice
    method _allows_quasi of Bool () { return $FALSE; }
} # class QDRDBMS::AST::TypeInvoNQ

###########################################################################
###########################################################################

class QDRDBMS::AST::TypeInvoAQ {
    does QDRDBMS::AST::TypeInvo;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeInvo.BUILD twice
    method _allows_quasi of Bool () { return $TRUE; }
} # class QDRDBMS::AST::TypeInvoAQ

###########################################################################
###########################################################################

role QDRDBMS::AST::TypeDict {
    does QDRDBMS::AST::Node;

    has Array $!map_aoa;
    has Hash  $!map_hoa;

    has Str $!as_perl;

    trusts QDRDBMS::AST::_Tuple;
    trusts QDRDBMS::AST::_Relation;
    trusts QDRDBMS::AST::HostGateRtn;
    trusts QDRDBMS::Interface::HostGateRtn;

###########################################################################

submethod BUILD (Array :$map!) {

    my Bool $allows_quasi = self._allows_quasi();

    die q{new(): Bad :$map arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$map.defined or !$map.does(Array);
    my Array $map_aoa = [];
    my Hash  $map_hoa = {};
    for $map -> $elem {
        die q{new(): Bad :$map arg; it is not an object of a}
                ~ q{ Array-doing class, or it doesn't have 2 elements.}
            if !$elem.defined or !$elem.does(Array) or $elem.elems != 2;
        my ($entity_name, $type_invo) = $elem.values;
        die q{new(): Bad :$map arg elem; its first elem is not}
                ~ q{ an object of a QDRDBMS::AST::EntityName-doing class.}
            if !$entity_name.defined
                or !$entity_name.does(::QDRDBMS::AST::EntityName);
        my Str $entity_name_text = $entity_name.text();
        die q{new(): Bad :$map arg elem; its first elem is not}
                ~ q{ distinct between the arg elems.}
            if $map_hoa.exists($entity_name_text);
        if $allows_quasi {
            die q{new(): Bad :$map arg elem; its second elem is not an}
                    ~ q{ object of a QDRDBMS::AST::TypeInvoAQ-doing class.}
                if !$type_invo.defined
                    or !$type_invo.does(::QDRDBMS::AST::TypeInvoAQ);
        }
        else {
            die q{new(): Bad :$map arg elem; its second elem is not an}
                    ~ q{ object of a QDRDBMS::AST::TypeInvoNQ-doing class.}
                if !$type_invo.defined
                    or !$type_invo.does(::QDRDBMS::AST::TypeInvoNQ);
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
        $!as_perl = "QDRDBMS::AST::TypeDict.new( :map($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $FALSE
        if $other!map_aoa.elems !=== $self!map_aoa.elems;
    my Hash $v1 = $self!map_hoa;
    my Hash $v2 = $other!map_hoa;
    for $v1.pairs -> $e {
        return $FALSE
            if !$v2.exists($e.key);
        return $FALSE
            if !$e.value.[1].equal_repr( :other($v2.{$e.key}.[1]) );
    }
    return $TRUE;
}

###########################################################################

method map of Array () {
    return [$!map_aoa.map:{ [.values] }];
}

method map_hoa of Hash () {
    return {$!map_hoa.pairs.map:{ .key => [.value.values] }};
}

###########################################################################

} # role QDRDBMS::AST::TypeDict

###########################################################################
###########################################################################

class QDRDBMS::AST::TypeDictNQ {
    does QDRDBMS::AST::TypeDict;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeDict.BUILD twice
    method _allows_quasi of Bool () { return $FALSE; }
} # class QDRDBMS::AST::TypeDictNQ

###########################################################################
###########################################################################

class QDRDBMS::AST::TypeDictAQ {
    does QDRDBMS::AST::TypeDict;
    submethod BUILD {} # otherwise Pugs r16488 invo TypeDict.BUILD twice
    method _allows_quasi of Bool () { return $TRUE; }
} # class QDRDBMS::AST::TypeDictAQ

###########################################################################
###########################################################################

class QDRDBMS::AST::ExprDict {
    does QDRDBMS::AST::Node;

    has Array $!map_aoa;
    has Hash  $!map_hoa;

    # Note: This type is specific such that values are always some ::Expr,
    # but this type may be later generalized to hold ::Node instead.

    has Str $!as_perl;

    trusts QDRDBMS::AST::_Tuple;
    trusts QDRDBMS::AST::_Relation;
    trusts QDRDBMS::AST::ProcInvo;

###########################################################################

submethod BUILD (Array :$map!) {

    die q{new(): Bad :$map arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$map.defined or !$map.does(Array);
    my Array $map_aoa = [];
    my Hash  $map_hoa = {};
    for $map -> $elem {
        die q{new(): Bad :$map arg; it is not an object of a}
                ~ q{ Array-doing class, or it doesn't have 2 elements.}
            if !$elem.defined or !$elem.does(Array) or $elem.elems != 2;
        my ($entity_name, $expr) = $elem.values;
        die q{new(): Bad :$map arg elem; its first elem is not}
                ~ q{ an object of a QDRDBMS::AST::EntityName-doing class.}
            if !$entity_name.defined
                or !$entity_name.does(::QDRDBMS::AST::EntityName);
        my Str $entity_name_text = $entity_name.text();
        die q{new(): Bad :$map arg elem; its first elem is not}
                ~ q{ distinct between the arg elems.}
            if $map_hoa.exists($entity_name_text);
        die q{new(): Bad :$map arg elem; its second elem is not}
                ~ q{ an object of a QDRDBMS::AST::Expr-doing class.}
            if !$expr.defined or !$expr.does(::QDRDBMS::AST::Expr);
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
        $!as_perl = "QDRDBMS::AST::ExprDict.new( :map($s) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $FALSE
        if $other!map_aoa.elems !=== $self!map_aoa.elems;
    my Hash $v1 = $self!map_hoa;
    my Hash $v2 = $other!map_hoa;
    for $v1.pairs -> $e {
        return $FALSE
            if !$v2.exists($e.key);
        return $FALSE
            if !$e.value.[1].equal_repr( :other($v2.{$e.key}.[1]) );
    }
    return $TRUE;
}

###########################################################################

method map of Array () {
    return [$!map_aoa.map:{ [.values] }];
}

method map_hoa of Hash () {
    return {$!map_hoa.pairs.map:{ .key => [.value.values] }};
}

###########################################################################

} # class QDRDBMS::AST::ExprDict

###########################################################################
###########################################################################

class QDRDBMS::AST::FuncDecl {
    does QDRDBMS::AST::Node;

###########################################################################

submethod BUILD {
    die q{not implemented};
}

###########################################################################

} # class QDRDBMS::AST::FuncDecl

###########################################################################
###########################################################################

class QDRDBMS::AST::ProcDecl {
    does QDRDBMS::AST::Node;

###########################################################################

submethod BUILD {
    die q{not implemented};
}

###########################################################################

} # class QDRDBMS::AST::ProcDecl

###########################################################################
###########################################################################

class QDRDBMS::AST::HostGateRtn {
    does QDRDBMS::AST::Node;

    has QDRDBMS::AST::TypeDict $!upd_params;
    has QDRDBMS::AST::TypeDict $!ro_params;
    has QDRDBMS::AST::TypeDict $!vars;
    has Array                  $!stmts;

    has Str $!as_perl;

    trusts QDRDBMS::Interface::HostGateRtn;

###########################################################################

submethod BUILD (QDRDBMS::AST::TypeDict :$upd_params!,
        QDRDBMS::AST::TypeDict :$ro_params!,
        QDRDBMS::AST::TypeDict :$vars!, Array :$stmts!) {

    die q{new(): Bad :$upd_params arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::TypeDict-doing class.}
        if !$upd_params.defined
            or !$upd_params.does(::QDRDBMS::AST::TypeDict);
    die q{new(): Bad :$ro_params arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::TypeDict-doing class.}
        if !$ro_params.defined
            or !$ro_params.does(::QDRDBMS::AST::TypeDict);
    die q{new(): Bad :$upd_params or :$ro_params arg;}
            ~ q{ they both reference at least 1 same procedure param.}
        if any($ro_params!map_hoa.keys) === any($upd_params!map_hoa.keys);

    die q{new(): Bad :$vars arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::TypeDict-doing class.}
        if !$vars.defined or !$vars.does(::QDRDBMS::AST::TypeDict);

    die q{new(): Bad :$stmts arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$stmts.defined or !$stmts.does(Array);
    for $stmts -> $stmt {
        die q{new(): Bad :$stmts arg elem; it is not}
                ~ q{ an object of a QDRDBMS::AST::Stmt-doing class.}
            if !$stmt.defined or !$stmt.does(::QDRDBMS::AST::Stmt);
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
        $!as_perl = "QDRDBMS::AST::HostGateRtn.new( :upd_params($sup)"
            ~ ", :ro_params($srp), :vars($sv), :stmts($ss) )";
    }
    return $!as_perl;
}

###########################################################################

method _equal_repr of Bool (::T $self: T $other!) {
    return $FALSE
        if !$self!upd_params.equal_repr( :other($other!upd_params) )
            or !$self!ro_params.equal_repr( :other($other!ro_params) )
            or !$self!vars.equal_repr( :other($other!vars) );
    my Array $v1 = $self!stmts;
    my Array $v2 = $other!stmts;
    return $FALSE
        if $v2.elems !=== $v1.elems;
    for 0..^$v1.elems -> $i {
        return $FALSE
            if !$v1.[$i].equal_repr( :other($v2.[$i]) );
    }
    return $TRUE;
}

###########################################################################

method upd_params of QDRDBMS::AST::TypeDict () {
    return $!upd_params;
}

method ro_params of QDRDBMS::AST::TypeDict () {
    return $!ro_params;
}

method vars of QDRDBMS::AST::TypeDict () {
    return $!vars;
}

method stmts of QDRDBMS::AST::EntityName () {
    return [$!stmts.values];
}

###########################################################################

} # class QDRDBMS::AST::HostGateRtn

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

QDRDBMS::AST -
Abstract syntax tree for the QDRDBMS D language

=head1 VERSION

This document describes QDRDBMS::AST version 0.0.0 for Perl 6.

It also describes the same-number versions for Perl 6 of [...].

=head1 SYNOPSIS

I<This documentation is pending.>

    use QDRDBMS::AST <newBoolLit newTextLit newBlobLit newIntLit
        newTupleSel newQuasiTupleSel newRelationSel newQuasiRelationSel
        newVarInvo newFuncInvo newProcInvo newFuncReturn newProcReturn
        newEntityName newTypeInvoNQ newTypeInvoAQ newTypeDictNQ
        newTypeDictAQ newExprDict newFuncDecl newProcDecl newHostGateRtn>;

    my $truth_value = newBoolLit( :v(2 + 2 == 4) );
    my $planetoid = newTextLit( :v('Ceres') );
    my $package = newBlobLit( :v(pack 'H2', 'P') );
    my $answer = newIntLit( :v(42) );

I<This documentation is pending.>

=head1 DESCRIPTION

The native command language of a L<QDRDBMS> DBMS (database management
system) / virtual machine is called B<QDRDBMS D>; see L<QDRDBMS::Language>
for the language's human readable authoritative design document.

QDRDBMS D has 3 closely corresponding main representation formats, which
are catalog relations (what routines inside the DBMS see), hierarchical AST
(abstract syntax tree) nodes (what the application driving the DBMS
typically sees), and string-form QDRDBMS D code that users interacting with
QDRDBMS via a shell interface would use.  The string-form would be parsed
into the AST, and the AST be flattened into the relations; similarly, the
relations can be unflattened into the AST, and string-form code be
generated from the AST if desired.

This library, QDRDBMS::AST ("AST"), provides a few dozen container classes
which collectively implement the AST representation format of QDRDBMS D;
each class is called an I<AST node type> or I<node type>, and an object of
one of these classes is called an I<AST node> or I<node>.

These are all of the roles and classes that QDRDBMS::AST defines (more will
be added in the future), which are visually arranged here in their "does"
or "isa" hierarchy, children indented under parents:

    QDRDBMS::AST::Node (dummy role)
        QDRDBMS::AST::Expr (dummy role)
            QDRDBMS::AST::Lit (dummy role)
                QDRDBMS::AST::BoolLit
                QDRDBMS::AST::TextLit
                QDRDBMS::AST::BlobLit
                QDRDBMS::AST::IntLit
            QDRDBMS::AST::_Tuple (implementing role)
                QDRDBMS::AST::TupleSel
                QDRDBMS::AST::QuasiTupleSel
            QDRDBMS::AST::_Relation (implementing role)
                QDRDBMS::AST::RelationSel
                QDRDBMS::AST::QuasiRelationSel
            QDRDBMS::AST::VarInvo
            QDRDBMS::AST::FuncInvo
        QDRDBMS::AST::Stmt (dummy role)
            QDRDBMS::AST::ProcInvo
            QDRDBMS::AST::FuncReturn
            QDRDBMS::AST::ProcReturn
            # more control-flow statement types would go here
        QDRDBMS::AST::EntityName
        QDRDBMS::AST::TypeInvo (implementing role)
            QDRDBMS::AST::TypeInvoNQ
            QDRDBMS::AST::TypeInvoAQ
        QDRDBMS::AST::TypeDict (implementing role)
            QDRDBMS::AST::TypeDictNQ
            QDRDBMS::AST::TypeDictAQ
        QDRDBMS::AST::ExprDict
        QDRDBMS::AST::FuncDecl
        QDRDBMS::AST::ProcDecl
        # more routine declaration types would go here
        QDRDBMS::AST::HostGateRtn

All QDRDBMS D abstract syntax trees are such in the compositional sense;
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

The QDRDBMS framework uses QDRDBMS AST nodes for the dual purpose of
defining routines to execute and defining values to use as arguments to and
return values from the execution of said routines.  The C<prepare()> method
of a C<QDRDBMS::Interface::DBMS> object, and by extension the
C<QDRDBMS::Interface::HostGateRtn->new()> constructor function, takes a
C<QDRDBMS::AST::HostGateRtn> node as its primary argument, such that the
AST object defines the source code that is compiled to become the Interface
object.  The C<fetch_ast()> and C<store_ast()> methods of a
C<QDRDBMS::Interface::HostGateVar> object will get or set that object's
primary value attribute, which is any C<QDRDBMS::AST::Node>.  The C<Var>
objects are bound to C<Rtn> objects, and they are the means by which an
executed routine accepts input or provides output at C<execute()> time.

=head2 AST Node Values Versus Representations

In the general case, QDRDBMS AST nodes do not maintain canonical
representations of all QDRDBMS D values, meaning that it is possible and
common to have 2 given AST nodes that logically denote the same value, but
they have different actual compositions.  (Some node types are special
cases for which the aforementioned isn't true; see below.)

For example, a node whose value is just the number 5 can have any number of
representations, each of which is an expression that evaluates to the
number 5 (such as [C<5>, C<2+3>, C<10/2>]).  Another example is a node
whose value is the set C<{3,5,7}>; it can be represented, for example,
either by C<Set(5,3,7,7,7)> or C<Union(Set(3,5),Set(5,7))> or
C<Set(7,5,3)>.  I<These examples aren't actual QDRDBMS AST syntax.>

For various reasons, the QDRDBMS::AST classes themselves do not do any node
refactoring, and their representations differ little if any from the format
of their constructor arguments, which can contain extra information that is
not logically significant in determining the node value.  One reason is
that this allows a semblance of maintaining the actual syntax that the user
specified, which is useful for their debugging purposes.  Another reason is
the desire to keep this library as light-weight as possible, such that it
just implements the essentials; doing refactoring can require a code size
and complexity that is orders of magnitude larger than these essentials,
and that work isn't always helpful.  It should also be noted that any nodes
having references to externally user-defined entities can't be fully
refactored as each of those represents a free variable that a static node
analysis can't decompose; only nodes consisting of just system-defined or
literal entities (meaning zero free variables) can be fully refactored in a
static node analysis (though there are a fair number of those in practice,
particularly as C<Var> values).

A consequence of this is that the QDRDBMS::AST classes in general do not
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
available over time whose purpose is to refactor a QDRDBMS AST node, either
as part of a static analysis that considers only the node in isolation (and
any user-defined entity references have to be treated as free variables and
not generally be factored out), or as part of an Engine implementation that
also considers the current virtual machine environment and what
user-defined entities exist there (and depending on the context,
user-defined entity references don't have to be free variables).

=head1 INTERFACE

The interface of QDRDBMS::AST is fundamentally object-oriented; you use it
by creating objects from its member classes, usually invoking C<new()> on
the appropriate class name, and then invoking methods on those objects.
All of their attributes are private, so you must use accessor methods.

QDRDBMS::AST also provides wrapper subroutines for all member class
constructors, 1 per each, where each subroutine has identical parameters to
the constructor it wraps, and the name of each subroutine is equal to the
trailing part of the class name, specifically the C<Foo> of
C<QDRDBMS::AST::Foo>, but with a C<new> prefix (so that Perl doesn't
confuse a fully-qualified sub name with a class name).  All of these
subroutines are exportable, but are not exported by default, and exist
solely as syntactic sugar to allow user code to have more brevity.  I<TODO:
 Reimplement these as lexical aliases or compile-time macros instead, to
avoid the overhead of extra routine calls.>

The usual way that QDRDBMS::AST indicates a failure is to throw an
exception; most often this is due to invalid input.  If an invoked routine
simply returns, you can assume that it has succeeded, even if the return
value is undefined.

=head2 The QDRDBMS::AST::BoolLit Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::TextLit Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::BlobLit Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::IntLit Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::TupleSel Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::QuasiTupleSel Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::RelationSel Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::QuasiRelationSel Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::VarInvo Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::FuncInvo Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::ProcInvo Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::FuncReturn Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::ProcReturn Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::EntityName Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::TypeInvoNQ Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::TypeInvoAQ Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::TypeDictNQ Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::TypeDictAQ Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::ExprDict Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::FuncDecl Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::ProcDecl Class

I<This documentation is pending.>

=head2 The QDRDBMS::AST::HostGateRtn Class

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

Go to L<QDRDBMS> for the majority of distribution-internal references, and
L<QDRDBMS::SeeAlso> for the majority of distribution-external references.

=head1 BUGS AND LIMITATIONS

For design simplicity in the short term, all AST arguments that are
applicable must be explicitly defined by the user, even if it might be
reasonable for QDRDBMS to figure out a default value for them, such as
"same as self".  This limitation will probably be removed in the future.
All that said, a few arguments may be exempted from this limitation.

I<This documentation is pending.>

=head1 AUTHOR

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENSE AND COPYRIGHT

This file is part of the QDRDBMS framework.

QDRDBMS is Copyright © 2002-2007, Darren Duncan.

See the LICENSE AND COPYRIGHT of L<QDRDBMS> for details.

=head1 ACKNOWLEDGEMENTS

The ACKNOWLEDGEMENTS in L<QDRDBMS> apply to this file too.

=cut
