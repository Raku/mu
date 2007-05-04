use v6-alpha;

###########################################################################
###########################################################################

module QDRDBMS::AST-0.0.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub LitBool of QDRDBMS::AST::LitBool (Bool :$v!) is export {
    return QDRDBMS::AST::LitBool.new( :v($v) );
}

sub LitText of QDRDBMS::AST::LitText (Str :$v!) is export {
    return QDRDBMS::AST::LitText.new( :v($v) );
}

sub LitBlob of QDRDBMS::AST::LitBlob (Blob :$v!) is export {
    return QDRDBMS::AST::LitBlob.new( :v($v) );
}

sub LitInt of QDRDBMS::AST::LitInt (Int :$v!) is export {
    return QDRDBMS::AST::LitInt.new( :v($v) );
}

sub SetSel of QDRDBMS::AST::SetSel (Array :$v!) is export {
    return QDRDBMS::AST::SetSel.new( :v($v) );
}

sub SeqSel of QDRDBMS::AST::SeqSel (Array :$v!) is export {
    return QDRDBMS::AST::SeqSel.new( :v($v) );
}

sub BagSel of QDRDBMS::AST::BagSel (Array :$v!) is export {
    return QDRDBMS::AST::BagSel.new( :v($v) );
}

multi sub EntityName of QDRDBMS::AST::EntityName
        (QDRDBMS::AST::LitText :$text!) is export {
    return QDRDBMS::AST::EntityName.new( :text($text) );
}

multi sub EntityName of QDRDBMS::AST::EntityName
        (QDRDBMS::AST::SeqSel :$seq!) is export {
    return QDRDBMS::AST::EntityName.new( :seq($seq) );
}

sub ExprDict of QDRDBMS::AST::ExprDict (Array :$map!) is export {
    return QDRDBMS::AST::ExprDict.new( :map($map) );
}

sub TypeDict of QDRDBMS::AST::TypeDict (Array :$map!) is export {
    return QDRDBMS::AST::TypeDict.new( :map($map) );
}

sub VarInvo of QDRDBMS::AST::VarInvo
        (QDRDBMS::AST::EntityName :$v!) is export {
    return QDRDBMS::AST::VarInvo.new( :v($v) );
}

sub FuncInvo of QDRDBMS::AST::FuncInvo (QDRDBMS::AST::EntityName :$func!,
        QDRDBMS::AST::ExprDict :$ro_args!) is export {
    return QDRDBMS::AST::FuncInvo.new(
        :func($func), :ro_args($ro_args) );
}

sub ProcInvo of QDRDBMS::AST::ProcInvo (QDRDBMS::AST::EntityName :$proc!,
        QDRDBMS::AST::ExprDict :$upd_args!,
        QDRDBMS::AST::ExprDict :$ro_args!) is export {
    return QDRDBMS::AST::ProcInvo.new(
        :proc($proc), :upd_args($upd_args), :ro_args($ro_args) );
}

sub FuncReturn of QDRDBMS::AST::FuncReturn
        (QDRDBMS::AST::Expr :$v!) is export {
    return QDRDBMS::AST::FuncReturn.new( :v($v) );
}

sub ProcReturn of QDRDBMS::AST::ProcReturn () is export {
    return QDRDBMS::AST::ProcReturn.new();
}

sub FuncDecl of QDRDBMS::AST::FuncDecl () is export {
    return QDRDBMS::AST::FuncDecl.new();
}

sub ProcDecl of QDRDBMS::AST::ProcDecl () is export {
    return QDRDBMS::AST::ProcDecl.new();
}

sub HostGateRtn of QDRDBMS::AST::HostGateRtn
        (QDRDBMS::AST::TypeDict :$upd_params!,
        QDRDBMS::AST::TypeDict :$ro_params!,
        QDRDBMS::AST::TypeDict :$vars!, Array :$stmts!) is export {
    return QDRDBMS::AST::HostGateRtn.new( :upd_params($upd_params),
        :ro_params($ro_params), :vars($vars), :stmts($stmts) );
}

###########################################################################

} # module QDRDBMS::AST

###########################################################################
###########################################################################

role QDRDBMS::AST::Node {

    method as_perl of Str () {
        die q{not implemented by subclass};
    }

    method equal_repr of Str () {
        die q{not implemented by subclass};
    }

} # role QDRDBMS::AST::Node

###########################################################################
###########################################################################

role QDRDBMS::AST::Expr {
    does QDRDBMS::AST::Node;
} # role QDRDBMS::AST::Expr

###########################################################################
###########################################################################

class QDRDBMS::AST::LitBool {
    does QDRDBMS::AST::Expr;

    my $FALSE = Bool::False;
    my $TRUE  = Bool::True;

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
        $!as_perl = "QDRDBMS::AST::LitBool.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

sub v of Bool () {
    return $!v;
}

###########################################################################

} # class QDRDBMS::AST::LitBool

###########################################################################
###########################################################################

class QDRDBMS::AST::LitText {
    does QDRDBMS::AST::Expr;

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
        $!as_perl = "QDRDBMS::AST::LitText.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

sub v of Str () {
    return $!v;
}

###########################################################################

} # class QDRDBMS::AST::LitText

###########################################################################
###########################################################################

class QDRDBMS::AST::LitBlob {
    does QDRDBMS::AST::Expr;

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
            split q{}, ] ~ $hex_digit_text ~ q[)];
        $!as_perl = "QDRDBMS::AST::LitBlob.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

sub v of Blob () {
    return $!v;
}

###########################################################################

} # class QDRDBMS::AST::LitBlob

###########################################################################
###########################################################################

class QDRDBMS::AST::LitInt {
    does QDRDBMS::AST::Expr;

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
        $!as_perl = "QDRDBMS::AST::LitInt.new( :v($s) )";
    }
    return $!as_perl;
}

###########################################################################

sub v of Int () {
    return $!v;
}

###########################################################################

} # class QDRDBMS::AST::LitInt

###########################################################################
###########################################################################

role QDRDBMS::AST::ListSel {
    does QDRDBMS::AST::Expr;

    has Array $!v;

    has Str $!as_perl;

    trusts QDRDBMS::AST::EntityName;

###########################################################################

submethod BUILD (Array :$v!) {

    die q{new(): Bad :$v arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$v.defined or !$v.does(Array);
    for $v -> $ve {
        die q{new(): Bad :$v arg elem; it is not}
                ~ q{ an object of a QDRDBMS::AST::Expr-doing class.}
            if !$ve.defined or !$ve.does(QDRDBMS::AST::Expr);
    }

    $!v = [$v.values];

    return;
}

###########################################################################

sub v of Array () {
    return [$!v.values];
}

###########################################################################

} # role QDRDBMS::AST::ListSel

###########################################################################
###########################################################################

class QDRDBMS::AST::SetSel {
    does QDRDBMS::AST::ListSel;
} # class QDRDBMS::AST::SetSel

###########################################################################
###########################################################################

class QDRDBMS::AST::SeqSel {
    does QDRDBMS::AST::ListSel;
} # class QDRDBMS::AST::SeqSel

###########################################################################
###########################################################################

class QDRDBMS::AST::BagSel {
    does QDRDBMS::AST::ListSel;
} # class QDRDBMS::AST::BagSel

###########################################################################
###########################################################################

class QDRDBMS::AST::EntityName {
    does QDRDBMS::AST::Node;

    has QDRDBMS::AST::LitText $!text_possrep;
    has QDRDBMS::AST::SeqSel  $!seq_possrep;

    has Str $!as_perl;

###########################################################################

multi submethod BUILD (QDRDBMS::AST::LitText :$text!) {

    die q{new(): Bad :$text arg; it is not a valid object}
            ~ q{ of a QDRDBMS::AST::LitText-doing class.}
        if !$text.defined or !$text.does(QDRDBMS::AST::LitText);
    my Str $text_v = $text.v();
    die q{new(): Bad :$text arg; it contains character sequences that}
            ~ q{ are invalid within the Text possrep of an EntityName.}
        if $text_v.match( / \\ $/ ) or $text_v.match( / \\ <-[bp]> / );

    $!text_possrep = $text;
    $!seq_possrep = QDRDBMS::AST::SeqSel.new( :v(
            [$text_v.split( /\./ ).map:{
                    QDRDBMS::AST::LitText.new( :v(
                            .trans( < \\p \\b >
                                 => < .   \\  > )
                        ) );
                }]
        ) );

    return;
}

multi submethod BUILD (QDRDBMS::AST::SeqSel :$seq!) {

    die q{new(): Bad :$v arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::SeqSel-doing class.}
        if !$seq.defined or !$seq.does(QDRDBMS::AST::SeqSel);
    my $seq_elems = $seq!v;
    for $seq_elems -> $seq_e {
        die q{new(): Bad :$seq arg elem; it is not}
                ~ q{ an object of a QDRDBMS::AST::LitText-doing class.}
            if !$seq_e.does(QDRDBMS::AST::LitText);
    }

    $!text_possrep = QDRDBMS::AST::LitText.new( :v(
            $seq_elems.map:{
                    .v().trans( < \\  .   >
                             => < \\b \\p > )
                }.join( q{.} )
        ) );
    $!seq_possrep = $seq;

    return;
}

###########################################################################

method text of QDRDBMS::AST::LitText () {
    return $!text_possrep;
}

###########################################################################

method seq of QDRDBMS::AST::SeqSel () {
    return $!seq_possrep;
}

###########################################################################

} # class QDRDBMS::AST::EntityName

###########################################################################
###########################################################################

class QDRDBMS::AST::ExprDict {
    does QDRDBMS::AST::Node;

    has Array $!map_aoa;
    has Hash  $!map_hoa;

    # Note: This type is specific such that values are always some ::Expr,
    # but this type may be later generalized to hold ::Node instead.

    has Str $!as_perl;

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
                or !$entity_name.does(QDRDBMS::AST::EntityName);
        my Str $entity_name_text_v = $entity_name.text().v();
        die q{new(): Bad :$map arg elem; its first elem is not}
                ~ q{ distinct between the arg elems.}
            if $map_hoa.exists($entity_name_text_v);
        die q{new(): Bad :$map arg elem; its second elem is not}
                ~ q{ an object of a QDRDBMS::AST::Expr-doing class.}
            if !$expr.defined or !$expr.does(QDRDBMS::AST::Expr);
        my Array $elem_cpy = [$entity_name, $expr];
        $map_aoa.push( $elem_cpy );
        $map_hoa{$entity_name_text_v} = $elem_cpy;
    }

    $!map_aoa = $map_aoa;
    $!map_hoa = $map_hoa;

    return;
}

###########################################################################

sub map of Array () {
    return [$!map_aoa.map:{ [.values] }];
}

sub map_hoa of Hash () {
    return {$!map_hoa.pairs.map:{ .key => [.value.values] }};
}

###########################################################################

} # class QDRDBMS::AST::ExprDict

###########################################################################
###########################################################################

class QDRDBMS::AST::TypeDict {
    does QDRDBMS::AST::Node;

    has Array $!map_aoa;
    has Hash  $!map_hoa;

    # Note: This type may be generalized later to allow ::TypeDict values
    # and not just EntityName values; also, the latter will probably be
    # made more strict, to just be type names.

    has Str $!as_perl;

    trusts QDRDBMS::AST::HostGateRtn;
    trusts QDRDBMS::Interface::HostGateRtn;

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
        my ($entity_name, $type_name) = $elem.values;
        die q{new(): Bad :$map arg elem; its first elem is not}
                ~ q{ an object of a QDRDBMS::AST::EntityName-doing class.}
            if !$entity_name.defined
                or !$entity_name.does(QDRDBMS::AST::EntityName);
        my Str $entity_name_text_v = $entity_name.text().v();
        die q{new(): Bad :$map arg elem; its first elem is not}
                ~ q{ distinct between the arg elems.}
            if $map_hoa.exists($entity_name_text_v);
        die q{new(): Bad :$map arg elem; its second elem is not}
                ~ q{ an object of a QDRDBMS::AST::EntityName-doing class.}
            if !$type_name.defined
                or !$type_name.does(QDRDBMS::AST::EntityName);
        my Array $elem_cpy = [$entity_name, $type_name];
        $map_aoa.push( $elem_cpy );
        $map_hoa{$entity_name_text_v} = $elem_cpy;
    }

    $!map_aoa = $map_aoa;
    $!map_hoa = $map_hoa;

    return;
}

###########################################################################

sub map of Array () {
    return [$!map_aoa.map:{ [.values] }];
}

sub map_hoa of Hash () {
    return {$!map_hoa.pairs.map:{ .key => [.value.values] }};
}

###########################################################################

} # class QDRDBMS::AST::TypeDict

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
        if !$v.defined or !$v.does(QDRDBMS::AST::EntityName);

    $!v = $v;

    return;
}

###########################################################################

sub v of QDRDBMS::AST::EntityName () {
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
        if !$func.defined or !$func.does(QDRDBMS::AST::EntityName);

    die q{new(): Bad :$ro_args arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::ExprDict-doing class.}
        if !$ro_args.defined or !$ro_args.does(QDRDBMS::AST::ExprDict);

    $!func    = $func;
    $!ro_args = $ro_args;

    return;
}

###########################################################################

sub func of QDRDBMS::AST::EntityName () {
    return $!func;
}

sub ro_args of QDRDBMS::AST::ExprDict () {
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
        if !$proc.defined or !$proc.does(QDRDBMS::AST::EntityName);

    die q{new(): Bad :$upd_args arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::ExprDict-doing class.}
        if !$upd_args.defined or !$upd_args.does(QDRDBMS::AST::ExprDict);
    die q{new(): Bad :$ro_args arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::ExprDict-doing class.}
        if !$ro_args.defined or !$ro_args.does(QDRDBMS::AST::ExprDict);
    my Hash $upd_args_map_hoa = $upd_args!map_hoa;
    for $upd_args_map_hoa.values -> $an_and_vn {
        die q{new(): Bad :$upd_args arg elem expr; it is not}
                ~ q{ an object of a QDRDBMS::AST::VarInvo-doing class.}
            if !$an_and_vn.[1].does(QDRDBMS::AST::VarInvo);
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

sub proc of QDRDBMS::AST::EntityName () {
    return $!proc;
}

sub upd_args of QDRDBMS::AST::ExprDict () {
    return $!upd_args;
}

sub ro_args of QDRDBMS::AST::ExprDict () {
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
        if !$v.defined or !$v.does(QDRDBMS::AST::Expr);

    $!v = $v;

    return;
}

###########################################################################

sub v of QDRDBMS::AST::Expr () {
    return $!v;
}

###########################################################################

} # class QDRDBMS::AST::FuncReturn

###########################################################################
###########################################################################

class QDRDBMS::AST::ProcReturn {
    does QDRDBMS::AST::Stmt;
} # class QDRDBMS::AST::ProcReturn

###########################################################################
###########################################################################

class QDRDBMS::AST::FuncDecl {
    does QDRDBMS::AST::Node;

###########################################################################

submethod BUILD () {
    die q{not implemented};
}

###########################################################################

} # class QDRDBMS::AST::FuncDecl

###########################################################################
###########################################################################

class QDRDBMS::AST::ProcDecl {
    does QDRDBMS::AST::Node;

###########################################################################

submethod BUILD () {
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
            or !$upd_params.does(QDRDBMS::AST::TypeDict);
    die q{new(): Bad :$ro_params arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::TypeDict-doing class.}
        if !$ro_params.defined or !$ro_params.does(QDRDBMS::AST::TypeDict);
    die q{new(): Bad :$upd_params or :$ro_params arg;}
            ~ q{ they both reference at least 1 same procedure param.}
        if any($ro_params!map_hoa.keys) === any($upd_params!map_hoa.keys);

    die q{new(): Bad :$vars arg; it is not an object of a}
            ~ q{ QDRDBMS::AST::TypeDict-doing class.}
        if !$vars.defined or !$vars.does(QDRDBMS::AST::TypeDict);

    die q{new(): Bad :$stmts arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$stmts.defined or !$stmts.does(Array);
    for $stmts -> $stmt {
        die q{new(): Bad :$stmts arg elem; it is not}
                ~ q{ an object of a QDRDBMS::AST::Stmt-doing class.}
            if !$stmt.defined or !$stmt.does(QDRDBMS::AST::Stmt);
    }

    $!upd_params = $upd_params;
    $!ro_params  = $ro_params;
    $!vars       = $vars;
    $!stmts      = [$stmts.values];

    return;
}

###########################################################################

sub upd_params of QDRDBMS::AST::TypeDict () {
    return $!upd_params;
}

sub ro_params of QDRDBMS::AST::TypeDict () {
    return $!ro_params;
}

sub vars of QDRDBMS::AST::TypeDict () {
    return $!vars;
}

sub stmts of QDRDBMS::AST::EntityName () {
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

    use QDRDBMS::AST <LitBool LitText LitBlob LitInt SetSel SeqSel BagSel
        EntityName ExprDict TypeDict VarInvo FuncInvo ProcInvo FuncReturn
        ProcReturn FuncDecl ProcDecl HostGateRtn>;

    my $truth_value = LitBool( :v(2 + 2 == 4) );
    my $planetoid = LitText( :v('Ceres') );
    my $package = LitBlob( :v(pack 'H2', 'P') );
    my $answer = LitInt( :v(42) );

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
        QDRDBMS::AST::EntityName
        QDRDBMS::AST::ExprDict
        QDRDBMS::AST::TypeDict
        QDRDBMS::AST::Expr (dummy role)
            QDRDBMS::AST::LitBool
            QDRDBMS::AST::LitText
            QDRDBMS::AST::LitBlob
            QDRDBMS::AST::LitInt
            QDRDBMS::AST::ListSel (implementing role)
                QDRDBMS::AST::SetSel
                QDRDBMS::AST::SeqSel
                QDRDBMS::AST::BagSel
            QDRDBMS::AST::VarInvo
            QDRDBMS::AST::FuncInvo
        QDRDBMS::AST::Stmt (dummy role)
            QDRDBMS::AST::ProcInvo
            QDRDBMS::AST::FuncReturn
            QDRDBMS::AST::ProcReturn
            # more control-flow statement types would go here
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
executed routine obtains input or returns output at C<execute()> time.

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
that this allows a semblence of maintaining the actual syntax that the user
specified, which is useful for their debugging purposes.  Another reason is
the desire to keep this library as light-weight as possible, such that it
just implements the essentials; doing refactoring can require a code size
and complexity that is orders of magnitude larger than these essentials,
and that work isn't always helpful.  It should also be noted that any nodes
having references to externally user-defined entities can't be fully
refactored as each of those represents a free variable that a static node
analysis can't decompose; only nodes consisting of just system-defined or
literal entities (meaning zero free variables) can be fully refactored in a
static node analysys (though there are a fair number of those in practice,
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
[C<LitBool>, C<LitText>, C<LitBlob>, C<LitInt>, C<EntityName>, C<VarInvo>,
C<ProcReturn>] are guaranteed to only ever have a single representation per
value, and so C<equal_repr> is guaranteed to indicate value equality of 2
nodes of those types.  In fact, to assist the consequence this point, these
node classes also have the C<equal_value> method which is an alias for
C<equal_repr>, so you can use C<equal_value> in your use code to make it
better self documenting; C<equal_repr> is still available for all node
types to assist automated use code that wants to treat all node types the
same.  It should also be noted that a C<LitBool> node can only possibly be
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

The interface of QDRDBMS::AST is a combination of functions, objects, and
overloading.

The usual way that QDRDBMS::AST indicates a failure is to throw an
exception; most often this is due to invalid input.  If an invoked routine
simply returns, you can assume that it has succeeded, even if the return
value is undefined.

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

=head1 LICENCE AND COPYRIGHT

This file is part of the QDRDBMS framework.

QDRDBMS is Copyright © 2002-2007, Darren Duncan.

See the LICENCE AND COPYRIGHT of L<QDRDBMS> for details.

=head1 ACKNOWLEDGEMENTS

The ACKNOWLEDGEMENTS in L<QDRDBMS> apply to this file too.

=cut
