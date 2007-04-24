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

sub EntityName of QDRDBMS::AST::EntityName (QDRDBMS::AST::LitText :$text?,
        QDRDBMS::AST::SeqSel :$seq?) is export {
    return QDRDBMS::AST::EntityName.new( :text($text), :seq($seq) );
}

sub ExprDict of QDRDBMS::AST::ExprDict (Array :$map!) is export {
    return QDRDBMS::AST::ExprDict.new( :map($map) );
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

###########################################################################

} # module QDRDBMS::AST

###########################################################################
###########################################################################

role QDRDBMS::AST::Node {}

###########################################################################
###########################################################################

role QDRDBMS::AST::Expr {
    does QDRDBMS::AST::Node;
} # role QDRDBMS::AST::Expr

###########################################################################
###########################################################################

class QDRDBMS::AST::LitBool {
    does QDRDBMS::AST::Expr;

    has Bool $!v;

    submethod BUILD (Bool :$v!) {

        die q{new(): Bad :$v arg; it is not an object of a}
                ~ q{ Bool-doing class.}
            if !$v.defined or !$v.does(Bool);

        $!v = $v;

        return;
    }

    sub v of Bool () {
        return $!v;
    }

} # class QDRDBMS::AST::LitBool

###########################################################################
###########################################################################

class QDRDBMS::AST::LitText {
    does QDRDBMS::AST::Expr;

    has Str $!v;

    submethod BUILD (Str :$v!) {

        die q{new(): Bad :$v arg; it is not an object of a}
                ~ q{ Str-doing class.}
            if !$v.defined or !$v.does(Str);

        $!v = $v;

        return;
    }

    sub v of Str () {
        return $!v;
    }

} # class QDRDBMS::AST::LitText

###########################################################################
###########################################################################

class QDRDBMS::AST::LitBlob {
    does QDRDBMS::AST::Expr;

    has Blob $!v;

    submethod BUILD (Blob :$v!) {

        die q{new(): Bad :$v arg; it is not an object of a}
                ~ q{ Blob-doing class.}
            if !$v.defined or !$v.does(Blob);

        $!v = $v;

        return;
    }

    sub v of Blob () {
        return $!v;
    }

} # class QDRDBMS::AST::LitBlob

###########################################################################
###########################################################################

class QDRDBMS::AST::LitInt {
    does QDRDBMS::AST::Expr;

    has Int $!v;

    submethod BUILD (Int :$v!) {

        die q{new(): Bad :$v arg; it is not an object of a}
                ~ q{ Int-doing class.}
            if !$v.defined or !$v.does(Int);

        $!v = $v;

        return;
    }

    sub v of Int () {
        return $!v;
    }

} # class QDRDBMS::AST::LitInt

###########################################################################
###########################################################################

role QDRDBMS::AST::ListSel {
    does QDRDBMS::AST::Expr;

    has Array $!v;

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

###########################################################################

submethod BUILD
        (QDRDBMS::AST::LitText :$text?, QDRDBMS::AST::SeqSel :$seq?) {

    die q{new(): Exactly 1 of the args (:$text|:$seq) must be defined.}
#        if $text.defined !xor $seq.defined;
        if !($text.defined xor $seq.defined);

    if $text.defined {
        die q{new(): Bad :$text arg; it is not a valid object}
                ~ q{ of a QDRDBMS::AST::LitText-doing class.}
            if !$text.does(QDRDBMS::AST::LitText);
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
    }

    else { # $seq.defined
        die q{new(): Bad :$v arg; it is not an object of a}
                ~ q{ QDRDBMS::AST::SeqSel-doing class.}
            if !$seq.does(QDRDBMS::AST::SeqSel);
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
    }

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

class QDRDBMS::AST::VarInvo {
    does QDRDBMS::AST::Expr;

    has QDRDBMS::AST::EntityName $!v;

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



###########################################################################




###########################################################################

} # class QDRDBMS::AST::FuncDecl

###########################################################################
###########################################################################

class QDRDBMS::AST::ProcDecl {



###########################################################################




###########################################################################

} # class QDRDBMS::AST::ProcDecl

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

    use QDRDBMS::AST <LitBool LitText LitBlob LitInt>;

    my $truth_value = LitBool( :v(2 + 2 == 4) );
    my $planetoid = LitText( :v('Ceres') );
    my $package = LitBlob( :v(pack 'H2', 'P') );
    my $answer = LitInt( :v(42) );

I<This documentation is pending.>

=head1 DESCRIPTION

I<This documentation is pending.>

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
