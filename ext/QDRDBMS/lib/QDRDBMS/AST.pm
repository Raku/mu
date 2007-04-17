use v6-alpha;

###########################################################################
###########################################################################

#my Hash of QDRDBMS::AST::EntityName $LITERAL_TYPE_MAP = {
my Hash $LITERAL_TYPE_MAP = {
    'Bool' => ::QDRDBMS::AST::EntityName.new( :text('sys.type.Bool') ),
    'Str'  => ::QDRDBMS::AST::EntityName.new( :text('sys.type.Text') ),
    'Blob' => ::QDRDBMS::AST::EntityName.new( :text('sys.type.Blob') ),
    'Int'  => ::QDRDBMS::AST::EntityName.new( :text('sys.type.Int') ),
};

###########################################################################
###########################################################################

module QDRDBMS::AST-0.0.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub EntityName of QDRDBMS::AST::EntityName (Str :$text!) is export {
    return QDRDBMS::AST::EntityName.new( :text($text) );
}

sub LitDefExpr of QDRDBMS::AST::LitDefExpr
        (Bool|Str|Blob|Int :$lit!) is export {
    return QDRDBMS::AST::LitDefExpr.new( :lit($lit) );
}

sub VarNameExpr of QDRDBMS::AST::VarNameExpr
        (QDRDBMS::AST::EntityName :$var!) is export {
    return QDRDBMS::AST::VarNameExpr.new( :var($var) );
}

sub FuncInvoExpr of QDRDBMS::AST::FuncInvoExpr
        (QDRDBMS::AST::EntityName :$func!, Array :$func_args!) is export {
    return QDRDBMS::AST::FuncInvoExpr.new(
        :func($func), :func_args($func_args) );
}

sub ControlStmt of QDRDBMS::AST::ControlStmt () is export {
    return QDRDBMS::AST::ControlStmt.new();
}

sub ProcInvoStmt of QDRDBMS::AST::ProcInvoStmt
        (QDRDBMS::AST::EntityName :$proc!, Array :$proc_args!) is export {
    return QDRDBMS::AST::ProcInvoStmt.new(
        :proc($proc), :proc_args($proc_args) );
}

sub MultiProcInvoStmt of QDRDBMS::AST::MultiProcInvoStmt () is export {
    return QDRDBMS::AST::MultiProcInvoStmt.new();
}

sub Func of QDRDBMS::AST::Func () is export {
    return QDRDBMS::AST::Func.new();
}

sub Proc of QDRDBMS::AST::Proc () is export {
    return QDRDBMS::AST::Proc.new();
}

###########################################################################

} # module QDRDBMS::AST

###########################################################################
###########################################################################

class QDRDBMS::AST::EntityName {
    has Str $!text_possrep;
#    has Seq $!seq_possrep;

###########################################################################

submethod BUILD (Str :$text!) {

    die q{new(): Bad :$text arg; it is not a valid object}
            ~ q{ of a Str-doing class.}
        if !$text.defined or !$text.does(Str);

    $!text_possrep = $text;

    return;
}

###########################################################################

method text of Str () {
    return $!text_possrep;
}

###########################################################################

method seq of Seq () {
    die "not implemented";
}

###########################################################################

} # class QDRDBMS::AST::EntityName

###########################################################################
###########################################################################

role QDRDBMS::AST::Expr {}

###########################################################################
###########################################################################

class QDRDBMS::AST::LitDefExpr {
    does QDRDBMS::AST::Expr;

#    has Bool|Str|Blob|Int        $!lit_val;
    has Scalar $!lit_val;
    has QDRDBMS::AST::EntityName $!lit_type;

###########################################################################

#submethod BUILD (Bool|Str|Blob|Int :lit($lit_val)!) {
submethod BUILD (Bool|Str|Blob|Int :$lit!) {
    my $lit_val = $lit;

    die q{new(): Bad :$lit arg; it is not an object.}
        if !$lit_val.defined;
    my $lit_class = $lit_val.WHAT;
    if (my $lit_type = $LITERAL_TYPE_MAP{$lit_class}) {
        $!lit_val  = $lit_val;
        $!lit_type = $lit_type;
    }
    else {
        die q{new(): Bad :$lit arg; it is not an object of a}
            ~ q{ (Bool|Str|Blob|Int) class.};
    }

    return;
}

###########################################################################

#sub lit of Bool|Str|Blob|Int () {
sub lit of Scalar () {
    return $!lit_val;
}

###########################################################################

sub lit_type of QDRDBMS::AST::EntityName () {
    return $!lit_type;
}

###########################################################################

} # class QDRDBMS::AST::LitDefExpr

###########################################################################
###########################################################################

class QDRDBMS::AST::VarNameExpr {
    does QDRDBMS::AST::Expr;

    has QDRDBMS::AST::EntityName $!var_name;

###########################################################################

#submethod BUILD (QDRDBMS::AST::EntityName :var($var_name)!) {
submethod BUILD (QDRDBMS::AST::EntityName :$var!) {
    my $var_name = $var;

    die q{new(): Bad :$var arg; it is not a valid object}
            ~ q{ of a QDRDBMS::AST::EntityName-doing class.}
        if !$var_name.defined or !$var_name.does(QDRDBMS::AST::EntityName);
    $!var_name = $var_name;

    return;
}

###########################################################################

sub var of QDRDBMS::AST::EntityName () {
    return $!var_name;
}

###########################################################################

} # class QDRDBMS::AST::VarNameExpr

###########################################################################
###########################################################################

class QDRDBMS::AST::FuncInvoExpr {
    does QDRDBMS::AST::Expr;

    has QDRDBMS::AST::EntityName $!func_name;
    has Array                    $!func_args_aoa;
    has Hash                     $!func_args_hoa;

###########################################################################

#submethod BUILD (QDRDBMS::AST::EntityName :func($func_name)!,
#        Array :$func_args!) {
submethod BUILD (QDRDBMS::AST::EntityName :$func!, Array :$func_args!) {
    my $func_name = $func;

    die q{new(): Bad :$func arg; it is not a valid object}
            ~ q{ of a QDRDBMS::AST::EntityName-doing class.}
        if !$func_name.defined
            or !$func_name.does(QDRDBMS::AST::EntityName);
    $!func_name = $func_name;

    die q{new(): Bad :$func_args arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$func_args.defined or !$func_args.does(Array);
    my Array $func_args_aoa = [];
    my Hash  $func_args_hoa = {};
    for $func_args -> $elem {
        die q{new(): Bad :$func_args arg; it is not an object of a}
                ~ q{ Array-doing class, or it doesn't have 2 elements.}
            if !$elem.defined or !$elem.does(Array) or $elem.elems != 2;
        my ($param_name, $expr_ast) = $elem.values;
        die q{new(): Bad :$func_args arg elem; its first elem is not}
                ~ q{ an object of a QDRDBMS::AST::EntityName-doing class.}
            if !$param_name.defined
                or !$param_name.does(QDRDBMS::AST::EntityName);
        my Str $param_name_text = $param_name.text();
        die q{new(): Bad :$func_args arg elem; its first elem is not}
                ~ q{ distinct between the arg elems.}
            if $func_args_hoa.exists($param_name_text);
        die q{new(): Bad :$func_args arg elem; its second elem is not}
                ~ q{ an object of a QDRDBMS::AST::Expr-doing class.}
            if !$expr_ast.defined
                or !$expr_ast.does(QDRDBMS::AST::Expr);
        my Array $elem_cpy = [$param_name, $expr_ast];
        $func_args_aoa.push( $elem_cpy );
        $func_args_hoa{$param_name_text} = $elem_cpy;
    }
    $!func_args_aoa = $func_args_aoa;
    $!func_args_hoa = $func_args_hoa;

    return;
}

###########################################################################

sub func of QDRDBMS::AST::EntityName () {
    return $!func_name;
}

sub func_args of Str () {
    return [$!func_args_aoa.map:{ [.values] }];
}

sub func_args_hoa of Str () {
    return {$!func_args_hoa.pairs.map:{ .key => [.value.values] }};
}

###########################################################################

} # class QDRDBMS::AST::FuncInvoExpr

###########################################################################
###########################################################################

role QDRDBMS::AST::Stmt {}

###########################################################################
###########################################################################

class QDRDBMS::AST::ControlStmt {
    does QDRDBMS::AST::Stmt;



###########################################################################




###########################################################################

} # class QDRDBMS::AST::ControlStmt

###########################################################################
###########################################################################

class QDRDBMS::AST::ProcInvoStmt {
    does QDRDBMS::AST::Stmt;

    has QDRDBMS::AST::EntityName $!proc_name;
    has Array                    $!proc_args_aoa;
    has Hash                     $!proc_args_hoa;

###########################################################################

#submethod BUILD (QDRDBMS::AST::EntityName :proc($proc_name)!,
#        Array :$proc_args!) {
submethod BUILD (QDRDBMS::AST::EntityName :$proc!, Array :$proc_args!) {
    my $proc_name = $proc;

    die q{new(): Bad :$proc arg; it is not a valid object}
            ~ q{ of a QDRDBMS::AST::EntityName-doing class.}
        if !$proc_name.defined
            or !$proc_name.does(QDRDBMS::AST::EntityName);
    $!proc_name = $proc_name;

    die q{new(): Bad :$proc_args arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$proc_args.defined or !$proc_args.does(Array);
    my Array $proc_args_aoa = [];
    my Hash  $proc_args_hoa = {};
    for $proc_args -> $elem {
        die q{new(): Bad :$proc_args arg; it is not an object of a}
                ~ q{ Array-doing class, or it doesn't have 2 elements.}
            if !$elem.defined or !$elem.does(Array) or $elem.elems != 2;
        my ($param_name, $expr_ast) = $elem.values;
        die q{new(): Bad :$proc_args arg elem; its first elem is not}
                ~ q{ an object of a QDRDBMS::AST::EntityName-doing class.}
            if !$param_name.defined
                or !$param_name.does(QDRDBMS::AST::EntityName);
        my Str $param_name_text = $param_name.text();
        die q{new(): Bad :$proc_args arg elem; its first elem is not}
                ~ q{ distinct between the arg elems.}
            if $proc_args_hoa.exists($param_name_text);
        die q{new(): Bad :$proc_args arg elem; its second elem is not}
                ~ q{ an object of a QDRDBMS::AST::Expr-doing class.}
            if !$expr_ast.defined
                or !$expr_ast.does(QDRDBMS::AST::Expr);
        my Array $elem_cpy = [$param_name, $expr_ast];
        $proc_args_aoa.push( $elem_cpy );
        $proc_args_hoa{$param_name_text} = $elem_cpy;
    }
    $!proc_args_aoa = $proc_args_aoa;
    $!proc_args_hoa = $proc_args_hoa;

    return;
}

###########################################################################

sub proc of QDRDBMS::AST::EntityName () {
    return $!proc_name;
}

sub proc_args of Str () {
    return [$!proc_args_aoa.map:{ [.values] }];
}

sub proc_args_hoa of Str () {
    return {$!proc_args_hoa.pairs.map:{ .key => [.value.values] }};
}

###########################################################################

} # class QDRDBMS::AST::ProcInvoStmt

###########################################################################
###########################################################################

class QDRDBMS::AST::MultiProcInvoStmt {
    does QDRDBMS::AST::Stmt;



###########################################################################




###########################################################################

} # class QDRDBMS::AST::MultiProcInvoStmt

###########################################################################
###########################################################################

class QDRDBMS::AST::Func {



###########################################################################




###########################################################################

} # class QDRDBMS::AST::Func

###########################################################################
###########################################################################

class QDRDBMS::AST::Proc {



###########################################################################




###########################################################################

} # class QDRDBMS::AST::Proc

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
