use v6-alpha;

###########################################################################
###########################################################################

#my Hash of QDRDBMS::AST::EntityName $LITERAL_TYPE_MAP = {
my Hash $LITERAL_TYPE_MAP = {
#    'Bool' => QDRDBMS::AST::EntityName.new( :text('sys.type.Bool') ),
#    'Str'  => QDRDBMS::AST::EntityName.new( :text('sys.type.Text') ),
#    'Blob' => QDRDBMS::AST::EntityName.new( :text('sys.type.Blob') ),
#    'Int'  => QDRDBMS::AST::EntityName.new( :text('sys.type.Int') ),
#    'Num'  => QDRDBMS::AST::EntityName.new( :text('sys.type.Num.Rat') ),
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
        (Bool|Str|Blob|Int|Num :$lit!) is export {
    return QDRDBMS::AST::LitDefExpr.new( :lit($lit) );
}

sub VarNameExpr of QDRDBMS::AST::VarNameExpr
        (QDRDBMS::AST::EntityName :$var!) is export {
    return QDRDBMS::AST::VarNameExpr.new( :var($var) );
}

sub FuncInvoExpr of QDRDBMS::AST::FuncInvoExpr
        (QDRDBMS::AST::EntityName :$func!, Hash :$func_args!) is export {
    return QDRDBMS::AST::FuncInvoExpr.new(
        :func($func), :func_args($func_args) );
}

sub ControlStmt of QDRDBMS::AST::ControlStmt () is export {
    return QDRDBMS::AST::ControlStmt.new();
}

sub ProcInvoStmt of QDRDBMS::AST::ProcInvoStmt () is export {
    return QDRDBMS::AST::ProcInvoStmt.new();
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

###########################################################################

submethod BUILD (Str :$text!) {

    die q{new(): Bad :$text arg; it is not a valid object}
            ~ q{ of a Str-doing class.}
        if !$text.defined or !$text.does(Str);

    $!text_possrep = $text;

    return;
}

###########################################################################

method as_text of Str () {
    return $!text_possrep;
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

#    has Bool|Str|Blob|Int|Num $!lit_val;
    has Scalar $!lit_val;
    has Type                  $!lit_type;

###########################################################################

#submethod BUILD (Bool|Str|Blob|Int|Num :lit($lit_val)!) {
submethod BUILD (Bool|Str|Blob|Int|Num :$lit!) {
    my $lit_val = $lit;

    my $lit_class = $lit_val.WHAT;
    die q{new(): Bad :$lit arg; it is not an object.}
        if !$lit_class;
    if (my $lit_type = $LITERAL_TYPE_MAP.{$lit_class}) {
        $!lit_val  = $lit_val;
        $!lit_type = $lit_type;
    }
    else {
        die q{new(): Bad :$lit arg; it is not an object of a}
            ~ q{ (Bool|Str|Blob|Int|Num) class.};
    }

    return;
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

} # class QDRDBMS::AST::VarNameExpr

###########################################################################
###########################################################################

class QDRDBMS::AST::FuncInvoExpr {
    does QDRDBMS::AST::Expr;

    has QDRDBMS::AST::EntityName      $!func_name;
#    has Hash of QDRDBMS::AST::Expr $!func_args;
    has Hash $!func_args;

###########################################################################

#submethod BUILD (QDRDBMS::AST::EntityName :func($func_name)!,
#        Hash of QDRDBMS::AST::Expr :$func_args!) {
submethod BUILD (QDRDBMS::AST::EntityName :$func!,
        Hash :$func_args!) {
    my $func_name = $func;

    die q{new(): Bad :$func arg; it is not a valid object}
            ~ q{ of a QDRDBMS::AST::EntityName-doing class.}
        if !$func_name.defined
            or !$func_name.does(QDRDBMS::AST::EntityName);
    $!func_name = $func_name;
    if (!$func_args.defined) {
        $!func_args = {};
    }
    elsif ($func_args.does(Array)) {
        # TODO.
    }
    elsif ($func_args.does(Hash)) {
        # TODO.
    }
    else {
        die q{new(): Bad :$func_args arg; its not a Array|Hash.};
    }

    return;
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



###########################################################################




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
