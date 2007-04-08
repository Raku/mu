use v6-alpha;

###########################################################################
###########################################################################

my $LITERAL_TYPE_MAP = {
    'Bool' => QDRDBMS::AST::TypeRef.new( :text('sys.type.Bool') ),
    'Str'  => QDRDBMS::AST::TypeRef.new( :text('sys.type.Text') ),
    'Blob' => QDRDBMS::AST::TypeRef.new( :text('sys.type.Blob') ),
    'Int'  => QDRDBMS::AST::TypeRef.new( :text('sys.type.Int') ),
    'Num'  => QDRDBMS::AST::TypeRef.new( :text('sys.type.Num.Rat') ),
};

###########################################################################
###########################################################################

module QDRDBMS::AST-0.0.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub TypeRef is export {
    return QDRDBMS::AST::TypeRef.new( @_ );
}

sub FuncRef is export {
    return QDRDBMS::AST::FuncRef.new( @_ );
}

sub ProcRef is export {
    return QDRDBMS::AST::ProcRef.new( @_ );
}

sub VarRef is export {
    return QDRDBMS::AST::VarRef.new( @_ );
}

sub LitDefExpr is export {
    return QDRDBMS::AST::LitDefExpr.new( @_ );
}

sub VarRefExpr is export {
    return QDRDBMS::AST::VarRefExpr.new( @_ );
}

sub FuncInvExpr is export {
    return QDRDBMS::AST::FuncInvExpr.new( @_ );
}

sub Stmt is export {
    return QDRDBMS::AST::Stmt.new( @_ );
}

sub Func is export {
    return QDRDBMS::AST::Func.new( @_ );
}

sub Proc is export {
    return QDRDBMS::AST::Proc.new( @_ );
}

###########################################################################

} # module QDRDBMS::AST

###########################################################################
###########################################################################

role QDRDBMS::AST::_EntityRef {
    has Str $!text_possrep;

###########################################################################

submethod BUILD (Str :$text!) {

    confess q{new(): Bad $text arg; it is not a valid object}
            ~ q{ of a Str-doing class.}
        if !blessed $text or !$text.isa( 'Str' );

    $!text_possrep = $text;

    return;
}

###########################################################################

method as_text of Str () {
    return $!text_possrep;
}

###########################################################################

} # role QDRDBMS::AST::_EntityRef

###########################################################################
###########################################################################

class QDRDBMS::AST::TypeRef {
    does QDRDBMS::AST::_EntityRef;
} # class QDRDBMS::AST::TypeRef

###########################################################################
###########################################################################

class QDRDBMS::AST::FuncRef {
    does QDRDBMS::AST::_EntityRef;
} # class QDRDBMS::AST::FuncRef

###########################################################################
###########################################################################

class QDRDBMS::AST::ProcRef {
    does QDRDBMS::AST::_EntityRef;
} # class QDRDBMS::AST::ProcRef

###########################################################################
###########################################################################

class QDRDBMS::AST::VarRef {
    does QDRDBMS::AST::_EntityRef;
} # class QDRDBMS::AST::VarRef

###########################################################################
###########################################################################

class QDRDBMS::AST::LitDefExpr {
    has Bool|Str|Blob|Int|Num $!lit_val;
    has Type                  $!lit_type;

###########################################################################

submethod BUILD (Bool|Str|Blob|Int|Num :lit($lit_val)!) {

    my $lit_class = blessed $lit_val;
    confess q{new(): Bad $lit arg; it is not an object.}
        if !$lit_class;
    if (my $lit_type = $LITERAL_TYPE_MAP.{$lit_class}) {
        $!lit_val  = $lit_val;
        $!lit_type = $lit_type;
    }
    else {
        confess q{new(): Bad $lit arg; it is not an object of a}
            ~ q{ (Bool|Str|Blob|Int|Num) class.};
    }

    return;
}

###########################################################################

} # class QDRDBMS::AST::LitDefExpr

###########################################################################
###########################################################################

class QDRDBMS::AST::VarRefExpr {
    has QDRDBMS::AST::VarRef $!var_name;

###########################################################################

submethod BUILD (QDRDBMS::AST::VarRef :var($var_ref)!) {

    confess q{new(): Bad $var arg; it is not a valid object}
            ~ q{ of a QDRDBMS::AST::VarRef-doing class.}
        if !blessed $var_ref
            or !$var_ref.isa( 'QDRDBMS::AST::VarRef' );
    $!var_name = $var_ref;

    return;
}

###########################################################################

} # class QDRDBMS::AST::VarRefExpr

###########################################################################
###########################################################################

class QDRDBMS::AST::FuncInvExpr {
    has QDRDBMS::AST::FuncRef      $!func_name;
    has Hash of QDRDBMS::AST::Expr $!func_args;

###########################################################################

submethod BUILD (QDRDBMS::AST::FuncRef :func($func_ref)!,
        Hash of QDRDBMS::AST::Expr :$func_args!) {

    confess q{new(): Bad $func arg; it is not a valid object}
            ~ q{ of a QDRDBMS::AST::FuncRef-doing class.}
        if !blessed $func_ref
            or !$func_ref.isa( 'QDRDBMS::AST::FuncRef' );
    $!func_name = $func_ref;
    if (!defined $func_args) {
        $!func_args = {};
    }
    elsif (ref $func_args eq 'ARRAY') {
        # TODO.
    }
    elsif (ref $func_args eq 'HASH') {
        # TODO.
    }
    else {
        confess q{new(): Bad $func_args arg; its not a Array|Hash.};
    }

    return;
}

###########################################################################

} # class QDRDBMS::AST::FuncInvExpr

###########################################################################
###########################################################################

class QDRDBMS::AST::Stmt {



###########################################################################




###########################################################################

} # class QDRDBMS::AST::Stmt

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

This document describes QDRDBMS::AST version 0.0.0.

It also describes the same-number versions of [...].

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
