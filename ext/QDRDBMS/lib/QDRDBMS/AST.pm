use v6-alpha;

###########################################################################
###########################################################################

my $LITERAL_TYPE_MAP = {
    'Bool' => QDRDBMS::AST::TypeRef.new({ 'text' => 'sys.type.Bool' }),
    'Str'  => QDRDBMS::AST::TypeRef.new({ 'text' => 'sys.type.Text' }),
    'Blob' => QDRDBMS::AST::TypeRef.new({ 'text' => 'sys.type.Blob' }),
    'Int'  => QDRDBMS::AST::TypeRef.new({ 'text' => 'sys.type.Int' }),
    'Num'  => QDRDBMS::AST::TypeRef.new({ 'text' => 'sys.type.Num.Rat' }),
};

###########################################################################
###########################################################################

module QDRDBMS::AST-0.0.0 {
    # Note: This given version applies to all of this file's packages.

    use base 'Exporter';
    our @EXPORT_OK = qw(
        TypeRef FuncRef ProcRef VarRef
        Expr Stmt Func Proc
    );

###########################################################################

sub TypeRef {
    return QDRDBMS::AST::TypeRef.new( @_ );
}

sub FuncRef {
    return QDRDBMS::AST::FuncRef.new( @_ );
}

sub ProcRef {
    return QDRDBMS::AST::ProcRef.new( @_ );
}

sub VarRef {
    return QDRDBMS::AST::VarRef.new( @_ );
}

sub Expr {
    return QDRDBMS::AST::Expr.new( @_ );
}

sub Stmt {
    return QDRDBMS::AST::Stmt.new( @_ );
}

sub Func {
    return QDRDBMS::AST::Func.new( @_ );
}

sub Proc {
    return QDRDBMS::AST::Proc.new( @_ );
}

###########################################################################

} # module QDRDBMS::AST

###########################################################################
###########################################################################

role QDRDBMS::AST::_EntityRef {

    use Carp;
    use Scalar::Util qw( blessed );

    my $ATTR_TEXT_POSSREP;
    BEGIN { $ATTR_TEXT_POSSREP = 'text_possrep'; }

###########################################################################

submethod BUILD (Str :$text!) {

    confess q{new(): Bad $text arg; it is not a valid object}
            ~ q{ of a Str-doing class.}
        if !blessed $text or !$text.isa( 'Str' );

    $self.{$ATTR_TEXT_POSSREP} = $text;

    return;
}

###########################################################################

sub as_text {
    my ($self) = @_;
    return $self.{$ATTR_TEXT_POSSREP};
}

###########################################################################

} # role QDRDBMS::AST::_EntityRef

###########################################################################
###########################################################################

class QDRDBMS::AST::TypeRef {
    use base 'QDRDBMS::AST::_EntityRef';
} # class QDRDBMS::AST::TypeRef

###########################################################################
###########################################################################

class QDRDBMS::AST::FuncRef {
    use base 'QDRDBMS::AST::_EntityRef';
} # class QDRDBMS::AST::FuncRef

###########################################################################
###########################################################################

class QDRDBMS::AST::ProcRef {
    use base 'QDRDBMS::AST::_EntityRef';
} # class QDRDBMS::AST::ProcRef

###########################################################################
###########################################################################

class QDRDBMS::AST::VarRef {
    use base 'QDRDBMS::AST::_EntityRef';
} # class QDRDBMS::AST::VarRef

###########################################################################
###########################################################################

class QDRDBMS::AST::Expr {

    use Carp;
    use Scalar::Util qw( blessed );

    my $ATTR_KIND = 'kind';
    my $ATTR_LIT_VAL = 'lit_val';
    my $ATTR_LIT_TYPE = 'lit_type';
    my $ATTR_VAR_NAME = 'var_name';
    my $ATTR_FUNC_NAME = 'func_name';
    my $ATTR_FUNC_ARGS_AOA = 'func_args_aoa';
    my $ATTR_FUNC_ARGS_HASH = 'func_args_hash';

###########################################################################

multi submethod BUILD (Bool|Str|Blob|Int|Num :lit($lit_val)!) {
}
multi submethod BUILD (QDRDBMS::AST::VarRef :var($var_ref)!) {
}
multi submethod BUILD (QDRDBMS::AST::FuncRef :func($func_ref)!,
        QDRDBMS::AST::Expr :%func_args!) {

    if (defined $lit_val) {
        $self.{$ATTR_KIND} = 'lit';
        confess q{new(): The args $var, $func, $func_args}
                ~ q{ can not be set when the $lit arg is set.}
            if defined $var_ref or defined $func_ref or defined $func_args;
        my $lit_class = blessed $lit_val;
        confess q{new(): Bad $var arg; it is not an object.}
            if !$lit_class;
        if (my $lit_type = $LITERAL_TYPE_MAP.{$lit_class}) {
            $self.{$ATTR_LIT_VAL} = $lit_val;
            $self.{$ATTR_LIT_TYPE} = $lit_type;
        }
        else {
            confess q{new(): Bad $lit arg; it is not an object of a}
                ~ q{ (Bool|Str|Blob|Int|Num) class.};
        }
    }

    elsif (defined $var_ref) {
        $self.{$ATTR_KIND} = 'var';
        confess q{new(): The args $lit, $func, $func_args}
                ~ q{ can not be set when the $var arg is set.}
            if defined $func_ref or defined $func_args;
        confess q{new(): Bad $var arg; it is not a valid object}
                ~ q{ of a QDRDBMS::AST::VarRef-doing class.}
            if !blessed $var_ref
                or !$var_ref.isa( 'QDRDBMS::AST::VarRef' );
        $self.{$ATTR_VAR_NAME} = $var_ref;
    }

    elsif (defined $func_ref) {
        $self.{$ATTR_KIND} = 'func';
        confess q{new(): Bad $func arg; it is not a valid object}
                ~ q{ of a QDRDBMS::AST::FuncRef-doing class.}
            if !blessed $func_ref
                or !$func_ref.isa( 'QDRDBMS::AST::FuncRef' );
        $self.{$ATTR_FUNC_NAME} = $func_ref;
        if (!defined $func_args) {
            $self.{$ATTR_FUNC_ARGS_AOA}  = [];
            $self.{$ATTR_FUNC_ARGS_HASH} = {};
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
    }

    else {
        confess q{new(): None of the args $lit, $var, $func}
            ~ q{ were set, but one of those must be.};
    }

    return;
}

###########################################################################

} # class QDRDBMS::AST::Expr

###########################################################################
###########################################################################

class QDRDBMS::AST::Stmt {

    use Carp;
    use Scalar::Util qw( blessed );



###########################################################################




###########################################################################

} # class QDRDBMS::AST::Stmt

###########################################################################
###########################################################################

class QDRDBMS::AST::Func {

    use Carp;
    use Scalar::Util qw( blessed );



###########################################################################




###########################################################################

} # class QDRDBMS::AST::Func

###########################################################################
###########################################################################

class QDRDBMS::AST::Proc {

    use Carp;
    use Scalar::Util qw( blessed );



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
