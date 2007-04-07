use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use QDRDBMS::GSTV qw( Bool Str Blob Int Num Hash );

###########################################################################
###########################################################################

my $LITERAL_TYPE_MAP = {
    'QDRDBMS::GSTV::Bool'
        => QDRDBMS::AST::TypeRef->new({ 'text' => Str('sys.type.Bool') }),
    'QDRDBMS::GSTV::Str'
        => QDRDBMS::AST::TypeRef->new({ 'text' => Str('sys.type.Text') }),
    'QDRDBMS::GSTV::Blob'
        => QDRDBMS::AST::TypeRef->new({ 'text' => Str('sys.type.Blob') }),
    'QDRDBMS::GSTV::Int'
        => QDRDBMS::AST::TypeRef->new({ 'text' => Str('sys.type.Int') }),
    'QDRDBMS::GSTV::Num'
        => QDRDBMS::AST::TypeRef->new({
            'text' => Str('sys.type.Num.Rat') }),
};

###########################################################################
###########################################################################

{ package QDRDBMS::AST; # module
    our $VERSION = 0.000;
    # Note: This given version applies to all of this file's packages.

    use base 'Exporter';
    our @EXPORT_OK = qw(
        TypeRef FuncRef ProcRef VarRef
        Expr Stmt Func Proc
    );

###########################################################################

sub TypeRef {
    return QDRDBMS::AST::TypeRef->new( @_ );
}

sub FuncRef {
    return QDRDBMS::AST::FuncRef->new( @_ );
}

sub ProcRef {
    return QDRDBMS::AST::ProcRef->new( @_ );
}

sub VarRef {
    return QDRDBMS::AST::VarRef->new( @_ );
}

sub Expr {
    return QDRDBMS::AST::Expr->new( @_ );
}

sub Stmt {
    return QDRDBMS::AST::Stmt->new( @_ );
}

sub Func {
    return QDRDBMS::AST::Func->new( @_ );
}

sub Proc {
    return QDRDBMS::AST::Proc->new( @_ );
}

###########################################################################

} # module QDRDBMS::AST

###########################################################################
###########################################################################

{ package QDRDBMS::AST::_EntityRef; # role

    use Carp;
    use Scalar::Util qw( blessed );

    my $ATTR_TEXT_POSSREP;
    BEGIN { $ATTR_TEXT_POSSREP = 'text_possrep'; }

###########################################################################

sub new {
    my ($class, $args) = @_;
    my $self = bless {}, $class;
    my ($text) = @{$args}{'text'};

    confess q{new(): Bad $text arg; it is not a valid object}
            . q{ of a QDRDBMS::GSTV::Str-doing class.}
        if !blessed $text or !$text->isa( 'QDRDBMS::GSTV::Str' );

    $self->{$ATTR_TEXT_POSSREP} = $text;

    return $self;
}

###########################################################################

sub as_text {
    my ($self) = @_;
    return $self->{$ATTR_TEXT_POSSREP};
}

###########################################################################

} # role QDRDBMS::AST::_EntityRef

###########################################################################
###########################################################################

{ package QDRDBMS::AST::TypeRef; # class
    use base 'QDRDBMS::AST::_EntityRef';
} # class QDRDBMS::AST::TypeRef

###########################################################################
###########################################################################

{ package QDRDBMS::AST::FuncRef; # class
    use base 'QDRDBMS::AST::_EntityRef';
} # class QDRDBMS::AST::FuncRef

###########################################################################
###########################################################################

{ package QDRDBMS::AST::ProcRef; # class
    use base 'QDRDBMS::AST::_EntityRef';
} # class QDRDBMS::AST::ProcRef

###########################################################################
###########################################################################

{ package QDRDBMS::AST::VarRef; # class
    use base 'QDRDBMS::AST::_EntityRef';
} # class QDRDBMS::AST::VarRef

###########################################################################
###########################################################################

{ package QDRDBMS::AST::Expr; # class

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

sub new {
    my ($class, $args) = @_;
    my $self = bless {}, $class;
    my ($lit_val, $var_ref, $func_ref, $func_args)
        = @{$args}{'lit', 'var', 'func', 'func_args'};

    if (defined $lit_val) {
        $self->{$ATTR_KIND} = 'lit';
        confess q{new(): The args $var, $func, $func_args}
                . q{ can not be set when the $lit arg is set.}
            if defined $var_ref or defined $func_ref or defined $func_args;
        my $lit_class = blessed $lit_val;
        confess q{new(): Bad $var arg; it is not an object.}
            if !$lit_class;
        if (my $lit_type = $LITERAL_TYPE_MAP->{$lit_class}) {
            $self->{$ATTR_LIT_VAL} = $lit_val;
            $self->{$ATTR_LIT_TYPE} = $lit_type;
        }
        else {
            confess q{new(): Bad $lit arg; it is not an object of a}
                . q{ QDRDBMS::GSTV::(Bool|Str|Blob|Int|Num) class.};
        }
    }

    elsif (defined $var_ref) {
        $self->{$ATTR_KIND} = 'var';
        confess q{new(): The args $lit, $func, $func_args}
                . q{ can not be set when the $var arg is set.}
            if defined $func_ref or defined $func_args;
        confess q{new(): Bad $var arg; it is not a valid object}
                . q{ of a QDRDBMS::AST::VarRef-doing class.}
            if !blessed $var_ref
                or !$var_ref->isa( 'QDRDBMS::AST::VarRef' );
        $self->{$ATTR_VAR_NAME} = $var_ref;
    }

    elsif (defined $func_ref) {
        $self->{$ATTR_KIND} = 'func';
        confess q{new(): Bad $func arg; it is not a valid object}
                . q{ of a QDRDBMS::AST::FuncRef-doing class.}
            if !blessed $func_ref
                or !$func_ref->isa( 'QDRDBMS::AST::FuncRef' );
        $self->{$ATTR_FUNC_NAME} = $func_ref;
        if (!defined $func_args) {
            $self->{$ATTR_FUNC_ARGS_AOA}  = [];
            $self->{$ATTR_FUNC_ARGS_HASH} = {};
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
            . q{ were set, but one of those must be.};
    }

    return $self;
}

###########################################################################

} # class QDRDBMS::AST::Expr

###########################################################################
###########################################################################

{ package QDRDBMS::AST::Stmt; # class

    use Carp;
    use Scalar::Util qw( blessed );



###########################################################################




###########################################################################

} # class QDRDBMS::AST::Stmt

###########################################################################
###########################################################################

{ package QDRDBMS::AST::Func; # class

    use Carp;
    use Scalar::Util qw( blessed );



###########################################################################




###########################################################################

} # class QDRDBMS::AST::Func

###########################################################################
###########################################################################

{ package QDRDBMS::AST::Proc; # class

    use Carp;
    use Scalar::Util qw( blessed );



###########################################################################




###########################################################################

} # class QDRDBMS::AST::Proc

###########################################################################
###########################################################################

1; # Magic true value required at end of a reuseable file's code.
__END__

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

This file requires any version of Perl 5.x.y that is at least 5.8.1.

It also requires these Perl 5 classes that are in the current distribution:
L<QDRDBMS::GSTV-(0.0.0)|QDRDBMS::GSTV>.

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
