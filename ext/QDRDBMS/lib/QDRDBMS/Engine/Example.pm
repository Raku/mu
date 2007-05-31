use v6-alpha;

use QDRDBMS;
use QDRDBMS::Engine::Example::Operators;

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example-0.0.0 {
    # Note: This given version applies to all of this file's packages.

    does QDRDBMS::Engine::Role;

###########################################################################

submethod new_dbms of QDRDBMS::Engine::Example::DBMS (Any :$dbms_config!) {
    return ::QDRDBMS::Engine::Example::DBMS.new(
        :dbms_config($dbms_config) );
}

###########################################################################

} # class QDRDBMS::Engine::Example

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::DBMS {
    does QDRDBMS::Engine::Role::DBMS;

    has Any $!dbms_config;

###########################################################################

submethod BUILD (Any :$dbms_config!) {

    $!dbms_config = $dbms_config;

    return;
}

###########################################################################

method new_var of QDRDBMS::Engine::Example::HostGateVar
        (QDRDBMS::AST::TypeInvo :$decl_type!) {
    return ::QDRDBMS::Engine::Example::HostGateVar.new(
        :dbms(self), :decl_type($decl_type) );
}

method prepare of QDRDBMS::Engine::Example::HostGateRtn
        (QDRDBMS::AST::HostGateRtn :$rtn_ast!) {
    return ::QDRDBMS::Engine::Example::HostGateRtn.new(
        :dbms(self), :rtn_ast($rtn_ast) );
}

###########################################################################

} # class QDRDBMS::Engine::Example::DBMS

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::HostGateVar {
    does QDRDBMS::Engine::Role::HostGateVar;

    use QDRDBMS::AST <newLitBool>;

    has QDRDBMS::Engine::Example::DBMS $!dbms;
    has QDRDBMS::AST::TypeInvo         $!decl_type;
    has QDRDBMS::AST::Node             $!val_ast;

    trusts QDRDBMS::Engine::Example::HostGateRtn;

###########################################################################

submethod BUILD (QDRDBMS::Engine::Example::DBMS :$dbms!,
        QDRDBMS::AST::TypeInvo :$decl_type!) {

    $!dbms      = $dbms;
    $!decl_type = $decl_type;
    $!val_ast   = newLitBool( :v(Bool::False) );
        # TODO: make default value of $decl_type

    return;
}

###########################################################################

method fetch_ast of QDRDBMS::AST::Node () {
    return $!val_ast;
}

###########################################################################

method store_ast (QDRDBMS::AST::Node :$val_ast!) {

    $!val_ast = $val_ast;

    return;
}

###########################################################################

} # class QDRDBMS::Engine::Example::HostGateVar

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::HostGateRtn {
    does QDRDBMS::Engine::Role::HostGateRtn;

    has QDRDBMS::Engine::Example::DBMS $!dbms;
    has QDRDBMS::AST::HostGateRtn      $!rtn_ast;
    has Code                           $!prep_rtn;
    has Hash                           $!bound_upd_args;
    has Hash                           $!bound_ro_args;

###########################################################################

submethod BUILD (QDRDBMS::Engine::Example::DBMS :$dbms!,
        QDRDBMS::AST::HostGateRtn :$rtn_ast!) {

    my $prep_rtn = sub { 1; }; # TODO; the real thing.

    $!dbms           = $dbms;
    $!rtn_ast        = $rtn_ast;
    $!prep_rtn       = $prep_rtn;
    $!bound_upd_args = {};
    $!bound_ro_args  = {};

    return;
}

###########################################################################

method bind_host_params (Array :$upd_args!, Array :$ro_args!) {
    # TODO: Compare declared type of each routine param and the variable
    # we are trying to bind to it, that they are of compatible types.
    # TODO: Fix this!
#    for $upd_args -> $elem {
#        $!bound_upd_args.{$elem.[0].text()} = $elem.[1];
#    }
#    for $ro_args -> $elem {
#        $!bound_ro_args.{$elem.[0].text()} = $elem.[1];
#    }
    return;
}

###########################################################################

method execute () {
    # TODO: Fix this!
#    $!prep_rtn.( |%$!bound_upd_args, |%$!bound_ro_args );
    return;
}

###########################################################################

} # class QDRDBMS::Engine::Example::HostGateRtn

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::Value {



###########################################################################



###########################################################################

} # class QDRDBMS::Engine::Example::Value

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

QDRDBMS::Engine::Example -
Self-contained reference implementation of a QDRDBMS Engine

=head1 VERSION

This document describes QDRDBMS::Engine::Example version 0.0.0 for Perl 6.

=head1 SYNOPSIS

I<This documentation is pending.>

=head1 DESCRIPTION

I<This documentation is pending.>

=head1 INTERFACE

I<This documentation is pending; this section may also be split into
several.>

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 6.x.y that is at least 6.0.0.

It also requires these Perl 6 classes that are in the current distribution:
L<QDRDBMS-0.0.0|QDRDBMS>.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

Go to L<QDRDBMS> for the majority of distribution-internal references, and
L<QDRDBMS::SeeAlso> for the majority of distribution-external references.

=head1 BUGS AND LIMITATIONS

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
