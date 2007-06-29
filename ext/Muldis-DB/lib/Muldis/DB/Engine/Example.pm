use v6-alpha;

use Muldis::DB;
use Muldis::DB::Engine::Example::Operators;

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example-0.0.1 {
    # Note: This given version applies to all of this file's packages.

    does Muldis::DB::Engine::Role;

###########################################################################

submethod new_dbms of Muldis::DB::Engine::Example::DBMS
        (Any :$dbms_config!) {
    return ::Muldis::DB::Engine::Example::DBMS.new(
        :dbms_config($dbms_config) );
}

###########################################################################

} # class Muldis::DB::Engine::Example

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::DBMS {
    does Muldis::DB::Engine::Role::DBMS;

    has Any $!dbms_config;

###########################################################################

submethod BUILD (Any :$dbms_config!) {
    $!dbms_config = $dbms_config;
    return;
}

###########################################################################

method new_var of Muldis::DB::Engine::Example::HostGateVar
        (Muldis::DB::AST::TypeInvo :$decl_type!) {
    return ::Muldis::DB::Engine::Example::HostGateVar.new(
        :dbms(self), :decl_type($decl_type) );
}

method prepare of Muldis::DB::Engine::Example::HostGateRtn
        (Muldis::DB::AST::HostGateRtn :$rtn_ast!) {
    return ::Muldis::DB::Engine::Example::HostGateRtn.new(
        :dbms(self), :rtn_ast($rtn_ast) );
}

###########################################################################

} # class Muldis::DB::Engine::Example::DBMS

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::HostGateVar {
    does Muldis::DB::Engine::Role::HostGateVar;

    use Muldis::DB::AST <newBoolLit>;

    has Muldis::DB::Engine::Example::DBMS $!dbms;
    has Muldis::DB::AST::TypeInvo         $!decl_type;
    has Muldis::DB::AST::Node             $!val_ast;

    trusts Muldis::DB::Engine::Example::HostGateRtn;

###########################################################################

submethod BUILD (Muldis::DB::Engine::Example::DBMS :$dbms!,
        Muldis::DB::AST::TypeInvo :$decl_type!) {

    $!dbms      = $dbms;
    $!decl_type = $decl_type;
    $!val_ast   = newBoolLit( :v(Bool::False) );
        # TODO: make default value of $decl_type

    return;
}

###########################################################################

method fetch_ast of Muldis::DB::AST::Node () {
    return $!val_ast;
}

###########################################################################

method store_ast (Muldis::DB::AST::Node :$val_ast!) {
    $!val_ast = $val_ast;
    return;
}

###########################################################################

} # class Muldis::DB::Engine::Example::HostGateVar

###########################################################################
###########################################################################

class Muldis::DB::Engine::Example::HostGateRtn {
    does Muldis::DB::Engine::Role::HostGateRtn;

    has Muldis::DB::Engine::Example::DBMS $!dbms;
    has Muldis::DB::AST::HostGateRtn      $!rtn_ast;
    has Code                              $!prep_rtn;
    has Hash                              $!bound_upd_args;
    has Hash                              $!bound_ro_args;

###########################################################################

submethod BUILD (Muldis::DB::Engine::Example::DBMS :$dbms!,
        Muldis::DB::AST::HostGateRtn :$rtn_ast!) {

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

} # class Muldis::DB::Engine::Example::HostGateRtn

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Muldis::DB::Engine::Example -
Self-contained reference implementation of a Muldis::DB Engine

=head1 VERSION

This document describes Muldis::DB::Engine::Example version 0.0.1 for Perl
6.

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
L<Muldis::DB-0.0.1|Muldis::DB>.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

Go to L<Muldis::DB> for the majority of distribution-internal references,
and L<Muldis::DB::SeeAlso> for the majority of distribution-external
references.

=head1 BUGS AND LIMITATIONS

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
