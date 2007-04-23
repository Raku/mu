use v6-alpha;

use QDRDBMS;
use QDRDBMS::Engine::Example::Operators;

###########################################################################
###########################################################################

module QDRDBMS::Engine::Example-0.0.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub new_dbms of QDRDBMS::Engine::Example::DBMS (Any :$dbms_config!) {
    return ::QDRDBMS::Engine::Example::DBMS.new(
        :dbms_config($dbms_config) );
}

###########################################################################

} # module QDRDBMS::Engine::Example

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::DBMS {
    has Any $!dbms_config;

###########################################################################

submethod BUILD (Any :$dbms_config!) {

    $!dbms_config = $dbms_config;

    return;
}

###########################################################################

method prepare of QDRDBMS::Engine::Example::Routine
        (QDRDBMS::AST::ProcDecl :$rtn_ast!) {
    return QDRDBMS::Engine::Example::Routine.new(
        :dbms(self), :rtn_ast($rtn_ast) );
}

method new_var of QDRDBMS::Engine::Example::Variable () {
    return QDRDBMS::Engine::Example::Variable.new( :dbms(self) );
}

###########################################################################

} # class QDRDBMS::Engine::Example::DBMS

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::Routine {
    has QDRDBMS::Engine::Example::DBMS $!dbms_eng;
    has QDRDBMS::AST::ProcDecl         $!rtn_ast;
    has Code                           $!prep_rtn;
    has Array                          $!bound_vars;

###########################################################################

#submethod BUILD (QDRDBMS::Engine::Example::DBMS :dbms($dbms_eng)!,
submethod BUILD (QDRDBMS::Engine::Example::DBMS :$dbms!,
        QDRDBMS::AST::ProcDecl :$rtn_ast!) {
    my $dbms_eng = $dbms;

    my $prep_rtn = sub { 1; }; # TODO; the real thing.

    $!dbms_eng   = $dbms_eng;
    $!rtn_ast    = $rtn_ast;
    $!prep_rtn   = $prep_rtn;
    $!bound_vars = {};

    return;
}

###########################################################################

#method bind_host_params (Array :vars($var_engs)!) {
method bind_host_params (Array :$vars!) {
    my $var_engs = $vars;
    $!bound_vars.push( $var_engs.values ); # TODO; overwrite dupl names
    return;
}

###########################################################################

method execute () {
    $!prep_rtn.( |%$!bound_vars );
    return;
}

###########################################################################

} # class QDRDBMS::Engine::Example::Routine

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::Variable {
    has QDRDBMS::Engine::Example::DBMS $!dbms_eng;

###########################################################################

#submethod BUILD (QDRDBMS::Engine::Example::DBMS :dbms($dbms_eng)!) {
submethod BUILD (QDRDBMS::Engine::Example::DBMS :$dbms!) {
    my $dbms_eng = $dbms;

    $!dbms_eng = $dbms_eng;

    return;
}

###########################################################################

} # class QDRDBMS::Engine::Example::Variable

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
