use v6-alpha;

use QDRDBMS;
use QDRDBMS::Engine::Example::PhysType;

###########################################################################
###########################################################################

package QDRDBMS::Engine::Example-0.0.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub new_dbms of QDRDBMS::Engine::Example::DBMS
        (Hash of Any :$dbms_config!) {
    return QDRDBMS::Engine::Example::DBMS.new(
        :dbms_config($dbms_config) );
}

###########################################################################

} # package QDRDBMS::Engine::Example

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::DBMS {

    use Carp;

    my $ATTR_DBMS_CONFIG = 'dbms_config';

###########################################################################

submethod BUILD (Hash of Any :$dbms_config!) {

    $self.{$ATTR_DBMS_CONFIG} = $dbms_config;

    return;
}

###########################################################################

method prepare_routine of QDRDBMS::Engine::Example::Routine
        (QDRDBMS::AST::Proc :routine($rtn_ast)!) {
    return QDRDBMS::Engine::Example::Routine.new(
        :dbms(self), :routine($rtn_ast) );
}

method new_variable of QDRDBMS::Engine::Example::Variable () {
    return QDRDBMS::Engine::Example::Variable.new( :dbms(self) );
}

###########################################################################

} # class QDRDBMS::Engine::Example::DBMS

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::Routine {

    use Carp;

    my $ATTR_DBMS_ENG   = 'dbms_eng';
    my $ATTR_RTN_AST    = 'rtn_ast';
    my $ATTR_PREP_RTN   = 'prep_rtn';
    my $ATTR_BOUND_VARS = 'bound_vars';

###########################################################################

submethod BUILD (QDRDBMS::Engine::Example::DBMS :dbms($dbms_eng)!,
        QDRDBMS::AST::Proc :routine($rtn_ast)!) {

    my $prep_rtn = sub { 1; }; # TODO; the real thing.

    $self.{$ATTR_DBMS_ENG}   = $dbms_eng;
    $self.{$ATTR_RTN_AST}    = $rtn_ast;
    $self.{$ATTR_PREP_RTN}   = $prep_rtn;
    $self.{$ATTR_BOUND_VARS} = {};

    return;
}

###########################################################################

method bind_variables () {
    my ($self, $args) = @_;
    my ($var_engs) = @{$args}{'variables'};
    $self.{$ATTR_BOUND_VARS}
        = {%{$self.{$ATTR_BOUND_VARS}}, %{$var_engs}};
    return;
}

###########################################################################

method execute () {
    $self.{$ATTR_PREP_RTN}.( $self.{$ATTR_BOUND_VARS} );
    return;
}

###########################################################################

} # class QDRDBMS::Engine::Example::Routine

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::Variable {

    use Carp;

    my $ATTR_DBMS_ENG = 'dbms_eng';

###########################################################################

submethod BUILD (QDRDBMS::Engine::Example::DBMS :dbms($dbms_eng)!) {

    $self.{$ATTR_DBMS_ENG} = $dbms_eng;

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

This document describes QDRDBMS::Engine::Example version 0.0.0.

=head1 SYNOPSIS

I<This documentation is pending.>

=head1 DESCRIPTION

I<This documentation is pending.>

=head1 INTERFACE

I<This documentation is pending; this section may also be split into several.>

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
