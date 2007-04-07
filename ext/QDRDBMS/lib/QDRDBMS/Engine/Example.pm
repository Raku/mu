use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use QDRDBMS;
use QDRDBMS::Engine::Example::PhysType;

###########################################################################
###########################################################################

{ package QDRDBMS::Engine::Example; # package
    our $VERSION = 0.000;
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub new_dbms {
    my (undef, $args) = @_;
    return QDRDBMS::Engine::Example::DBMS->new( $args );
}

###########################################################################

} # package QDRDBMS::Engine::Example

###########################################################################
###########################################################################

{ package QDRDBMS::Engine::Example::DBMS; # class

    use Carp;

    my $ATTR_DBMS_CONFIG = 'dbms_config';

###########################################################################

sub new {
    my ($class, $args) = @_;
    my $self = bless {}, $class;
    my ($dbms_config) = @{$args}{'dbms_config'};

    $self->{$ATTR_DBMS_CONFIG} = $dbms_config;

    return $self;
}

###########################################################################

sub prepare_routine {
    my ($self, $args) = @_;
    $args = {%{$args}, 'dbms' => $self};
    return QDRDBMS::Engine::Example::Routine->new( $args );
}

sub new_variable {
    my ($self, $args) = @_;
    $args = {%{$args}, 'dbms' => $self};
    return QDRDBMS::Engine::Example::Variable->new( $args );
}

###########################################################################

} # class QDRDBMS::Engine::Example::DBMS

###########################################################################
###########################################################################

{ package QDRDBMS::Engine::Example::Routine; # class

    use Carp;

    my $ATTR_DBMS_ENG   = 'dbms_eng';
    my $ATTR_RTN_AST    = 'rtn_ast';
    my $ATTR_PREP_RTN   = 'prep_rtn';
    my $ATTR_BOUND_VARS = 'bound_vars';

###########################################################################

sub new {
    my ($class, $args) = @_;
    my $self = bless {}, $class;
    my ($dbms_eng, $rtn_ast) = @{$args}{'dbms', 'routine'};

    my $prep_rtn = sub { 1; }; # TODO; the real thing.

    $self->{$ATTR_DBMS_ENG}   = $dbms_eng;
    $self->{$ATTR_RTN_AST}    = $rtn_ast;
    $self->{$ATTR_PREP_RTN}   = $prep_rtn;
    $self->{$ATTR_BOUND_VARS} = {};

    return $self;
}

###########################################################################

sub bind_variables {
    my ($self, $args) = @_;
    my ($var_engs) = @{$args}{'variables'};
    $self->{$ATTR_BOUND_VARS}
        = {%{$self->{$ATTR_BOUND_VARS}}, %{$var_engs}};
    return;
}

###########################################################################

sub execute {
    my ($self, undef) = @_;
    $self->{$ATTR_PREP_RTN}->( $self->{$ATTR_BOUND_VARS} );
    return;
}

###########################################################################

} # class QDRDBMS::Engine::Example::Routine

###########################################################################
###########################################################################

{ package QDRDBMS::Engine::Example::Variable; # class

    use Carp;

    my $ATTR_DBMS_ENG = 'dbms_eng';

###########################################################################

sub new {
    my ($class, $args) = @_;
    my $self = bless {}, $class;
    my ($dbms_eng) = @{$args}{'dbms'};

    $self->{$ATTR_DBMS_ENG} = $dbms_eng;

    return $self;
}

###########################################################################

} # class QDRDBMS::Engine::Example::Variable

###########################################################################
###########################################################################

{ package QDRDBMS::Engine::Example::Value; # class



###########################################################################



###########################################################################

} # class QDRDBMS::Engine::Example::Value

###########################################################################
###########################################################################

1; # Magic true value required at end of a reuseable file's code.
__END__

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

This file requires any version of Perl 5.x.y that is at least 5.8.1.

It also requires these Perl 5 classes that are in the current distribution:
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
