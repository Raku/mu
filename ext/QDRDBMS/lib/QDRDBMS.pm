use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use QDRDBMS::AST;

###########################################################################
###########################################################################

{ package QDRDBMS; # package
    our $VERSION = 0.000;
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub new_dbms {
    my (undef, $args) = @_;
    return QDRDBMS::Interface::DBMS->new( $args );
}

###########################################################################

} # package QDRDBMS

###########################################################################
###########################################################################

{ package QDRDBMS::Interface::DBMS; # class

    use Carp;
    use Scalar::Util qw( blessed );

    my $ATTR_DBMS_ENG = 'dbms_eng';

###########################################################################

sub new {
    my ($class, $args) = @_;
    my $self = bless {}, $class;
    my ($engine_name, $dbms_config)
        = @{$args}{'engine_name', 'dbms_config'};

    confess q{new(): Bad $engine_name arg; it is not an object of a}
            . q{ QDRDBMS::GSTV::Str-doing class.}
        if !blessed $engine_name
            or !$engine_name->isa( 'QDRDBMS::GSTV::Str' );
    $engine_name = ${$engine_name};

    if (defined $dbms_config) {
        confess q{new(): Bad $dbms_config arg; it is not an object of a}
                . q{ QDRDBMS::GSTV::Hash-doing class.}
            if !blessed $dbms_config
                or !$dbms_config->isa( 'QDRDBMS::GSTV::Hash' );
    }
    else {
        $dbms_config = {};
    }

    # A package may be loaded due to it being embedded in a non-excl file.
    if (!do {
            no strict 'refs';
            defined %{$engine_name . '::'};
        }) {
        # Note: We have to invoke this 'require' in an eval string
        # because we need the bareword semantics, where 'require'
        # will munge the package name into file system paths.
        eval "require $engine_name;";
        if (my $err = $@) {
            confess q{new(): Could not load QDRDBMS Engine class}
                . qq{ '$engine_name': $err};
        }
        confess qq{new(): Could not load QDRDBMS Engine class}
                . qq{ '$engine_name': while that file did compile without}
                . q{ errors, it did not declare the same-named package.}
            if !do {
                no strict 'refs';
                defined %{$engine_name . '::'};
            };
    }
    confess qq{new(): The QDRDBMS Engine class '$engine_name' does not}
            . q{ provide the new_dbms() constructor function.}
        if !$engine_name->can( 'new_dbms' );
    my $dbms_eng = eval {
        $engine_name->new_dbms({ 'dbms_config' => $dbms_config });
    };
    if (my $err = $@) {
        confess qq{new(): The QDRDBMS Engine class '$engine_name' threw an}
            . qq{ exception during its new_dbms() execution: $err}
    }
    confess q{new(): The new_dbms() constructor function of the QDRDBMS}
            . qq{ Engine class '$engine_name' did not return an object}
            . q{ to serve as a DBMS Engine.}
        if !blessed $dbms_eng;
    my $dbms_eng_class = blessed $dbms_eng;

    confess qq{new(): The QDRDBMS DBMS Engine class '$dbms_eng_class' does}
            . q{ not provide the prepare_routine() method.}
        if !$dbms_eng->can( 'prepare_routine' );
    confess qq{new(): The QDRDBMS DBMS Engine class '$dbms_eng_class' does}
            . q{ not provide the new_variable() method.}
        if !$dbms_eng->can( 'new_variable' );

    $self->{$ATTR_DBMS_ENG} = $dbms_eng;

    return $self;
}

###########################################################################

sub prepare_routine {
    my ($self, $args) = @_;
    $args = {%{$args}, 'dbms' => $self};
    return QDRDBMS::Interface::Routine->new( $args );
}

sub new_variable {
    my ($self, $args) = @_;
    $args = {%{$args}, 'dbms' => $self};
    return QDRDBMS::Interface::Variable->new( $args );
}

###########################################################################

} # class QDRDBMS::Interface::DBMS

###########################################################################
###########################################################################

{ package QDRDBMS::Interface::Routine; # class

    use Carp;
    use Scalar::Util qw( blessed );

    my $ATTR_DBMS_INTF = 'dbms_intf';
    my $ATTR_RTN_AST   = 'rtn_ast';
    my $ATTR_RTN_ENG   = 'rtn_eng';

    my $DBMS_ATTR_DBMS_ENG = 'dbms_eng';
    my $VAR_ATTR_VAR_ENG   = 'var_eng';

###########################################################################

sub new {
    my ($class, $args) = @_;
    my $self = bless {}, $class;
    my ($dbms_intf, $rtn_ast) = @{$args}{'dbms', 'routine'};

    confess q{new(): Bad $dbms arg; it is not an object of a}
            . q{ QDRDBMS::Interface::DBMS-doing class.}
        if !blessed $dbms_intf
            or !$dbms_intf->isa( 'QDRDBMS::Interface::DBMS' );
    my $dbms_eng = $dbms_intf->{$DBMS_ATTR_DBMS_ENG};
    my $dbms_eng_class = blessed $dbms_eng;

    confess q{new(): Bad $dbms arg; it is not an object of a}
            . q{ QDRDBMS::AST::Routine-doing class.}
        if !blessed $rtn_ast or !$rtn_ast->isa( 'QDRDBMS::AST::Routine' );

    my $rtn_eng = eval {
        $dbms_eng->prepare_routine({ 'routine' => $rtn_ast });
    };
    if (my $err = $@) {
        confess qq{new(): The QDRDBMS DBMS Engine class '$dbms_eng_class'}
            . q{ threw an exception during its prepare_routine()}
            . qq{ execution: $err}
    }
    confess q{new(): The prepare_routine() method of the QDRDBMS}
            . qq{ DBMS class '$dbms_eng_class' did not return an object}
            . q{ to serve as a Routine Engine.}
        if !blessed $rtn_eng;
    my $rtn_eng_class = blessed $rtn_eng;

    confess qq{new(): The QDRDBMS Routine Engine class '$rtn_eng_class'}
            . q{ does not provide the bind_variables() method.}
        if !$rtn_eng->can( 'bind_variables' );
    confess qq{new(): The QDRDBMS Routine Engine class '$rtn_eng_class'}
            . q{ does not provide the execute() method.}
        if !$rtn_eng->can( 'execute' );

    $self->{$ATTR_DBMS_INTF} = $dbms_intf;
    $self->{$ATTR_RTN_AST}   = $rtn_ast;
    $self->{$ATTR_RTN_ENG}   = $rtn_eng;

    return $self;
}

###########################################################################

sub bind_variables {
    my ($self, $args) = @_;
    my ($var_intfs) = @{$args}{'variables'};

    confess q{new(): Bad $variables arg; it is not an object of a}
            . q{ QDRDBMS::GSTV::Hash-doing class.}
        if !blessed $var_intfs
            or !$var_intfs->isa( 'QDRDBMS::GSTV::Hash' );

    my $var_engs = {};
    for my $var_name (keys %{$var_intfs}) {
        my $var_intf = $var_intfs->{$var_name};
        confess q{new(): Bad $var_value arg elem; it is not an object of a}
                . q{ QDRDBMS::Interface::Variable-doing class.}
            if !blessed $var_intf
                or !$var_intf->isa( 'QDRDBMS::Interface::Variable' );
        $var_engs->{$var_name} = $var_intf->{$VAR_ATTR_VAR_ENG};
    }

    $self->{$ATTR_RTN_ENG}->bind_variables({ 'variables' => $var_engs });
    return;
}

###########################################################################

sub execute {
    my ($self, undef) = @_;
    $self->{$ATTR_RTN_ENG}->execute();
    return;
}

###########################################################################

} # class QDRDBMS::Interface::Routine

###########################################################################
###########################################################################

{ package QDRDBMS::Interface::Variable; # class

    use Carp;
    use Scalar::Util qw( blessed );

    my $ATTR_DBMS_INTF = 'dbms_intf';
    my $ATTR_VAR_ENG   = 'var_eng';

    my $DBMS_ATTR_DBMS_ENG = 'dbms_eng';

###########################################################################

sub new {
    my ($class, $args) = @_;
    my $self = bless {}, $class;
    my ($dbms_intf) = @{$args}{'dbms'};

    confess q{new(): Bad $dbms arg; it is not an object of a}
            . q{ QDRDBMS::Interface::DBMS-doing class.}
        if !blessed $dbms_intf
            or !$dbms_intf->isa( 'QDRDBMS::Interface::DBMS' );
    my $dbms_eng = $dbms_intf->{$DBMS_ATTR_DBMS_ENG};
    my $dbms_eng_class = blessed $dbms_eng;

    my $var_eng = eval {
        $dbms_eng->new_variable({});
    };
    if (my $err = $@) {
        confess qq{new(): The QDRDBMS DBMS Engine class '$dbms_eng_class'}
            . q{ threw an exception during its new_variable()}
            . qq{ execution: $err}
    }
    confess q{new(): The prepare_routine() method of the QDRDBMS}
            . qq{ DBMS class '$dbms_eng_class' did not return an object}
            . q{ to serve as a Routine Engine.}
        if !blessed $var_eng;
    my $var_eng_class = blessed $var_eng;

#    confess qq{new(): The QDRDBMS Variable Engine class '$var_eng_class'}
#            . q{ does not provide the ...() method.}
#        if !$var_eng->can( '...' );

    $self->{$ATTR_DBMS_INTF} = $dbms_intf;
    $self->{$ATTR_VAR_ENG} = $var_eng;

    return $self;
}

###########################################################################

} # class QDRDBMS::Interface::Variable

###########################################################################
###########################################################################

1; # Magic true value required at end of a reuseable file's code.
__END__

=pod

=encoding utf8

=head1 NAME

QDRDBMS -
A fully-featured truly relational DBMS in Perl

=head1 VERSION

This document describes QDRDBMS version 0.0.0.

It also describes the same-number versions of QDRDBMS::Interface::DBMS
("DBMS"), QDRDBMS::Interface::Routine ("Routine"), and
QDRDBMS::Interface::Variable ("Variable").

I<Note that the "QDRDBMS" package serves only as the name-sake
representative for this whole file, which can be referenced as a unit by
documentation or 'use' statements or Perl archive indexes.  Aside from
'use' statements, you should never refer directly to "QDRDBMS" in your
code; instead refer to other above-named packages in this file.>

=head1 SYNOPSIS

    use QDRDBMS::GSTV qw( Str );

    use QDRDBMS;

    # Instantiate a QDRDBMS DBMS / virtual machine.
    my $dbms = QDRDBMS->new_dbms({
        'engine_name' => Str('QDRDBMS::Engine::Example') });

    # TODO: Create or connect to a repository and work with it.

=head1 DESCRIPTION

The "QDRDBMS" DBMS framework is a powerful but elegant system, which makes
it easy to create and use relational databases in a very reliable,
portable, and efficient way.  This "QDRDBMS" file is the core of the
QDRDBMS framework and defines a truly relational common programmatic
interface (API), called the QDRDBMS Native Interface, which applications
invoke and which multiple interchangeable "Engine" back-ends (usually
provided by third parties) implement.  This interface is rigorously
defined, such that there should be no ambiguity when trying to invoke or
implement it, and so an application written to it should behave identically
no matter which conforming "Engine" is in use.

QDRDBMS incorporates a complete and uncompromising implementation of "The
Third Manifesto" (TTM), a formal proposal by Christopher J. Date and Hugh
Darwen for a solid foundation for data and database management systems
(DBMSs); like Edgar F. Codd's original papers, TTM can be seen as an
abstract blueprint for the design of a DBMS and the language interface to
such a DBMS.  The main web site for TTM is
L<http://www.thethirdmanifesto.com/>, and its authors have also written
several books and papers and taught classes on the subject over the last
35+ years, along with Codd himself (some are listed in the
L<QDRDBMS::SeeAlso> documentation file).  Note that the QDRDBMS
documentation will be focusing mainly on how QDRDBMS itself works, and will
not spend much time in providing rationales; you can read TTM itself and
various other external documentation for much of that.

The QDRDBMS Native Interface is defined mainly in terms of a new high-level
programming language named "QDRDBMS D", which is computationally complete
(and industrial strength) and has fully integrated database functionality;
this language, which satisfies TTM's definition of a "D" language, is
described fully in the L<QDRDBMS::Language> documentation file that comes
with this "QDRDBMS" distribution.

While it is possible that one could write a self-contained application in
QDRDBMS D and compile that into its own executable, in practice one would
normally just write some components of their application in QDRDBMS D (as
either named modules or anonymous routines) and write the rest of the
application in their other language(s) of choice.  Assuming the main
application is written in Perl, it is this "QDRDBMS" file which provides
the glue between your Perl code and your QDRDBMS D code; "QDRDBMS"
implements a virtual machine that is embedded in your Perl application and
in which the QDRDBMS D code runs (it is analagous to the Perl interpreter
itself, which provides a virtual machine in which Perl code runs).

The classes and methods of this "QDRDBMS" file, together with those of
L<QDRDBMS::AST>, define the balance of the QDRDBMS Native Interface.  A
QDRDBMS::Interface::DBMS object represents a single active QDRDBMS virtual
machine; it has a spartan DBI-inspired set of methods which you use to
compile/prepare and/or invoke/execute QDRDBMS D statements and routines
within the virtual machine, input data to it, and output data from it.

You can create more than one DBMS object at a time, and they are
essentially all isolated from each other, even if more than one uses the
same Engine class to implement it; that is, multiple DBMS objects will not
have references to each other at a level visible in the QDRDBMS Native
Interface, if at all.  To account for situations where multiple DBMS
objects want to use the same external resources, such as a repository file
on disk, it is expected that the Engines will employ appropriate measures
such as system-managed locks so that resource corruption or application
failure is prevented.  I<Also, QDRDBMS should be thread safe and/or saavy
in the future, but for now it officially is not and you should not share
QDRDBMS objects between multiple threads, nor have objects in separate
threads try to access the same external resources.>

QDRDBMS does not use any dialect of SQL in its native API (unlike many
other DBMS products) because SQL is more ambiguous and error-prone to use,
and it is less expressive.  While QDRDBMS D is very different from SQL, it
is fully capable of modelling anything in the real world accurately, and it
can support a complete SQL emulation layer on top of it, so that your
legacy applications can be migrated to use the QDRDBMS DBMS with little
trouble.  Likewise, emulation layers for any other programming language can
be supported, such as Tutorial D or XQuery or FoxPro or dBase.

One distinctive feature of a QDRDBMS DBMS (compared to a typical other
vendor's DBMS) is that data definition statements are structured as
standard data manipulation statements but that the target relation
variables are system catalog relation variables rather than user-defined
relation variables.  In SQL terms, you create or alter tables by adding or
updating their "information schema" records, which in SQL are read-only,
not by using special 'create' or 'alter' statements.

Each QDRDBMS Engine has the complete freedom to implement the QDRDBMS DBMS
and QDRDBMS D however it likes; all QDRDBMS cares about is that the user
interface and behaviour conform to its preconceptions.

L<QDRDBMS::Engine::Example> is the self-contained and pure-Perl reference
implementation of an Engine and is included in the "QDRDBMS" core
distribution to allow the core to be completely testable on its own.  It is
coded intentionally in a simple fashion so that it is easy to maintain and
and easy for developers to study.  As a result, while it performs correctly
and reliably, it also performs quite slowly; you should only use Example
for testing, development, and study; you should not use it in production.

For production use, there should be a wide variety of third party Engine
modules that become available over time.  One plan which I favor is that
the new (under development) enterprise-strength and Perl implemented
database server named L<Genezzo> (see also L<http://www.genezzo.com/>) will
evolve to implement the QDRDBMS DBMS natively, and be I<the> back-end which
I recommend above all others for production use.

Most of the other (near term) third party Engines will likely just map
QDRDBMS's rigorously defined API onto a pre-existing quasi-relational
database manager (such as SQLite, PostgreSQL, MySQL, Firebird, Teradata,
Oracle, Sybase, SQL Server, Informix, DB2, OpenBase, FrontBase, etc).
Given this fact, QDRDBMS's most prominant feature is that it provides a
common API for access to those databases, each of which takes a different
SQL or quasi-SQL dialect.  An application written to it should easily port
to alternative relational database engines with minimal effort.

This might seem strange to somebody who has not tried to port between
databases before, especially given that the Perl DBI purports to provide
"Database Independence".  However, the level of DBI's provided independence
is I<Database Driver Independence>, and not I<Database Language
Independence>.  To further demonstrate the difference, it is useful to
compare the DBI and QDRDBMS.  I<Such documentation is currently absent.>

=head1 FEATURE SUPPORT VALIDATION

The QDRDBMS Native Interface declares accessors for a large number of
actual or possible database features, any of which your application can
invoke, and all of which each QDRDBMS Engine would ideally implement or
interface to.

In reality, however, all Engines or underlying databases probably don't
support some features, and if your application tries to invoke any of the
same features that an Engine you are using doesn't support, then you will
have problems ranging from immediate crashes/exceptions to subtle data
corruption over time.

As an official quality assurance (QA) measure, QDRDBMS provides a means for
each Engine to programmatically declare which features it does and does not
support, so that code using that Engine will know so in advance of trying
to use said features.  Feature support declarations are typically coarse
grained and lump closely similar things together, for simplicity; they will
be just as fine grained as necessary and no finer (this can be changed over
time).  See the C<features()> method, which is how you read the
declarations.

One benefit of this QA feature is that, after you have written your
application and it is working with one Engine/database, and you want to
move it to a different Engine/database, you can determine at a glance which
alternatives also support the features you are using.  Note that, generally
speaking, you would have to be using very proprietary features to begin
with in order for the majority of QDRDBMS Engines/databases to not support
the application outright.

Another benefit of this QA feature is that there can be made a common
comprehensive test suite to run against all Engines in order to tell that
they are implementing the QDRDBMS interface properly or not; said test
suite will be smart enough to only test each Engine's compliance for those
features that the Engine claims to support, and not fail it for non-working
features that it explicitly says it doesn't support.  This common test
suite will save each Engine maker from having to write their own module
tests.  It would be used similarly to how Sun has an official validation
suite for Java Virtual Machines to make sure they implement the official
Java specification.  Please see the L<QDRDBMS::Validator> module(s), which
implements this test suite.

=head1 INTERFACE

The interface of QDRDBMS is entirely object-oriented; you use it by
creating objects from its member classes, usually invoking C<new()> on the
appropriate class name, and then invoking methods on those objects.  All of
their attributes are private, so you must use accessor methods.  QDRDBMS
does not declare any subroutines or export such.

The usual way that QDRDBMS indicates a failure is to throw an exception;
most often this is due to invalid input.  If an invoked routine simply
returns, you can assume that it has succeeded, even if the return value is
undefined.

=head2 The QDRDBMS::Interface::DBMS Class

I<This documentation is pending.>

=head2 The QDRDBMS::Interface::Routine Class

I<This documentation is pending.>

=head2 The QDRDBMS::Interface::Variable Class

I<This documentation is pending.>

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 5.x.y that is at least 5.8.1.

It also requires these Perl 5 classes that are in the current distribution:
L<QDRDBMS::GSTV-(0.0.0)|QDRDBMS::GSTV>,
L<QDRDBMS::AST-(0.0.0)|QDRDBMS::AST>.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

These documentation files are included in the QDRDBMS distribution:
L<QDRDBMS::Language>.

The Perl 5 module L<QDRDBMS::Validator> is bundled with QDRDBMS and can be
used to test QDRDBMS Engines.

The Perl 5 package L<QDRDBMS::Engine::Example> is bundled with QDRDBMS and
implements a self-contained reference implementation of a QDRDBMS Engine.

Go to the L<QDRDBMS::SeeAlso> file for the majority of external references.

=head1 BUGS AND LIMITATIONS

The QDRDBMS Perl-5 framework has been built according to certain old-school
or traditional Perl-5-land design principles, including that there are no
explicit attempts in code to enforce privacy of the framework's internals,
besides not documenting them as part of the public API.  (The Perl 6
version of QDRDBMS will be different.)  That said, you should still respect
that privacy and just use the public API that QDRDBMS provides.  If you
bypass the public API anyway, as Perl 5 allows, you do so at your own
peril.

I<This documentation is pending.>

=head1 AUTHOR

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the QDRDBMS framework.

QDRDBMS is Copyright © 2002-2007, Darren Duncan.  All rights reserved.

QDRDBMS is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License (GPL) as published by the Free
Software Foundation (L<http://www.fsf.org/>); either version 2 of the
License, or (at your option) any later version.  You should have received a
copy of the GPL as part of the QDRDBMS distribution, in the file named
"GPL"; if not, see L<http://www.gnu.org/licenses/> or write to the Free
Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
02110-1301 USA.

I<Note that the copyright holders expect that QDRDBMS will subsequently be
relicensed instead under the terms of the GPL version 3.0 or later, as soon
as that license is finalized, assuming no difficulties arise with it.>

Linking QDRDBMS statically or dynamically with other components is making a
combined work based on QDRDBMS.  Thus, the terms and conditions of the GPL
cover the whole combination.  However, for an additional fee, the copyright
holders of QDRDBMS can sell you an alternate license, with a limited scope,
that allows you to link QDRDBMS with non-free software components.

Any versions of QDRDBMS that you modify and distribute must carry prominent
notices stating that you changed the files and the date of any changes, in
addition to preserving this original copyright notice and other credits.
QDRDBMS is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  However, for an additional fee, the copyright
holders of QDRDBMS can sell you a warranty for it.

While it is by no means required, the copyright holders of QDRDBMS would
appreciate being informed any time you create a modified version of QDRDBMS
that you are willing to distribute, because that is a practical way of
suggesting improvements to the standard version.

=head1 ACKNOWLEDGEMENTS

None yet.

=head1 FORUMS

Several public email-based forums for QDRDBMS now exist, all of which you
can reach via L<http://mm.DarrenDuncan.net/mailman/listinfo>; go there to
manage your subscriptions to, or view the archives of, the following:

=over

=item C<qdrdbms-announce@mm.DarrenDuncan.net>

This low-volume list is mainly for official announcements from the QDRDBMS
developers, though developers of QDRDBMS extensions can also post their
announcements here.  This is not a discussion list.

=item C<qdrdbms-users@mm.DarrenDuncan.net>

This list is for general discussion among people who are using QDRDBMS,
which is not concerned with the implementation of QDRDBMS itself.  This is
the best place to ask for basic help in getting QDRDBMS installed on your
machine or to make it do what you want.  You could also submit feature
requests or report perceived bugs here, if you don't want to use CPAN's RT
system.

=item C<qdrdbms-devel@mm.DarrenDuncan.net>

This list is for discussion among people who are designing or implementing
the QDRDBMS core API (including QDRDBMS D language design), or who are
implementing QDRDBMS Engines, or who are writing core documentation, tests,
or examples.  It is not the place for non-implementers to get help in using
said.

=back

An official IRC channel for QDRDBMS is also intended, but not yet started.

Alternately, you can purchase more advanced commercial support for QDRDBMS
from its author; contact C<perl@DarrenDuncan.net> for details.

=cut
