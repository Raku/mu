use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use QDRDBMS;

###########################################################################
###########################################################################

{ package QDRDBMS::Validator; # module
    our $VERSION = 0.000;
    # Note: This given version applies to all of this file's packages.

    use QDRDBMS::GSTV qw( Bool Str Blob Int Num Hash );
    use Test::More;

###########################################################################

sub main {
    my ($args) = @_;
    my ($engine_name, $dbms_config)
        = @{$args}{'engine_name', 'dbms_config'};

    plan( 'tests' => 1 );

    print "#### QDRDBMS::Validator starting test of $engine_name ####\n";

    # Instantiate a QDRDBMS DBMS / virtual machine.
    my $dbms = QDRDBMS->new_dbms({
        'engine_name' => $engine_name, 'dbms_config' => $dbms_config });
    isa_ok( $dbms, 'QDRDBMS::Interface::DBMS' );

    print "#### QDRDBMS::Validator finished test of $engine_name ####\n";
}

###########################################################################

} # module QDRDBMS::Validator

###########################################################################
###########################################################################

1; # Magic true value required at end of a reuseable file's code.
__END__

=pod

=encoding utf8

=head1 NAME

QDRDBMS::Validator -
A common comprehensive test suite to run against all Engines

=head1 VERSION

This document describes QDRDBMS::Validator version 0.0.0.

=head1 SYNOPSIS

This can be the complete content of the main C<t/*.t> file for an example
QDRDBMS Engine distribution:

    use 5.008001;
    use utf8;
    use strict;
    use warnings FATAL => 'all';

    use QDRDBMS::GSTV qw( Str Hash );

    # Load the test suite.
    use QDRDBMS::Validator;

    # Run the test suite.
    QDRDBMS::Validator::main({
            'engine_name'   => Str('QDRDBMS::Engine::Example'),
            'engine_config' => Hash({}),
        });

    1;

The current release of QDRDBMS::Validator uses L<Test::More> internally,
and C<main()> will invoke it to output what the standard Perl test harness
expects.  I<It is expected that this will change in the future so that
Validator does not use Test::More internally, and rather will simply return
test results in a data structure that the main t/*.t then can disseminate
and pass the components to Test::More itself.>

=head1 DESCRIPTION

The QDRDBMS::Validator Perl 5 module is a common comprehensive test suite
to run against all QDRDBMS Engines.  You run it against a QDRDBMS Engine
module to ensure that the Engine and/or the database behind it implements
the parts of the QDRDBMS API that your application needs, and that the API
is implemented correctly.  QDRDBMS::Validator is intended to guarantee a
measure of quality assurance (QA) for QDRDBMS, so your application can use
the database access framework with confidence of safety.

Alternately, if you are writing a QDRDBMS Engine module yourself,
QDRDBMS::Validator saves you the work of having to write your own test
suite for it.  You can also be assured that if your module passes
QDRDBMS::Validator's approval, then your module can be easily swapped in
for other Engine modules by your users, and that any changes you make
between releases haven't broken something important.

QDRDBMS::Validator would be used similarly to how Sun has an official
validation suite for Java Virtual Machines to make sure they implement the
official Java specification.

For reference and context, please see the FEATURE SUPPORT VALIDATION
documentation section in the core L<QDRDBMS> module.

Note that, as is the nature of test suites, QDRDBMS::Validator will be
getting regular updates and additions, so that it anticipates all of the
different ways that people want to use their databases.  This task is
unlikely to ever be finished, given the seemingly infinite size of the
task.  You are welcome and encouraged to submit more tests to be included
in this suite at any time, as holes in coverage are discovered.

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
L<QDRDBMS::GSTV-(0.0.0)|QDRDBMS::GSTV>,
L<QDRDBMS::AST-(0.0.0)|QDRDBMS::AST>, L<QDRDBMS-0.0.0|QDRDBMS>.

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
