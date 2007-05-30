use v6-alpha;

use QDRDBMS;

###########################################################################
###########################################################################

module QDRDBMS::Validator-0.0.0 {

    use Test;

    use QDRDBMS::AST <newLitBool newLitText newLitBlob newLitInt
        newTupleSel newQuasiTupleSel newRelationSel newQuasiRelationSel
        newVarInvo newFuncInvo newProcInvo newFuncReturn newProcReturn
        newEntityName newTypeInvoNQ newTypeInvoAQ newTypeDictNQ
        newTypeDictAQ newExprDict newFuncDecl newProcDecl newHostGateRtn>;

###########################################################################

sub main (Str :$engine_name!, Any :$dbms_config!) {

    plan( 7 );

    say "#### QDRDBMS::Validator starting test of $engine_name ####";

    # Instantiate a QDRDBMS DBMS / virtual machine.
    my QDRDBMS::Interface::DBMS $dbms = QDRDBMS::new_dbms(
        :engine_name($engine_name), :dbms_config($dbms_config) );
    isa_ok( $dbms, 'QDRDBMS::Interface::DBMS' );

    _scenario_foods_suppliers_shipments( $dbms );

    say "#### QDRDBMS::Validator finished test of $engine_name ####";

    return;
}

###########################################################################

sub _scenario_foods_suppliers_shipments (QDRDBMS::Interface::DBMS $dbms!) {

    # Declare our example executable code as QDRDBMS ASTs.

    my $tynm_Text = newEntityName( :text('sys.type.Text') );
    my $tynm_Int  = newEntityName( :text('sys.type.Int') );

    my $atnm_colour  = newEntityName( :text('colour') );
    my $atnm_country = newEntityName( :text('country') );
    my $atnm_farm    = newEntityName( :text('farm') );
    my $atnm_food    = newEntityName( :text('food') );
    my $atnm_qty     = newEntityName( :text('qty') );

    my $rel_type_suppliers = newTypeDictNQ( :map([
        [$atnm_farm,    newTypeInvoNQ(
            :kind('Scalar'), :spec($tynm_Text) )],
        [$atnm_country, newTypeInvoNQ(
            :kind('Scalar'), :spec($tynm_Text) )],
    ]) );

    my $rel_type_foods = newTypeDictNQ( :map([
        [$atnm_food,   newTypeInvoNQ(
            :kind('Scalar'), :spec($tynm_Text) )],
        [$atnm_colour, newTypeInvoNQ(
            :kind('Scalar'), :spec($tynm_Text) )],
    ]) );

    my $rel_type_shipments = newTypeDictNQ( :map([
        [$atnm_farm, newTypeInvoNQ( :kind('Scalar'), :spec($tynm_Text) )],
        [$atnm_food, newTypeInvoNQ( :kind('Scalar'), :spec($tynm_Text) )],
        [$atnm_qty,  newTypeInvoNQ( :kind('Scalar'), :spec($tynm_Int) )],
    ]) );

    # Load our example executable code into the virtual machine.

    my $var_suppliers = $dbms.new_var( :decl_type(newTypeInvoNQ(
        :kind('Relation'), :spec($rel_type_suppliers) )) );
    isa_ok( $var_suppliers, 'QDRDBMS::Interface::HostGateVar' );

    my $var_foods = $dbms.new_var( :decl_type(newTypeInvoNQ(
        :kind('Relation'), :spec($rel_type_foods) )) );
    isa_ok( $var_foods, 'QDRDBMS::Interface::HostGateVar' );

    my $var_shipments = $dbms.new_var( :decl_type(newTypeInvoNQ(
        :kind('Relation'), :spec($rel_type_shipments) )) );
    isa_ok( $var_shipments, 'QDRDBMS::Interface::HostGateVar' );

    # Declare our example literal data sets as QDRDBMS ASTs.

    my $rel_def_suppliers = newRelationSel(
        :heading($rel_type_suppliers),
        :body([
            newExprDict( :map([
                [$atnm_farm,    newLitText( :v('Hodgesons') )],
                [$atnm_country, newLitText( :v('Canada') )],
            ]) ),
            newExprDict( :map([
                [$atnm_farm,    newLitText( :v('Beckers') )],
                [$atnm_country, newLitText( :v('England') )],
            ]) ),
            newExprDict( :map([
                [$atnm_farm,    newLitText( :v('Wickets') )],
                [$atnm_country, newLitText( :v('Canada') )],
            ]) ),
        ]),
    );

    my $rel_def_foods = newRelationSel(
        :heading($rel_type_foods),
        :body([
            newExprDict( :map([
                [$atnm_food,   newLitText( :v('Bananas') )],
                [$atnm_colour, newLitText( :v('yellow') )],
            ]) ),
            newExprDict( :map([
                [$atnm_food,   newLitText( :v('Carrots') )],
                [$atnm_colour, newLitText( :v('orange') )],
            ]) ),
            newExprDict( :map([
                [$atnm_food,   newLitText( :v('Oranges') )],
                [$atnm_colour, newLitText( :v('orange') )],
            ]) ),
            newExprDict( :map([
                [$atnm_food,   newLitText( :v('Kiwis') )],
                [$atnm_colour, newLitText( :v('green') )],
            ]) ),
            newExprDict( :map([
                [$atnm_food,   newLitText( :v('Lemons') )],
                [$atnm_colour, newLitText( :v('yellow') )],
            ]) ),
        ]),
    );

    my $rel_def_shipments = newRelationSel(
        :heading($rel_type_shipments),
        :body([
            newExprDict( :map([
                [$atnm_farm, newLitText( :v('Hodgesons') )],
                [$atnm_food, newLitText( :v('Kiwis') )],
                [$atnm_qty,  newLitInt( :v(100) )],
            ]) ),
            newExprDict( :map([
                [$atnm_farm, newLitText( :v('Hodgesons') )],
                [$atnm_food, newLitText( :v('Lemons') )],
                [$atnm_qty,  newLitInt( :v(130) )],
            ]) ),
            newExprDict( :map([
                [$atnm_farm, newLitText( :v('Hodgesons') )],
                [$atnm_food, newLitText( :v('Oranges') )],
                [$atnm_qty,  newLitInt( :v(10) )],
            ]) ),
            newExprDict( :map([
                [$atnm_farm, newLitText( :v('Hodgesons') )],
                [$atnm_food, newLitText( :v('Carrots') )],
                [$atnm_qty,  newLitInt( :v(50) )],
            ]) ),
            newExprDict( :map([
                [$atnm_farm, newLitText( :v('Beckers') )],
                [$atnm_food, newLitText( :v('Carrots') )],
                [$atnm_qty,  newLitInt( :v(90) )],
            ]) ),
            newExprDict( :map([
                [$atnm_farm, newLitText( :v('Beckers') )],
                [$atnm_food, newLitText( :v('Bananas') )],
                [$atnm_qty,  newLitInt( :v(120) )],
            ]) ),
            newExprDict( :map([
                [$atnm_farm, newLitText( :v('Wickets') )],
                [$atnm_food, newLitText( :v('Lemons') )],
                [$atnm_qty,  newLitInt( :v(30) )],
            ]) ),
        ]),
    );

    # Load our example literal data sets into the virtual machine.

    $var_suppliers.store_ast( :val_ast($rel_def_suppliers) );
    pass( 'no death from loading example suppliers data into VM' );

    $var_foods.store_ast( :val_ast($rel_def_foods) );
    pass( 'no death from loading example foods data into VM' );

    $var_shipments.store_ast( :val_ast($rel_def_shipments) );
    pass( 'no death from loading example shipments data into VM' );

    return;
}

###########################################################################

} # module QDRDBMS::Validator

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

QDRDBMS::Validator -
A common comprehensive test suite to run against all Engines

=head1 VERSION

This document describes QDRDBMS::Validator version 0.0.0 for Perl 6.

=head1 SYNOPSIS

This can be the complete content of the main C<t/*.t> file for an example
QDRDBMS Engine distribution:

    use v6-alpha;

    # Load the test suite.
    use QDRDBMS::Validator;

    # Run the test suite.
    QDRDBMS::Validator::main(
            :engine_name('QDRDBMS::Engine::Example'),
            :dbms_config({}),
        );

    1;

The current release of QDRDBMS::Validator uses L<Test> internally, and
C<main()> will invoke it to output what the standard Perl test harness
expects.  I<It is expected that this will change in the future so that
Validator does not use Test internally, and rather will simply return test
results in a data structure that the main t/*.t then can disseminate and
pass the components to Test itself.>

=head1 DESCRIPTION

The QDRDBMS::Validator Perl 6 module is a common comprehensive test suite
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

I<This documentation is pending; this section may also be split into
several.>

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 6.x.y that is at least 6.0.0.

It also requires these Perl 6 classes that are in the current distribution:
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
