use v6-alpha;

use Muldis::DB;

###########################################################################
###########################################################################

module Muldis::DB::Validator-0.1.0 {

    use Test;

    use Muldis::DB::AST <newQuasiSetSel>;

###########################################################################

sub main (Str :$engine_name!, Any :$dbms_config!) {

    plan( 13 );

    say "#### Muldis::DB::Validator starting test of $engine_name ####";

    # Instantiate a Muldis::DB DBMS / virtual machine.
    my Muldis::DB::Interface::DBMS $dbms = Muldis::DB::new_dbms(
        :engine_name($engine_name), :dbms_config($dbms_config) );
    isa_ok( $dbms, 'Muldis::DB::Interface::DBMS' );

    _scenario_foods_suppliers_shipments( $dbms );

    say "#### Muldis::DB::Validator finished test of $engine_name ####";

    return;
}

###########################################################################

sub _scenario_foods_suppliers_shipments
        (Muldis::DB::Interface::DBMS $dbms!) {

    # Declare our example executable code as Muldis::DB ASTs.

    my $tynm_Text = ::Muldis::DB::AST::EntityName.new( :text<sys.type.Text> );
    my $tynm_UInt = ::Muldis::DB::AST::EntityName.new( :text<sys.type.UInt> );

    my $atnm_colour  = ::Muldis::DB::AST::EntityName.new( :text<colour> );
    my $atnm_country = ::Muldis::DB::AST::EntityName.new( :text<country> );
    my $atnm_farm    = ::Muldis::DB::AST::EntityName.new( :text<farm> );
    my $atnm_food    = ::Muldis::DB::AST::EntityName.new( :text<food> );
    my $atnm_qty     = ::Muldis::DB::AST::EntityName.new( :text<qty> );

    my $sca_type_Text = ::Muldis::DB::AST::TypeInvoNQ.new( :kind<Scalar>, :spec($tynm_Text) );
    my $sca_type_UInt = ::Muldis::DB::AST::TypeInvoNQ.new( :kind<Scalar>, :spec($tynm_UInt) );

    my $qrel_type_Relation = ::Muldis::DB::AST::TypeInvoAQ.new( :kind<Any>, :spec<Relation> );

    my $heading_suppliers = ::Muldis::DB::AST::TypeDictNQ.new( :map([
        [$atnm_farm,    $sca_type_Text],
        [$atnm_country, $sca_type_Text],
    ]) );
    my $heading_foods = ::Muldis::DB::AST::TypeDictNQ.new( :map([
        [$atnm_food,   $sca_type_Text],
        [$atnm_colour, $sca_type_Text],
    ]) );
    my $heading_shipments = ::Muldis::DB::AST::TypeDictNQ.new( :map([
        [$atnm_farm, $sca_type_Text],
        [$atnm_food, $sca_type_Text],
        [$atnm_qty,  $sca_type_UInt],
    ]) );
    my $heading_colours = ::Muldis::DB::AST::TypeDictNQ.new( :map([
        [$atnm_colour, $sca_type_Text],
    ]) );

    my $rel_type_suppliers = ::Muldis::DB::AST::TypeInvoNQ.new(
        :kind<Relation>, :spec($heading_suppliers) );
    my $rel_type_foods = ::Muldis::DB::AST::TypeInvoNQ.new(
        :kind<Relation>, :spec($heading_foods) );
    my $rel_type_shipments = ::Muldis::DB::AST::TypeInvoNQ.new(
        :kind<Relation>, :spec($heading_shipments) );

    my $pnm_matched_suppl = ::Muldis::DB::AST::EntityName.new( :text<matched_supp> );
    my $pnm_desi_colour   = ::Muldis::DB::AST::EntityName.new( :text<desi_colour> );
    my $pnm_src_suppl     = ::Muldis::DB::AST::EntityName.new( :text<src_suppl> );
    my $pnm_src_foods     = ::Muldis::DB::AST::EntityName.new( :text<src_foods> );
    my $pnm_src_shipm     = ::Muldis::DB::AST::EntityName.new( :text<src_shipm> );

    my $opnm_rel_asn = ::Muldis::DB::AST::EntityName.new( :text<sys.rtn.Relation.assign> );
    my $opnm_rel_jn  = ::Muldis::DB::AST::EntityName.new( :text<sys.rtn.Relation.join> );
    my $opnm_rel_sjn = ::Muldis::DB::AST::EntityName.new( :text<sys.rtn.Relation.semijoin> );

    my $anm_filter  = ::Muldis::DB::AST::EntityName.new( :text<filter> );
    my $anm_source  = ::Muldis::DB::AST::EntityName.new( :text<source> );
    my $anm_sources = ::Muldis::DB::AST::EntityName.new( :text<sources> );
    my $anm_target  = ::Muldis::DB::AST::EntityName.new( :text<target> );
    my $anm_v       = ::Muldis::DB::AST::EntityName.new( :text<v> );

    my $expr_3jn_ssp_sfd_scl = ::Muldis::DB::AST::FuncInvo.new(
        :func($opnm_rel_jn),
        :ro_args(::Muldis::DB::AST::ExprDict.new( :map([
            [$anm_sources, newQuasiSetSel(
                :heading($qrel_type_Relation),
                :body([
                    ::Muldis::DB::AST::VarInvo.new( :v($pnm_src_shipm) ),
                    ::Muldis::DB::AST::VarInvo.new( :v($pnm_src_foods) ),
                    ::Muldis::DB::AST::RelationSel.new(
                        :heading($heading_colours),
                        :body([
                            ::Muldis::DB::AST::ExprDict.new( :map([
                                [$atnm_colour, ::Muldis::DB::AST::VarInvo.new(
                                    :v($pnm_desi_colour) )],
                            ]) ),
                        ]),
                    ),
                ]),
            )],
        ]) )),
    );

    my $query_suppl_of_foods_of_clr = ::Muldis::DB::AST::HostGateRtn.new(
        :upd_params(::Muldis::DB::AST::TypeDictNQ.new( :map([
            [$pnm_matched_suppl, $rel_type_suppliers],
        ]) )),
        :ro_params(::Muldis::DB::AST::TypeDictNQ.new( :map([
            [$pnm_desi_colour, $sca_type_Text],
            [$pnm_src_suppl,   $rel_type_suppliers],
            [$pnm_src_foods,   $rel_type_foods],
            [$pnm_src_shipm,   $rel_type_shipments],
        ]) )),
        :vars(::Muldis::DB::AST::TypeDictNQ.new( :map([]) )),
        :stmts([
            ::Muldis::DB::AST::ProcInvo.new(
                :proc($opnm_rel_asn),
                :upd_args(::Muldis::DB::AST::ExprDict.new( :map([
                    [$anm_target, ::Muldis::DB::AST::VarInvo.new( :v($pnm_matched_suppl) )],
                ]) )),
                :ro_args(::Muldis::DB::AST::ExprDict.new( :map([
                    [$anm_v, ::Muldis::DB::AST::FuncInvo.new(
                        :func($opnm_rel_sjn),
                        :ro_args(::Muldis::DB::AST::ExprDict.new( :map([
                            [$anm_source, ::Muldis::DB::AST::VarInvo.new(
                                :v($pnm_src_suppl) )],
                            [$anm_filter, $expr_3jn_ssp_sfd_scl],
                        ]) )),
                    )],
                ]) )),
            ),
            ::Muldis::DB::AST::ProcReturn.new(),
        ]),
    );

    # Load our example executable code into the virtual machine.

    my $prep_rtn_suppl_of_foods_of_clr = $dbms.prepare(
        :rtn_ast($query_suppl_of_foods_of_clr) );
    isa_ok( $prep_rtn_suppl_of_foods_of_clr,
        'Muldis::DB::Interface::HostGateRtn' );

    my $src_suppliers = $dbms.new_var( :decl_type($rel_type_suppliers) );
    isa_ok( $src_suppliers, 'Muldis::DB::Interface::HostGateVar' );
    my $src_foods = $dbms.new_var( :decl_type($rel_type_foods) );
    isa_ok( $src_foods, 'Muldis::DB::Interface::HostGateVar' );
    my $src_shipments = $dbms.new_var( :decl_type($rel_type_shipments) );
    isa_ok( $src_shipments, 'Muldis::DB::Interface::HostGateVar' );

    my $desi_colour = $dbms.new_var( :decl_type($sca_type_Text) );
    isa_ok( $desi_colour, 'Muldis::DB::Interface::HostGateVar' );

    my $matched_suppl = $dbms.new_var( :decl_type($rel_type_suppliers) );
    isa_ok( $matched_suppl, 'Muldis::DB::Interface::HostGateVar' );

    $prep_rtn_suppl_of_foods_of_clr.bind_host_params(
        :upd_args([
            [$pnm_matched_suppl, $matched_suppl],
        ]),
        :ro_args([
            [$pnm_desi_colour, $desi_colour],
            [$pnm_src_suppl,   $src_suppliers],
            [$pnm_src_foods,   $src_foods],
            [$pnm_src_shipm,   $src_shipments],
        ]),
    );

    # Declare our example literal source data sets as Muldis::DB ASTs.

    my $rel_def_suppliers = ::Muldis::DB::AST::RelationSel.new(
        :heading($heading_suppliers),
        :body([
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_farm,    ::Muldis::DB::AST::TextLit.new( :v<Hodgesons> )],
                [$atnm_country, ::Muldis::DB::AST::TextLit.new( :v<Canada> )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_farm,    ::Muldis::DB::AST::TextLit.new( :v<Beckers> )],
                [$atnm_country, ::Muldis::DB::AST::TextLit.new( :v<England> )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_farm,    ::Muldis::DB::AST::TextLit.new( :v<Wickets> )],
                [$atnm_country, ::Muldis::DB::AST::TextLit.new( :v<Canada> )],
            ]) ),
        ]),
    );

    my $rel_def_foods = ::Muldis::DB::AST::RelationSel.new(
        :heading($heading_foods),
        :body([
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_food,   ::Muldis::DB::AST::TextLit.new( :v<Bananas> )],
                [$atnm_colour, ::Muldis::DB::AST::TextLit.new( :v<yellow> )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_food,   ::Muldis::DB::AST::TextLit.new( :v<Carrots> )],
                [$atnm_colour, ::Muldis::DB::AST::TextLit.new( :v<orange> )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_food,   ::Muldis::DB::AST::TextLit.new( :v<Oranges> )],
                [$atnm_colour, ::Muldis::DB::AST::TextLit.new( :v<orange> )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_food,   ::Muldis::DB::AST::TextLit.new( :v<Kiwis> )],
                [$atnm_colour, ::Muldis::DB::AST::TextLit.new( :v<green> )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_food,   ::Muldis::DB::AST::TextLit.new( :v<Lemons> )],
                [$atnm_colour, ::Muldis::DB::AST::TextLit.new( :v<yellow> )],
            ]) ),
        ]),
    );

    my $rel_def_shipments = ::Muldis::DB::AST::RelationSel.new(
        :heading($heading_shipments),
        :body([
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_farm, ::Muldis::DB::AST::TextLit.new( :v<Hodgesons> )],
                [$atnm_food, ::Muldis::DB::AST::TextLit.new( :v<Kiwis> )],
                [$atnm_qty,  ::Muldis::DB::AST::IntLit.new( :v(100) )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_farm, ::Muldis::DB::AST::TextLit.new( :v<Hodgesons> )],
                [$atnm_food, ::Muldis::DB::AST::TextLit.new( :v<Lemons> )],
                [$atnm_qty,  ::Muldis::DB::AST::IntLit.new( :v(130) )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_farm, ::Muldis::DB::AST::TextLit.new( :v<Hodgesons> )],
                [$atnm_food, ::Muldis::DB::AST::TextLit.new( :v<Oranges> )],
                [$atnm_qty,  ::Muldis::DB::AST::IntLit.new( :v(10) )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_farm, ::Muldis::DB::AST::TextLit.new( :v<Hodgesons> )],
                [$atnm_food, ::Muldis::DB::AST::TextLit.new( :v<Carrots> )],
                [$atnm_qty,  ::Muldis::DB::AST::IntLit.new( :v(50) )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_farm, ::Muldis::DB::AST::TextLit.new( :v<Beckers> )],
                [$atnm_food, ::Muldis::DB::AST::TextLit.new( :v<Carrots> )],
                [$atnm_qty,  ::Muldis::DB::AST::IntLit.new( :v(90) )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_farm, ::Muldis::DB::AST::TextLit.new( :v<Beckers> )],
                [$atnm_food, ::Muldis::DB::AST::TextLit.new( :v<Bananas> )],
                [$atnm_qty,  ::Muldis::DB::AST::IntLit.new( :v(120) )],
            ]) ),
            ::Muldis::DB::AST::ExprDict.new( :map([
                [$atnm_farm, ::Muldis::DB::AST::TextLit.new( :v<Wickets> )],
                [$atnm_food, ::Muldis::DB::AST::TextLit.new( :v<Lemons> )],
                [$atnm_qty,  ::Muldis::DB::AST::IntLit.new( :v(30) )],
            ]) ),
        ]),
    );

    # Load our example literal source data sets into the virtual machine.

    $src_suppliers.store_ast( :val_ast($rel_def_suppliers) );
    pass( 'no death from loading example suppliers data into VM' );
    $src_foods.store_ast( :val_ast($rel_def_foods) );
    pass( 'no death from loading example foods data into VM' );
    $src_shipments.store_ast( :val_ast($rel_def_shipments) );
    pass( 'no death from loading example shipments data into VM' );

    # Execute a query against the virtual machine, to look at our sample
    # data and see what suppliers there are for foods coloured 'orange'.

    $desi_colour.store_ast( :val_ast(::Muldis::DB::AST::TextLit.new( :v<orange> )) );
    pass( 'no death from loading desired colour into VM' );

    $prep_rtn_suppl_of_foods_of_clr.execute();
    pass( 'no death from executing prepared search routine' );

    my $rel_def_matched_suppl = $matched_suppl.fetch_ast();
    pass( 'no death from fetching search results from VM' );

    # Finally, use the result somehow (not done here).
    # The result should be:
    #    Relation(
    #        { farm<'Hodgesons'>, country<'Canada'> },
    #        { farm<'Beckers'>,   country<'England'> },
    #    );

    say "# debug: orange food suppliers found:";
    say "# " ~ $rel_def_matched_suppl.as_perl();

    return;
}

###########################################################################

} # module Muldis::DB::Validator

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Muldis::DB::Validator -
A common comprehensive test suite to run against all Engines

=head1 VERSION

This document describes Muldis::DB::Validator version 0.1.0 for Perl 6.

=head1 SYNOPSIS

This can be the complete content of the main C<t/*.t> file for an example
Muldis::DB Engine distribution:

    use v6-alpha;

    # Load the test suite.
    use Muldis::DB::Validator;

    # Run the test suite.
    Muldis::DB::Validator::main(
            :engine_name('Muldis::DB::Engine::Example'),
            :dbms_config({}),
        );

    1;

The current release of Muldis::DB::Validator uses L<Test> internally, and
C<main()> will invoke it to output what the standard Perl test harness
expects.  I<It is expected that this will change in the future so that
Validator does not use Test internally, and rather will simply return test
results in a data structure that the main t/*.t then can disseminate and
pass the components to Test itself.>

=head1 DESCRIPTION

The Muldis::DB::Validator Perl 6 module is a common comprehensive test
suite to run against all Muldis::DB Engines.  You run it against a
Muldis::DB Engine module to ensure that the Engine and/or the database
behind it implements the parts of the Muldis::DB API that your application
needs, and that the API is implemented correctly.  Muldis::DB::Validator is
intended to guarantee a measure of quality assurance (QA) for Muldis::DB,
so your application can use the database access framework with confidence
of safety.

Alternately, if you are writing a Muldis::DB Engine module yourself,
Muldis::DB::Validator saves you the work of having to write your own test
suite for it.  You can also be assured that if your module passes
Muldis::DB::Validator's approval, then your module can be easily swapped in
for other Engine modules by your users, and that any changes you make
between releases haven't broken something important.

Muldis::DB::Validator would be used similarly to how Sun has an official
validation suite for Java Virtual Machines to make sure they implement the
official Java specification.

For reference and context, please see the FEATURE SUPPORT VALIDATION
documentation section in the core L<Muldis::DB> module.

Note that, as is the nature of test suites, Muldis::DB::Validator will be
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
L<Muldis::DB::AST-(0.1.0)|Muldis::DB::AST>, L<Muldis::DB-0.1.0|Muldis::DB>.

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
