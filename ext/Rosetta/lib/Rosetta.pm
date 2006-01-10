#!/usr/bin/pugs
use v6;

# External packages used by packages in this file, that don't export symbols:
use Locale::KeyedText-(1.73.0...);
use Rosetta::Model-(0.710.0...);

###########################################################################
###########################################################################

# Constant values used by packages in this file:
# (None Yet)

###########################################################################
###########################################################################

package Rosetta-0.710.0 {
    # Note: This given version applies to all of this file's packages.
} # package Rosetta

###########################################################################
###########################################################################

class Rosetta::Interface {

    # External packages used by the Rosetta::Interface class, that do export symbols:
    # (None Yet)

    # Attributes of every Rosetta::Interface object:
    # (None Yet)

###########################################################################



###########################################################################

} # class Rosetta::Interface

###########################################################################
###########################################################################

class Rosetta::Interface::fubar {

    # External packages used by the Rosetta::Interface::fubar class, that do export symbols:
    # (None Yet)

    # Attributes of every Rosetta::Interface::fubar object:
    # (None Yet)

###########################################################################



###########################################################################

} # class Rosetta::Interface::fubar

###########################################################################
###########################################################################

class Rosetta::Engine {

    # External packages used by the Rosetta::Engine class, that do export symbols:
    # (None Yet)

    # Attributes of every Rosetta::Engine object:
    # (None Yet)

###########################################################################



###########################################################################

} # class Rosetta::Engine

###########################################################################
###########################################################################

=pod

=head1 NAME

Rosetta -
Rigorous database portability

=head1 VERSION

This document describes Rosetta version 0.710.0.

It also describes the same-number versions of Rosetta::Interface
("Interface"), Rosetta::Interface::fubar ("fubar"), and Rosetta::Engine
("Engine").

I<Note that the "Rosetta" package serves only as the name-sake
representative for this whole file, which can be referenced as a unit by
documentation or 'use' statements or Perl archive indexes.  Aside from
'use' statements, you should never refer directly to "Rosetta" in your
code; instead refer to other above-named packages in this file.>

=head1 SYNOPSIS

    ### DURING INIT PHASE ###

    use Rosetta; # also loads Rosetta::Model and Locale::KeyedText

    # Define how to talk to our database and where it is.
    my %DB_CONFIG = (
        'engine_name' => 'Rosetta::Engine::Native',
        'depot_identity' => {
            'file_name' => 'My Data',
        },
    );

    # Create a Rosetta Interface root, whose attributes will hold all of
    # the other data structures used by the Rosetta framework, as used by
    # the current process/application.
    my Rosetta::Interface $rosetta_root .= new();

    # Load a Rosetta Engine which will implement database access requests.
    my Rosetta::Interface::Engine $engine
        = $rosetta_root.load_engine( %DB_CONFIG{'engine_name'} );

    # Initialize (closed) database connection handle we will work with;
    # this does not talk to the underlying dbms.
    my Rosetta::Interface::Connection $conn
        = $engine.new_depot_connection( %DB_CONFIG{'depot_identity'} );

    # Create a Rosetta::Model document in which we store some local
    # command definitions and fragments thereof.
    my Rosetta::Model::Document $srt_doc.= new();

    # Define some data types.
    my Rosetta::Model::Node $sdtd_person_id = $srt_doc.build_node_tree(
        [ 'scalar_data_type', { 'base_type' => 'NUM_INT',
            'num_precision' => 9 } ]
    );
    my Rosetta::Model::Node $sdtd_person_name = $srt_doc.build_node_tree(
        [ 'scalar_data_type', { 'base_type' => 'STR_CHAR',
            'max_chars' => 100, 'char_set' => 'UNICODE' } ]
    );
    my Rosetta::Model::Node $sdtd_person_sex = $srt_doc.build_node_tree(
        [ 'scalar_data_type', { 'base_type' => 'STR_CHAR',
                'max_chars' => 1, 'char_set' => 'UNICODE' }, [
            [ 'scalar_data_type_value', { 'value' => 'M' } ],
            [ 'scalar_data_type_value', { 'value' => 'F' } ],
        ] ]
    );
    my Rosetta::Model::Node $rdtd_person = $srt_doc.build_node_tree(
        [ 'row_data_type', undef, [
            [ 'row_data_type_field', { 'name' => 'id' }, [
                $sdtd_person_id,
            ] ],
            [ 'row_data_type_field', { 'name' => 'name' }, [
                $sdtd_person_name,
            ] ],
            [ 'row_data_type_field', { 'name' => 'sex' }, [
                $sdtd_person_sex,
            ] ],
        ] ]
    );

    # Define the 'person' table.
    my Rosetta::Model::Node $tbd_person = $srt_doc.build_node_tree(
        [ 'table', { 'name' => 'person' }, [
            [ 'interface_row', undef, [
                $rdtd_person,
            ] ],
            [ 'table_field_detail', { 'name' => 'id', 'mandatory' => 1 } ],
            [ 'table_field_detail', { 'name' => 'name',
                'mandatory' => 1 } ],
            [ 'table_index', { 'name' => 'primary' ,
                    'index_type' => 'UNIQUE' }, [
                [ 'table_index_field', { 'name' => 'person_id' } ],
            ] ],
        ] ]
    );

    # Define and compile a routine that will validate whether the 'person'
    # table exists (and is correct).
    my Rosetta::Model::Node $fnd_tb_person_exists = $srt_doc.build_node_tree(
        [ 'function', { 'name' => 'tb_person_exists' }, [
            [ 'routine_arg', { 'name' => 'result',
                    'arg_type' => 'RETURN' }, [
                [ 'scalar_data_type', { 'base_type' => 'BOOLEAN' } ],
            ] ],
            [ 'routine_body', undef, [
                [ 'assignment_stmt', { 'into' => 'result' }, [
                    [ 'expression', {
                            'vf_call_sfunc' => 'DEPOT_OBJECT_EXISTS' }, [
                        $tbd_person,
                    ] ],
                ] ],
            ] ],
        ] ]
    );
    my Rosetta::Interface::Routine $fnh_tb_person_exists
        = $conn.compile_routine( $fnd_tb_person_exists );

    # Define and compile a routine that will create the 'person' table.
    # Like: CREATE TABLE person (
    #           id INTEGER(9) NOT NULL,
    #           name VARCHAR(100) NOT NULL,
    #           sex ENUM('M','F'),
    #           PRIMARY KEY (id)
    #       );
    #       COMMIT;
    my Rosetta::Model::Node $prd_create_tb_person = $srt_doc.build_node_tree(
        [ 'procedure', { 'name' => 'create_tb_person' }, [
            [ 'routine_body', undef, [
                [ 'create_stmt', undef, [
                    $tbd_person,
                ] ],
                [ 'statement', { 'call_sproc' => 'COMMIT' } ],
            ] ],
        ] ]
    );
    my Rosetta::Interface::Routine $prh_create_tb_person
        = $conn.compile_routine( $prd_create_tb_person );

    # Define and compile a procedure that will insert a record into the
    # 'person' table, which is populated from its row argument.
    # Like: INSERT INTO person
    #       SET id = :new_person.id,
    #           name = :new_person.name,
    #           sex = :new_person.sex;
    #       COMMIT;
    my Rosetta::Model::Node $prd_add_person = $srt_doc.build_node_tree(
        [ 'procedure', { 'name' => 'add_person' }, [
            [ 'routine_arg', { 'name' => 'new_person',
                    'arg_type' => 'IN' }, [
                $rdtd_person,
            ] ],
            [ 'routine_body', undef, [
                [ 'insert_stmt', { 'into' => 'person' }, [
                    [ 'expression', { 'vf_routine_arg' => 'new_person' } ],
                ] ],
                [ 'statement', { 'call_sproc' => 'COMMIT' } ],
            ] ],
        ] ]
    );
    my Rosetta::Interface::Routine $prh_add_person
        = $conn.compile_routine( $prd_add_person );

    # Define and compile a function that will select a record from the
    # 'person' table, whose 'id' field matches the functions 'person_id'
    # argument, and returns that as a row.
    # Like: SELECT s.id AS id, s.name AS name, s.sex AS sex
    #       FROM person AS s
    #       WHERE s.id = :person_id;
    my Rosetta::Model::Node $fnd_get_person = $srt_doc.build_node_tree(
        [ 'function', { 'name' => 'get_person' }, [
            [ 'routine_arg', { 'name' => 'result',
                    'arg_type' => 'RETURN' }, [
                $rdtd_person,
            ] ],
            [ 'routine_arg', { 'name' => 'person_id',
                    'arg_type' => 'IN' }, [
                $sdtd_person_id,
            ] ],
            [ 'routine_body', undef, [
                [ 'select_stmt', { 'into' => 'result' }, [
                    [ 'query', undef, [
                        [ 'interface_row', undef, [
                            $rdtd_person,
                        ] ],
                        [ 'query_source', { 'name' => 's',
                                'match' => 'person' }
                            [ 'query_source_field', { 'name' => 'id' } ],
                        ] ],
                        [ 'query_clause', { 'type' => 'WHERE' }, [
                            [ 'expression', {
                                    'vf_call_sfunc' => 'EQ' }, [
                                [ 'expression', {
                                    'call_sroutine_arg' => 'LHS',
                                    'vf_source_field' => 'id' } ],
                                [ 'expression', {
                                    'call_sroutine_arg' => 'RHS',
                                    'vf_routine_arg' => 'person_id' } ],
                            ] ],
                        ] ],
                    ] ],
                ] ],
            ] ],
        ] ]
    );
    my Rosetta::Interface::Routine $fnh_get_person
        = $conn.compile_routine( $fnd_get_person );

    ### DURING WORK PHASE ###

    # Actually connect to the database / talk to the underlying dbms;
    # if database doesn't exist yet, try to create it.
    try {
        $conn.open();
    };
    if (my $e = $!) {
        if ($e.does(Locale::KeyedText::Message)
                and $e.get_msg_key eq 'DEPOT_NO_EXIST') {
            $conn.create_target_depot();
            $conn.open();
        }
        else {
            die $!;
        }
    }

    try {
        # Check that the 'person' table exists and create it if not.
        if (!$fnh_tb_person_exists.prepare_and_execute().get_payload()) {
            $prh_create_tb_person.prepare_and_execute();
        }

        # Prompt user for details of 3 people and add them to the database.
        $prh_add_person.prepare();
        for 1..3 {
            my Rosetta::Interface::Row $new_person .= new( 'payload' => {
                'id' => ask_user_for_id(),
                'name' => ask_user_for_name(),
                'sex' => ask_user_for_sex(),
            } );
            $prh_add_person.bind_arg( 'new_person', $new_person );
            try {
                $prh_add_person.execute();
            };
            if (my $e = $!) {
                show_error_likely_bad_input( $e );
            }
            else {
                show_add_success_message();
            }
        }

        # Prompt user for id of 3 people and fetch them from the database.
        $fnh_get_person.prepare();
        for 1..3 {
            my Rosetta::Interface::Scalar $person_id
                .= new( 'payload' => ask_user_for_id() );
            $fnh_get_person.bind_arg( 'person_id', $person_id );
            my Rosetta::Interface::Row $fetched_person = try {
                $fnh_get_person.execute();
            };
            if (my $e = $!) {
                show_error_likely_bad_input( $e );
            }
            else {
                show_fetched_name( $fetched_person.get_field('name') );
                show_fetched_sex( $fetched_person.get_field('sex') );
            }
        }
    };

    # Close the database connection (it can be reopened later).
    $conn.close();

=head1 DESCRIPTION

Rosetta is a powerful but elegant virtual machine that implements a
relational model of data, and provides a rigorous API by which applications
can interact with it.  This relational model is like the one that E. F.
Codd proposed in his 1970 publication titled "A Relational Model of Data
for Large Shared Data Banks", but with additions either created for Rosetta
or drawn from multiple sources, mainly the ANSI/ISO SQL:2003 standard, and
various third party relational database managers.

Rosetta is highly modular, this being one of its fundamental features, with
the core 'Rosetta' file defining a common API which multiple interchangable
back-ends implement (each of which is usually made by a third party); the
former is called the RNI (Rosetta Native Interface), and the latter are
called Rosetta Engines.  Usually, each Engine implements a persistent data
store for long term use, but some can be RAM-based for use as a short-term
cache.  L<Rosetta::Engine::Native> is the reference implementation of an
Engine and provides both kinds of storage; it is distributed separately
from 'Rosetta' since it isn't needed if you use an alternate Engine.

The RNI is defined collectively by both 'Rosetta' itself and
L<Rosetta::Model>; Rosetta::Model defines IRL (Intermediate Relational
Language), a language loosely resembling SQL, in which applications write
instructions for the virtual machine to execute, including to create the
data models, query them, and manipulate them; 'Rosetta' provides the
handles on the virtual machine by which the applications give it input and
take its output.

Given that the most Rosetta Engine implementations simply map Rosetta's
rigorously defined API onto a pre-existing relational database manager
(such as Genezzo, SQLite, PostgreSQL, MySQL, Firebird, Oracle, Sybase, SQL
Server, Informix, DB2, etc), Rosetta's most prominant feature is that it
provides a common API for access to those databases.  An application
written to it should easily port to alternative relational database engines
with minimal effort.

This might seem strange to somebody who has not tried to port between
databases before, especially given that the Perl DBI purports to provide
"Database Independence".  However, the level of DBI's provided independence
is I<Database Driver Independence>, and not I<Database Language
Independence>.  To further demonstrate the difference, it is useful to
compare the DBI and Rosetta.  See the file docs/Overview.pod in this
distribution for that comparison.

=head1 INTERFACE

The interface of Rosetta is entirely object-oriented; you use it by
creating objects from its member classes, usually invoking C<new()> on the
appropriate class name, and then invoking methods on those objects.  All of
their attributes are private, so you must use accessor methods.  Rosetta
does not declare any subroutines or export such.

The usual way that Rosetta indicates a failure is to throw an exception;
most often this is due to invalid input.  If an invoked routine simply
returns, you can assume that it has succeeded, even if the return value is
undefined.

=head2 The Rosetta::Interface Class

I<This documentation is pending.>

=head2 The Rosetta::Interface::fubar Class

I<This documentation is pending.>

=head2 The Rosetta::Engine Class

I<This documentation is pending.>

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 6.x.y that is at least 6.0.0.

It also requires these Perl 6 classes that are in the current distribution:
L<Rosetta::Model-(0.710.0...)|Rosetta::Model>.

It also requires these Perl 6 classes that are on CPAN:
L<Locale::KeyedText-(1.73.0...)|Locale::KeyedText> (for error messages).

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

These documentation files are included in the Rosetta distribution:
L<Rosetta::Language>, L<Rosetta::Migration>.

The Perl 6 module L<Rosetta::Validator> is bundled with Rosetta and can be
used to test Rosetta Engine classes.

These Perl 6 packages implement Rosetta Engine classes:
L<Rosetta::Engine::Native>, L<Rosetta::Engine::Generic>.

These Perl 6 packages are the initial main dependents of Rosetta:
L<Rosetta::Emulator::DBI>.

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHOR

Darren R. Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the Rosetta database portability library.

Rosetta is Copyright (c) 2002-2006, Darren R. Duncan.  All rights reserved.
Address comments, suggestions, and bug reports to C<perl@DarrenDuncan.net>,
or visit L<http://www.DarrenDuncan.net/> for more information.

Rosetta is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License (GPL) as published by the Free
Software Foundation (L<http://www.fsf.org/>); either version 2 of the
License, or (at your option) any later version.  You should have received a
copy of the GPL as part of the Rosetta distribution, in the file named
"GPL"; if not, write to the Free Software Foundation, Inc., 51 Franklin St,
Fifth Floor, Boston, MA  02110-1301, USA.

Linking Rosetta statically or dynamically with other files is making a
combined work based on Rosetta.  Thus, the terms and conditions of the GPL
cover the whole combination.  As a special exception, the copyright holders
of Rosetta give you permission to link Rosetta with independent files,
regardless of the license terms of these independent files, and to copy
and distribute the resulting combined work under terms of your choice,
provided that every copy of the combined work is accompanied by a complete
copy of the source code of Rosetta (the version of Rosetta used to produce
the combined work), being distributed under the terms of the GPL plus this
exception.  An independent file is a file which is not derived from or
based on Rosetta, and which is fully useable when not linked to Rosetta in
any form.

Any versions of Rosetta that you modify and distribute must carry prominent
notices stating that you changed the files and the date of any changes, in
addition to preserving this original copyright notice and other credits.
Rosetta is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.

While it is by no means required, the copyright holders of Rosetta would
appreciate being informed any time you create a modified version of Rosetta
that you are willing to distribute, because that is a practical way of
suggesting improvements to the standard version.

=head1 ACKNOWLEDGEMENTS

None yet.

=cut
