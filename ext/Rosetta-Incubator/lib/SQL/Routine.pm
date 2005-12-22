#!/usr/bin/pugs
use v6;

# External packages used by packages in this file, that don't export symbols:
use Locale::KeyedText-(1.72.0...);

###########################################################################
###########################################################################

# Constant values used by packages in this file:
# (None Yet)

###########################################################################
###########################################################################

package SQL::Routine-0.710.0 {
    # Note: This given version applies to all of this file's packages.
} # package SQL::Routine

###########################################################################
###########################################################################

class SQL::Routine::Document {

    # External packages used by the SQL::Routine::Document class, that do export symbols:
    # (None Yet)

    # Attributes of every SQL::Routine::Document object:
    # (None Yet)

###########################################################################



###########################################################################

} # class SQL::Routine::Document

###########################################################################
###########################################################################

class SQL::Routine::Node {

    # External packages used by the SQL::Routine::Node class, that do export symbols:
    # (None Yet)

    # Attributes of every SQL::Routine::Node object:
    # (None Yet)

###########################################################################



###########################################################################

} # class SQL::Routine::Node

###########################################################################
###########################################################################

=pod

=head1 NAME

SQL::Routine -
Specify all database tasks with SQL routines

=head1 VERSION

This document describes SQL::Routine version 0.710.0.

It also describes the same-number versions of SQL::Routine::Document
("Document") and SQL::Routine::Node ("Node").

I<Note that the "SQL::Routine" package serves only as the name-sake
representative for this whole file, which can be referenced as a unit by
documentation or 'use' statements or Perl archive indexes.  Aside from
'use' statements, you should never refer directly to "SQL::Routine" in your
code; instead refer to other above-named packages in this file.>

=head1 SYNOPSIS

I<This documentation is pending.>

=head1 DESCRIPTION

SQL::Routine provides an effective language for defining relational data
models, both the means to create them and the means to interact with them.
The language loosely resembles the ANSI/ISO SQL:2003 standard in purpose
and structure, but its details are different.  This is partly so that it
can more elegantly support the specific relational model that E. F. Codd
proposed in his 1970 publication titled "A Relational Model of Data for
Large Shared Data Banks", but that SQL diverged from in some ways.
Regardless, it should be easy to translate database definitions and queries
between SQL and the language SQL::Routine provides.

Please see the pod-only file L<SQL::Routine::Language> ("Language"), which
is the human readable authoritative design document for SQL::Routine's
language; the file SQL::Routine itself is a machine readable language
specification that is derived from the human readable version, and in the
case of a conflict, Language takes precedence.

SQL::Routine is implemented as abstract syntax trees, and you use it by
creating, manipulating, and reading nodes in these trees.  Each tree node
is atomic, so you can just build the trees by copying scalar values from a
data dictionary; no stitching or parsing more complicated command strings
is necessary like with SQL.

L<Rosetta> (distributed separately) is a relational database access
solution that uses SQL::Routine objects as its native instruction set
rather than SQL strings.  But SQL::Routine can also be used independently
of Rosetta, such as when translating SQL from one dialect to another.

=head1 INTERFACE

The interface of SQL::Routine is entirely object-oriented; you use it by
creating objects from its member classes, usually invoking C<new()> on the
appropriate class name, and then invoking methods on those objects.  All of
their attributes are private, so you must use accessor methods.
SQL::Routine does not declare any subroutines or export such.

The usual way that SQL::Routine indicates a failure is to throw an
exception; most often this is due to invalid input.  If an invoked routine
simply returns, you can assume that it has succeeded, even if the return
value is undefined.

=head2 The SQL::Routine::Document Class

I<This documentation is pending.>

=head2 The SQL::Routine::Node Class

I<This documentation is pending.>

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 6.x.y that is at least 6.0.0.

It also requires these Perl 6 classes that are on CPAN:
L<Locale::KeyedText-(1.72.0...)|Locale::KeyedText> (for error messages).

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

These documentation files are included in the SQL::Routine distribution:
L<SQL::Routine::Language>.

These Perl 6 packages are the initial main dependents of SQL::Routine:
L<Rosetta>, L<SQL::Routine::SQLBuilder>, L<SQL::Routine::SQLParser>.

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHOR

Darren R. Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the SQL::Routine database portability library.

SQL::Routine is Copyright (c) 2002-2005, Darren R. Duncan.  All rights
reserved. Address comments, suggestions, and bug reports to
C<perl@DarrenDuncan.net>, or visit L<http://www.DarrenDuncan.net/> for more
information.

SQL::Routine is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License (GPL) as published by the
Free Software Foundation (L<http://www.fsf.org/>); either version 2 of the
License, or (at your option) any later version.  You should have received a
copy of the GPL as part of the SQL::Routine distribution, in the file named
"GPL"; if not, write to the Free Software Foundation, Inc., 51 Franklin St,
Fifth Floor, Boston, MA 02110-1301, USA.

Linking SQL::Routine statically or dynamically with other files is making
a combined work based on SQL::Routine.  Thus, the terms and conditions of
the GPL cover the whole combination.  As a special exception, the copyright
holders of SQL::Routine give you permission to link SQL::Routine with
independent files, regardless of the license terms of these independent
files, and to copy and distribute the resulting combined work under terms
of your choice, provided that every copy of the combined work is
accompanied by a complete copy of the source code of SQL::Routine (the
version of SQL::Routine used to produce the combined work), being
distributed under the terms of the GPL plus this exception.  An independent
file is a file which is not derived from or based on SQL::Routine, and
which is fully useable when not linked to SQL::Routine in any form.

Any versions of SQL::Routine that you modify and distribute must carry
prominent notices stating that you changed the files and the date of any
changes, in addition to preserving this original copyright notice and other
credits. SQL::Routine is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

While it is by no means required, the copyright holders of SQL::Routine
would appreciate being informed any time you create a modified version of
SQL::Routine that you are willing to distribute, because that is a
practical way of suggesting improvements to the standard version.

=head1 ACKNOWLEDGEMENTS

None yet.

=cut
