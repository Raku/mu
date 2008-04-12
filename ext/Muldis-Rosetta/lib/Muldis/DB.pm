use v6-alpha;

###########################################################################
###########################################################################

package Muldis::DB-0.6.2 {
    # Note that Perl code only exists at all in this file in order to help
    # the CPAN indexer handle the distribution properly.
} # package Muldis::DB

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Muldis::DB -
Full-featured truly relational DBMS in Perl

=head1 VERSION

This document is Muldis::DB version 0.6.2.

=head1 DESCRIPTION

The B<Muldis DB> DBMS framework is a powerful but elegant system, which
makes it easy to create and use relational databases in a very reliable,
portable, and efficient way.  This "DB" file provides a 10,000 mile view of
the Muldis DB framework as a whole, and the detail documentation for each
component is included with that component.  The distribution containing
this "DB" file is the I<Muldis DB core> distribution.

Loosely speaking, the Muldis DB framework at large is like the Perl DBI
framework at large, so if you know how to use the Perl DBI to work with
databases, it should be easy enough to apply that knowledge to using Muldis
DB to work with databases.  Like the Perl DBI, Muldis DB has separately
distributable core, implementation, and extension distributions.  Like an
implementation ("driver") of the Perl DBI, an implementation ("engine") of
Muldis DB works according to the command design pattern; its API is very
minimalistic and mainly serves to process arbitrary "commands" through a
single routine or three.  Unlike the Perl DBI, which takes commands in some
dialect of SQL, which changes based on the implementation in use, Muldis DB
takes commands in the B<Muldis D> language, which has just one dialect
shared by all implementations.  See the separate all-documentation
distribution L<Muldis::D> for the formal definition of the Muldis D
language which Muldis DB is based on.

The minimal core of the Muldis DB framework, the one component that
probably every program would use, is the L<Muldis::DB::Interface> file.  It
defines a small set of roles/classes that comprise a common API (that
processes I<Perl Hosted Data Muldis D> commands) for Muldis DB
implementations to do and which applications invoke, called the I<Muldis DB
Native Interface> (or I<MDBNI>).  For the most part, C<Interface> just
defines shims and it can only be used when they are subclassed by an
implementation.  In the Perl DBI framework analogy, C<Interface>
corresponds to the L<DBI> module itself.  An implementation is called a
I<Muldis DB Engine> or I<Engine>.

Thanks largely to the use of Muldis D as its command language, MDBNI is
rigorously defined, such that there should be no ambiguity when trying to
invoke or implement it, and so an application written to it should behave
identically no matter which conforming Engine is in use.

The maximal core of the Muldis DB framework, everything else of substance
in the same distribution as the minimal core (and this "DB" file),
comprises 2 additional components.  The first is
L<Muldis::DB::Engine::Example>, a self-contained and pure-Perl reference
implementation of Muldis DB.  The second is L<Muldis::DB::Validator>, a
common comprehensive test suite for Muldis DB implementations.  Together,
these components make it possible for the Muldis DB core distribution to be
completely testable on its own.  It is therefore also feasible for an
application to use the Muldis DB core in isolation from further framework
components, though doing so isn't recommended for production use since
C<Example> is kept simple on purpose and doesn't scale well.

Muldis DB, by way of using the Muldis D language, incorporates a complete
and uncompromising implementation of "I<The Third Manifesto>" (I<TTM>), a
formal proposal by Christopher J. Date and Hugh Darwen for a solid
foundation for data and database management systems (DBMSs); like Edgar F.
Codd's original papers, I<TTM> can be seen as an abstract blueprint for the
design of a DBMS and the language interface to such a DBMS.  Muldis D is a
high-level programming language which is computationally complete (and
industrial strength) and has fully integrated database functionality; it
satisfies I<TTM>'s definition of a "B<D>" language.  The main web site for
I<TTM> is L<http://www.thethirdmanifesto.com/>, and its authors have also
written several books and papers and taught classes on the subject over the
last 35+ years, along with Codd himself (some are listed in the separately
distributed L<Muldis::D::SeeAlso> documentation file).  Note that
the Muldis DB documentation will be focusing mainly on how Muldis DB itself
works, and will not spend much time in providing rationale; you can read
I<TTM> itself and various other external documentation for much of that.

I<This documentation is pending.>

While it is possible that one could write a self-contained application in
Muldis D and compile that into its own executable, in practice one would
normally just write some components of their application in Muldis D (as
either named modules or anonymous routines) and write the rest of the
application in their other language(s) of choice.  Assuming the main
application is written in Perl, it is the L<Muldis::DB::Interface> file
which provides the glue between your Perl code and your Muldis D code;
"Muldis::DB::Interface" implements a virtual machine that is embedded in
your Perl application and in which the Muldis D code runs (it is analogous
to the Perl interpreter itself, which provides a virtual machine in which
Perl code runs).

A Muldis::DB::Interface::Machine object represents a single active Muldis
DB virtual machine; it has a spartan DBI-inspired set of methods which you
use to compile/prepare and/or invoke/execute Muldis D statements and
routines within the virtual machine, input data to it, and output data from
it.

You can create more than one Machine object at a time, and they are
essentially all isolated from each other, even if more than one uses the
same Engine class to implement it; that is, multiple Machine objects will
not have references to each other at a level visible in the Muldis DB
Native Interface, if at all.  To account for situations where multiple
Machine objects want to use the same external resources, such as a
repository file on disk, it is expected that the Engines will employ
appropriate measures such as system-managed locks so that resource
corruption or application failure is prevented.  I<Also, Muldis DB should
be thread safe and/or savvy in the future, but for now it officially is not
and you should not share Muldis DB objects between multiple threads, nor
have objects in separate threads try to access the same external
resources.>

Muldis DB does not use any dialect of SQL in its native API (unlike many
other DBMS products) because SQL is more ambiguous and error-prone to use,
and it is less expressive.  While Muldis D is very different from SQL, it
is fully capable of modeling anything in the real world accurately, and it
can support a complete SQL emulation layer on top of it, so that your
legacy applications can be migrated to use the Muldis DB DBMS with little
trouble.  Likewise, emulation layers for any other programming language can
be supported, such as B<Tutorial D> or XQuery or FoxPro or dBase.

One distinctive feature of a Muldis DB DBMS (compared to a typical other
vendor's DBMS) is that data definition statements are structured
fundamentally as standard data manipulation statements but that the target
relation variables are system catalog relation variables rather than
user-defined relation variables.  In SQL terms, you create or alter tables
by adding or updating their "information schema" records, which in SQL are
read-only, not only by using special 'create' or 'alter' statements.

Each Muldis DB Engine has the complete freedom to implement the Muldis DB
DBMS and Muldis D however it likes; all Muldis DB cares about is that the
user interface and behaviour conform to its preconceptions.

For production use, there should be a wide variety of third party Engine
modules that become available over time.  One plan being favored is that
the new (under development) enterprise-strength and Perl implemented
database server named L<Genezzo> (see also L<http://www.genezzo.com/>) will
evolve to implement the Muldis DB DBMS natively, and be I<the> back-end
which is recommended above all others for production use.

Most of the other (near term) third party Engines will likely just map
Muldis DB's rigorously defined API onto a pre-existing quasi-relational
database manager (such as SQLite, PostgreSQL, MySQL, Firebird, Teradata,
Oracle, Sybase, SQL Server, Informix, DB2, OpenBase, FrontBase, etc). Given
this fact, Muldis DB's most prominent feature is that it provides a common
API for access to those databases, each of which takes a different SQL or
quasi-SQL dialect.  An application written to it should easily port to
alternative relational database engines with minimal effort.

This might seem strange to somebody who has not tried to port between
databases before, especially given that the Perl DBI purports to provide
"Database Independence".  However, the level of DBI's provided independence
is I<Database Driver Independence>, and not I<Database Language
Independence>.  To further demonstrate the difference, it is useful to
compare the DBI and Muldis DB.  I<Such documentation is currently absent.>

In the context of a Muldis DB implementation over the Perl DBI, if one were
to categorize Muldis DB among other Perl modules, it could reasonably be
called a database abstraction layer; but it should I<not> be called a Perl
object persistence layer, as that is a different paradigm.

=head1 FEATURE SUPPORT VALIDATION

The Muldis DB Native Interface declares accessors for a large number of
actual or possible database features, any of which your application can
invoke, and all of which each Muldis DB Engine would ideally implement or
interface to.

In reality, however, all Engines or underlying databases probably don't
support some features, and if your application tries to invoke any of the
same features that an Engine you are using doesn't support, then you will
have problems ranging from immediate crashes/exceptions to subtle data
corruption over time.

As an official quality assurance (QA) measure, Muldis DB provides a means
for each Engine to programmatically declare which features it does and does
not support, so that code using that Engine will know so in advance of
trying to use said features.  Feature support declarations are typically
coarse grained and lump closely similar things together, for simplicity;
they will be just as fine grained as necessary and no finer (this can be
changed over time).  See the C<features()> method, which is how you read
the declarations.

One benefit of this QA feature is that, after you have written your
application and it is working with one Engine/database, and you want to
move it to a different Engine/database, you can determine at a glance which
alternatives also support the features you are using.  Note that, generally
speaking, you would have to be using very proprietary features to begin
with in order for the majority of Muldis DB Engines/databases to not
support the application outright.

Another benefit of this QA feature is that there can be made a common
comprehensive test suite to run against all Engines in order to tell that
they are implementing the Muldis DB interface properly or not; said test
suite will be smart enough to only test each Engine's compliance for those
features that the Engine claims to support, and not fail it for non-working
features that it explicitly says it doesn't support.  This common test
suite will save each Engine maker from having to write their own module
tests.  It would be used similarly to how Sun has an official validation
suite for Java Virtual Machines to make sure they implement the official
Java specification.  Please see the L<Muldis::DB::Validator> module(s),
which implements this test suite.

=head1 SEE ALSO

The separate all-documentation distribution L<Muldis::D> is the
formal definition of the Muldis D language which Muldis DB is based on.

The Perl module L<Muldis::DB::Validator> is bundled with Muldis DB and can
be used to test Muldis DB Engines.

The Perl module L<Muldis::DB::Engine::Example> is bundled with Muldis DB
and implements a self-contained reference implementation of a Muldis DB
Engine.

Go to the L<Muldis::DB::SeeAlso> file for the majority of external
references.

=head1 AUTHOR

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENSE AND COPYRIGHT

This file is part of the Muldis DB framework.

Muldis DB is Copyright © 2002-2008, Darren Duncan.  All rights reserved.

Muldis DB is free software; you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License (LGPL) as published by
the Free Software Foundation (L<http://www.fsf.org/>); either version 3 of
the License, or (at your option) any later version.  You should have
received a copy of the LGPL as part of the Muldis DB distribution, in the
files named "LICENSE/LGPL" and "LICENSE/GPL" (the LGPLv3 is defined as the
terms of the GPLv3 plus extra permissions); if not, see
L<http://www.gnu.org/licenses/>.

If it is not feasible for you to employ Muldis DB subject to the terms of
the LGPL, then the copyright holder of Muldis DB can provide you a
customized proprietary license, often at no cost, so that it is still
possible for you to employ Muldis DB to meet your needs.

Any versions of Muldis DB that you modify and distribute must carry
prominent notices stating that you changed the files and the date of any
changes, in addition to preserving this original copyright notice and other
credits.  Muldis DB is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  However, for an additional fee, the
copyright holders of Muldis DB can sell you a warranty for it.

While it is by no means required, the copyright holder of Muldis DB would
appreciate being informed any time you create a modified version of Muldis
DB that you are willing to distribute, because that is a practical way of
suggesting improvements to the standard version.

=head1 TRADEMARK POLICY

MULDIS and MULDIS MULTIVERSE OF DISCOURSE are trademarks of Muldis Data
Systems Inc. (L<http://www.muldis.com/>), which is wholly owned by Darren
Duncan.  The trademarks apply to computer database software and related
services.  See L<http://www.muldis.com/trademark_policy.html> for the full
written details of Muldis Data Systems' trademark policy.

The word MULDIS is intended to be used as the distinguishing brand name for
all the products and services of Muldis Data Systems.  So we would greatly
appreciate it if in general you do not incorporate the word MULDIS into the
name or logo of your website, business, product or service, but rather use
your own distinct name (exceptions appear below).  It is, however, always
okay to use the word MULDIS only in descriptions of your website, business,
product or service to provide accurate information to the public about
yourself.

If you do incorporate the word MULDIS into your names anyway, either
because you have permission from us or you have some other good reason,
then:  You must make clear that you are not Muldis Data Systems and that
you do not represent Muldis Data Systems.  A simple or conspicuous
disclaimer on your home page and product or service documentation is an
excellent way of doing that.

Please respect the conventions of the Perl community by not using the
namespace C<Muldis::> at all for your own works, unless you have explicit
permission to do so from Muldis Data Systems; that namespace is mainly just
for our official works.  You can always use either the C<MuldisX::>
namespace for related unofficial works, or some other namespace that is
completely different.  Also as per conventions, its fine to use C<Muldis>
within a Perl package name where that word is nested under some other
project-specific namespace (for example, C<Foo::Storage::Muldis_DB> or
C<Bar::Interface::Muldis_DB>), and the package serves to interact with a
Muldis Data Systems work or service.

If you have made a language variant or extension based on the B<Muldis D>
language, then please follow the naming conventions described in the
VERSIONING (L<Muldis::D/VERSIONING>) documentation of the official
B<Muldis D> language spec.

If you would like to use (or have already used) the word MULDIS for any use
that ought to require permission, please contact Muldis Data Systems and
we'll discuss a way to make that happen.

=head1 ACKNOWLEDGEMENTS

None yet.

=head1 FORUMS

Several public email-based forums exist whose main topic is all
implementations of the L<Muldis D|Muldis::D> language, especially
the L<Muldis DB|Muldis::DB> project, which they are named for.  All of
these you can reach via L<http://mm.DarrenDuncan.net/mailman/listinfo>; go
there to manage your subscriptions to, or view the archives of, the
following:

=over

=item C<muldis-db-announce@mm.DarrenDuncan.net>

This low-volume list is mainly for official announcements from the Muldis
DB developers, though developers of Muldis DB extensions can also post
their announcements here.  This is not a discussion list.

=item C<muldis-db-users@mm.DarrenDuncan.net>

This list is for general discussion among people who are using Muldis DB,
which is not concerned with the implementation of Muldis DB itself.  This
is the best place to ask for basic help in getting Muldis DB installed on
your machine or to make it do what you want.  You could also submit feature
requests or report perceived bugs here, if you don't want to use CPAN's RT
system.

=item C<muldis-db-devel@mm.DarrenDuncan.net>

This list is for discussion among people who are designing or implementing
the Muldis DB core API (including Muldis D language design), or who are
implementing Muldis DB Engines, or who are writing core documentation,
tests, or examples.  It is not the place for non-implementers to get help
in using said.

=back

An official IRC channel for Muldis DB is also intended, but not yet
started.

Alternately, you can purchase more advanced commercial support for various
Muldis D implementations, particularly Muldis DB, from its author by way of
Muldis Data Systems; see (L<http://www.muldis.com/>) for details.

=cut
