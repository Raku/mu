use v6-alpha;

use Muldis::DB::AST;

###########################################################################
###########################################################################

module Muldis::DB-0.0.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub new_dbms of Muldis::DB::Interface::DBMS
        (Str :$engine_name!, Any :$dbms_config!) {
    return ::Muldis::DB::Interface::DBMS.new(
        :engine_name($engine_name), :dbms_config($dbms_config) );
}

###########################################################################

} # module Muldis::DB

###########################################################################
###########################################################################

class Muldis::DB::Interface::DBMS {
    has Any $!dbms_eng;

    trusts Muldis::DB::Interface::HostGateVar;
    trusts Muldis::DB::Interface::HostGateRtn;

###########################################################################

submethod BUILD (Str :$engine_name!, Any :$dbms_config!) {

    die q{new(): Bad :$engine_name arg; it is not an object of a}
            ~ q{ Str-doing class, or it is the empty string.}
        if !$engine_name.defined or !$engine_name.does(Str)
            or $engine_name eq q{};

    # A class may be loaded due to it being embedded in a non-excl file.
    if (!::($engine_name).does(Class)) {
        # Note: We have to invoke this 'require' in an eval string
        # because we need the bareword semantics, where 'require'
        # will munge the module name into file system paths.
        eval "require $engine_name;";
        if (my $err = $!) {
            die q{new(): Could not load Muldis::DB Engine class}
                ~ qq{ '$engine_name': $err};
        }
#        die qq{new(): Could not load Muldis::DB Engine class}
#                ~ qq{ '$engine_name': while that file did compile without}
#                ~ q{ errors, it did not declare the same-named class.}
#            if !::($engine_name).does(Class);
    }
    die qq{new(): The Muldis::DB root Engine class '$engine_name' is}
            ~ q{ not a Muldis::DB::Engine::Role-doing class.}
        if !::($engine_name).does(::Muldis::DB::Engine::Role);
    my $dbms_eng = undef;
    try {
        $dbms_eng = ::($engine_name).new_dbms(
            :dbms_config($dbms_config) );
    };
    if (my $err = $!) {
        die qq{new(): The Muldis::DB Engine class '$engine_name' threw}
            ~ qq{ an exception during its new_dbms() execution: $err};
    }
    die q{new(): The new_dbms() constructor submeth of the Muldis::DB}
            ~ qq{ root Engine class '$engine_name' did not return an}
            ~ q{ object of a Muldis::DB::Engine::Role::DBMS-doing class.}
        if !$dbms_eng.defined
            or !$dbms_eng.does(::Muldis::DB::Engine::Role::DBMS);

    $!dbms_eng = $dbms_eng;

    return;
}

###########################################################################

method new_var of Muldis::DB::Interface::HostGateVar
        (Muldis::DB::AST::TypeInvo :$decl_type!) {
    return ::Muldis::DB::Interface::HostGateVar.new(
        :dbms(self), :decl_type($decl_type) );
}

method prepare of Muldis::DB::Interface::HostGateRtn
        (Muldis::DB::AST::HostGateRtn :$rtn_ast!) {
    return ::Muldis::DB::Interface::HostGateRtn.new(
        :dbms(self), :rtn_ast($rtn_ast) );
}

###########################################################################

} # class Muldis::DB::Interface::DBMS

###########################################################################
###########################################################################

class Muldis::DB::Interface::HostGateVar {
    has Muldis::DB::Interface::DBMS $!dbms;
    has Any                         $!var_eng;

    trusts Muldis::DB::Interface::HostGateRtn;

###########################################################################

submethod BUILD (Muldis::DB::Interface::DBMS :$dbms!,
        Muldis::DB::AST::TypeInvo :$decl_type!) {

    die q{new(): Bad :$dbms arg; it is not an object of a}
            ~ q{ Muldis::DB::Interface::DBMS-doing class.}
        if !$dbms.defined or !$dbms.does(Muldis::DB::Interface::DBMS);
    my $dbms_eng = $dbms!dbms_eng;
    my $dbms_eng_class = $dbms_eng.WHAT;

    die q{new(): Bad :$decl_type arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::TypeInvo-doing class.}
        if !$decl_type.defined
            or !$decl_type.does(Muldis::DB::AST::TypeInvo);

    my $var_eng = undef;
    try {
        $var_eng = $dbms_eng.new_var( :decl_type($decl_type) );
    };
    if (my $err = $!) {
        die qq{new(): The Muldis::DB DBMS Eng class '$dbms_eng_class'}
            ~ q{ threw an exception during its new_var()}
            ~ qq{ execution: $err};
    }
    die q{new(): The new_var() method of the Muldis::DB}
            ~ qq{ DBMS class '$dbms_eng_class' did not return an object}
            ~ q{ of a Muldis::DB::Engine::Role::HostGateVar-doing class.}
        if !$var_eng.defined
            or !$var_eng.does(::Muldis::DB::Engine::Role::HostGateVar);

    $!dbms    = $dbms;
    $!var_eng = $var_eng;

    return;
}

###########################################################################

method fetch_ast of Muldis::DB::AST::Node () {

    my $val_ast = undef;
    try {
        $val_ast = $!var_eng.fetch_ast();
    };
    if (my $err = $!) {
        my $var_eng_class = $!var_eng.WHAT;
        die q{fetch_ast(): The Muldis::DB HostGateVar Engine}
            ~ qq{ class '$var_eng_class' threw an exception during its}
            ~ qq{ fetch_ast() execution: $err};
    }

    return $val_ast;
}

###########################################################################

method store_ast (Muldis::DB::AST::Node :$val_ast!) {

    die q{store_ast(): Bad :$val_ast arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::Node-doing class.}
        if !$val_ast.defined or !$val_ast.does(Muldis::DB::AST::Node);

    try {
        $!var_eng.store_ast( :val_ast($val_ast) );
    };
    if (my $err = $!) {
        my $var_eng_class = $!var_eng.WHAT;
        die q{store_ast(): The Muldis::DB HostGateVar Engine}
            ~ qq{ class '$var_eng_class' threw an exception during its}
            ~ qq{ store_ast() execution: $err};
    }

    return;
}

###########################################################################

} # class Muldis::DB::Interface::HostGateVar

###########################################################################
###########################################################################

class Muldis::DB::Interface::HostGateRtn {
    has Muldis::DB::Interface::DBMS  $!dbms;
    has Muldis::DB::AST::HostGateRtn $!rtn_ast;
    has Any                          $!rtn_eng;

###########################################################################

submethod BUILD (Muldis::DB::Interface::DBMS :$dbms!,
        Muldis::DB::AST::HostGateRtn :$rtn_ast!) {

    die q{new(): Bad :$dbms arg; it is not an object of a}
            ~ q{ Muldis::DB::Interface::DBMS-doing class.}
        if !$dbms.defined or !$dbms.does(Muldis::DB::Interface::DBMS);
    my $dbms_eng = $dbms!dbms_eng;
    my $dbms_eng_class = $dbms_eng.WHAT;

    die q{new(): Bad :$rtn_ast arg; it is not an object of a}
            ~ q{ Muldis::DB::AST::HostGateRtn-doing class.}
        if !$rtn_ast.defined
            or !$rtn_ast.does(Muldis::DB::AST::HostGateRtn);

    my $rtn_eng = undef;
    try {
        $rtn_eng = $dbms_eng.prepare( :rtn_ast($rtn_ast) );
    };
    if (my $err = $!) {
        die qq{new(): The Muldis::DB DBMS Eng class '$dbms_eng_class'}
            ~ qq{ threw an exception during its prepare() execution: $err};
    }
    die q{new(): The prepare() method of the Muldis::DB}
            ~ qq{ DBMS class '$dbms_eng_class' did not return an object}
            ~ q{ of a Muldis::DB::Engine::Role::HostGateRtn-doing class.}
        if !$rtn_eng.defined
            or !$rtn_eng.does(::Muldis::DB::Engine::Role::HostGateRtn);

    $!dbms    = $dbms;
    $!rtn_ast = $rtn_ast;
    $!rtn_eng = $rtn_eng;

    return;
}

###########################################################################

method bind_host_params (Array :$upd_args!, Array :$ro_args!) {

    my Hash $exp_upd_params_map_hoa = $!rtn_ast!upd_params!map_hoa;
    my Hash $exp_ro_params_map_hoa = $!rtn_ast!ro_params!map_hoa;

    die q{bind_host_params(): Bad :$upd_args arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$upd_args.defined or !$upd_args.does(Array);
    my Hash $seen_upd_param_names = {};
    my Array $upd_arg_engs = [];
    for $upd_args -> $elem {
        die q{bind_host_params(): Bad :$upd_args arg; it is not an object}
                ~ q{ of a Array-doing class, or it doesn't have 2 elems.}
            if !$elem.defined or !$elem.does(Array) or $elem.elems != 2;
        my ($param_name, $var_intf) = $elem.values;
        die q{bind_host_params(): Bad :$upd_args arg elem; its first}
                ~ q{ element is not an object of a}
                ~ q{ Muldis::DB::AST::EntityName-doing class.}
            if !$param_name.defined
                or !$param_name.does(Muldis::DB::AST::EntityName);
        my Str $param_name_text = $param_name.text();
        die q{bind_host_params(): Bad :$upd_args arg elem; its first}
                ~ q{ element does not match the name of a}
                ~ q{ subject-to-update routine param.}
            if !$exp_upd_params_map_hoa.exists($param_name_text);
        die q{bind_host_params(): Bad :$upd_args arg elem; its first elem}
                ~ q{ is not distinct between the arg elems.}
            if $seen_upd_param_names.exists($param_name_text);
        $seen_upd_param_names{$param_name_text} = 1;
        die q{bind_host_params(): Bad :$upd_args arg elem; its second}
                ~ q{ element is not an object of a}
                ~ q{ Muldis::DB::Interface::HostGateVar-doing class.}
            if !$var_intf.defined
                or !$var_intf.does(Muldis::DB::Interface::HostGateVar);
        $upd_arg_engs.push( [$param_name, $var_intf!var_eng] );
    }

    die q{bind_host_params(): Bad :$ro_args arg; it is not an object of a}
            ~ q{ Array-doing class.}
        if !$ro_args.defined or !$ro_args.does(Array);
    my Hash $seen_ro_param_names = {};
    my Array $ro_arg_engs = [];
    for $ro_args -> $elem {
        die q{bind_host_params(): Bad :$ro_args arg; it is not an object}
                ~ q{ of a Array-doing class, or it doesn't have 2 elems.}
            if !$elem.defined or !$elem.does(Array) or $elem.elems != 2;
        my ($param_name, $var_intf) = $elem.values;
        die q{bind_host_params(): Bad :$ro_args arg elem; its first}
                ~ q{ element is not an object of a}
                ~ q{ Muldis::DB::AST::EntityName-doing class.}
            if !$param_name.defined
                or !$param_name.does(Muldis::DB::AST::EntityName);
        my Str $param_name_text = $param_name.text();
        die q{bind_host_params(): Bad :$ro_args arg elem; its first}
                ~ q{ element does not match the name of a}
                ~ q{ read-only routine param.}
            if !$exp_ro_params_map_hoa.exists($param_name_text);
        die q{bind_host_params(): Bad :$ro_args arg elem; its first elem}
                ~ q{ is not distinct between the arg elems.}
            if $seen_ro_param_names.exists($param_name_text);
        $seen_ro_param_names{$param_name_text} = 1;
        die q{bind_host_params(): Bad :$ro_args arg elem; its second}
                ~ q{ element is not an object of a}
                ~ q{ Muldis::DB::Interface::HostGateVar-doing class.}
            if !$var_intf.defined
                or !$var_intf.does(Muldis::DB::Interface::HostGateVar);
        $ro_arg_engs.push( [$param_name, $var_intf!var_eng] );
    }

    try {
        $!rtn_eng.bind_host_params(
            :upd_args($upd_arg_engs), :ro_args($ro_arg_engs) );
    };
    if (my $err = $!) {
        my $rtn_eng_class = $!rtn_eng.WHAT;
        die q{bind_host_params(): The Muldis::DB HostGateRtn Engine}
            ~ qq{ class '$rtn_eng_class' threw an exception during its}
            ~ qq{ bind_host_params() execution: $err};
    }

    return;
}

###########################################################################

method execute () {
    try {
        $!rtn_eng.execute();
    };
    if (my $err = $!) {
        my $rtn_eng_class = $!rtn_eng.WHAT;
        die q{execute(): The Muldis::DB HostGateRtn Engine}
            ~ qq{ class '$rtn_eng_class' threw an exception during its}
            ~ qq{ execute() execution: $err};
    }
    return;
}

###########################################################################

} # class Muldis::DB::Interface::HostGateRtn

###########################################################################
###########################################################################

role Muldis::DB::Engine::Role {

    submethod new_dbms {
        die q{not implemented by subclass } ~ $?CLASS;
    }

} # role Muldis::DB::Engine::Role

###########################################################################
###########################################################################

role Muldis::DB::Engine::Role::DBMS {

    method new_var {
        die q{not implemented by subclass } ~ self.WHAT;
    }

    method prepare {
        die q{not implemented by subclass } ~ self.WHAT;
    }

} # role Muldis::DB::Engine::Role::DBMS

###########################################################################
###########################################################################

role Muldis::DB::Engine::Role::HostGateVar {

    method fetch_ast {
        die q{not implemented by subclass } ~ self.WHAT;
    }

    method store_ast {
        die q{not implemented by subclass } ~ self.WHAT;
    }

} # role Muldis::DB::Engine::Role::HostGateVar

###########################################################################
###########################################################################

role Muldis::DB::Engine::Role::HostGateRtn {

    method bind_host_params {
        die q{not implemented by subclass } ~ self.WHAT;
    }

    method execute {
        die q{not implemented by subclass } ~ self.WHAT;
    }

} # role Muldis::DB::Engine::Role::HostGateRtn

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Muldis::DB -
Full-featured truly relational DBMS in Perl

=head1 VERSION

This document describes Muldis::DB version 0.0.0 for Perl 6.

It also describes the same-number versions for Perl 6 of
Muldis::DB::Interface::DBMS ("DBMS"), Muldis::DB::Interface::HostGateVar
("HostGateVar"), and Muldis::DB::Interface::HostGateRtn ("HostGateRtn").

It also describes the same-number versions for Perl 6 of
Muldis::DB::Engine::Role, Muldis::DB::Engine::Role::DBMS,
Muldis::DB::Engine::Role::HostGateVar, and
Muldis::DB::Engine::Role::HostGateRtn.

=head1 SYNOPSIS

    use Muldis::DB;

    # Instantiate a Muldis::DB DBMS / virtual machine.
    my $dbms = Muldis::DB::new_dbms(
            :engine_name('Muldis::DB::Engine::Example'),
            :dbms_config({}),
        );

    # TODO: Create or connect to a repository and work with it.

I<This documentation is pending.>

=head1 DESCRIPTION

The "Muldis::DB" DBMS framework is a powerful but elegant system, which
makes it easy to create and use relational databases in a very reliable,
portable, and efficient way.  This "Muldis::DB" file is the core of the
Muldis::DB framework and defines a truly relational common programmatic
interface (API), called the Muldis::DB Native Interface, which applications
invoke and which multiple interchangeable "Engine" back-ends (usually
provided by third parties) implement.  This interface is rigorously
defined, such that there should be no ambiguity when trying to invoke or
implement it, and so an application written to it should behave identically
no matter which conforming "Engine" is in use.

Muldis::DB incorporates a complete and uncompromising implementation of
"The Third Manifesto" (TTM), a formal proposal by Christopher J. Date and
Hugh Darwen for a solid foundation for data and database management systems
(DBMSs); like Edgar F. Codd's original papers, TTM can be seen as an
abstract blueprint for the design of a DBMS and the language interface to
such a DBMS.  The main web site for TTM is
L<http://www.thethirdmanifesto.com/>, and its authors have also written
several books and papers and taught classes on the subject over the last
35+ years, along with Codd himself (some are listed in the
L<Muldis::DB::SeeAlso> documentation file).  Note that the Muldis::DB
documentation will be focusing mainly on how Muldis::DB itself works, and
will not spend much time in providing rationale; you can read TTM itself
and various other external documentation for much of that.

The Muldis::DB Native Interface is defined mainly in terms of a new
high-level programming language named "Muldis D", which is computationally
complete (and industrial strength) and has fully integrated database
functionality; this language, which satisfies TTM's definition of a "D"
language, is described fully in the L<Muldis::DB::Language> documentation
file that comes with this "Muldis::DB" distribution.

While it is possible that one could write a self-contained application in
Muldis D and compile that into its own executable, in practice one would
normally just write some components of their application in Muldis D (as
either named modules or anonymous routines) and write the rest of the
application in their other language(s) of choice.  Assuming the main
application is written in Perl, it is this "Muldis::DB" file which provides
the glue between your Perl code and your Muldis D code; "Muldis::DB"
implements a virtual machine that is embedded in your Perl application and
in which the Muldis D code runs (it is analogous to the Perl interpreter
itself, which provides a virtual machine in which Perl code runs).

The classes and methods of this "Muldis::DB" file, together with those of
L<Muldis::DB::AST>, define the balance of the Muldis::DB Native Interface.
A Muldis::DB::Interface::DBMS object represents a single active Muldis::DB
virtual machine; it has a spartan DBI-inspired set of methods which you use
to compile/prepare and/or invoke/execute Muldis D statements and routines
within the virtual machine, input data to it, and output data from it.

You can create more than one DBMS object at a time, and they are
essentially all isolated from each other, even if more than one uses the
same Engine class to implement it; that is, multiple DBMS objects will not
have references to each other at a level visible in the Muldis::DB Native
Interface, if at all.  To account for situations where multiple DBMS
objects want to use the same external resources, such as a repository file
on disk, it is expected that the Engines will employ appropriate measures
such as system-managed locks so that resource corruption or application
failure is prevented.  I<Also, Muldis::DB should be thread safe and/or
savvy in the future, but for now it officially is not and you should not
share Muldis::DB objects between multiple threads, nor have objects in
separate threads try to access the same external resources.>

Muldis::DB does not use any dialect of SQL in its native API (unlike many
other DBMS products) because SQL is more ambiguous and error-prone to use,
and it is less expressive.  While Muldis D is very different from SQL, it
is fully capable of modeling anything in the real world accurately, and it
can support a complete SQL emulation layer on top of it, so that your
legacy applications can be migrated to use the Muldis::DB DBMS with little
trouble.  Likewise, emulation layers for any other programming language can
be supported, such as Tutorial D or XQuery or FoxPro or dBase.

One distinctive feature of a Muldis::DB DBMS (compared to a typical other
vendor's DBMS) is that data definition statements are structured as
standard data manipulation statements but that the target relation
variables are system catalog relation variables rather than user-defined
relation variables.  In SQL terms, you create or alter tables by adding or
updating their "information schema" records, which in SQL are read-only,
not by using special 'create' or 'alter' statements.

Each Muldis::DB Engine has the complete freedom to implement the Muldis::DB
DBMS and Muldis D however it likes; all Muldis::DB cares about is that the
user interface and behaviour conform to its preconceptions.

L<Muldis::DB::Engine::Example> is the self-contained and pure-Perl
reference implementation of an Engine and is included in the "Muldis::DB"
core distribution to allow the core to be completely testable on its own.
It is coded intentionally in a simple fashion so that it is easy to
maintain and and easy for developers to study.  As a result, while it
performs correctly and reliably, it also performs quite slowly; you should
only use Example for testing, development, and study; you should not use it
in production.

For production use, there should be a wide variety of third party Engine
modules that become available over time.  One plan being favored is that
the new (under development) enterprise-strength and Perl implemented
database server named L<Genezzo> (see also L<http://www.genezzo.com/>) will
evolve to implement the Muldis::DB DBMS natively, and be I<the> back-end
which is recommended above all others for production use.

Most of the other (near term) third party Engines will likely just map
Muldis::DB's rigorously defined API onto a pre-existing quasi-relational
database manager (such as SQLite, PostgreSQL, MySQL, Firebird, Teradata,
Oracle, Sybase, SQL Server, Informix, DB2, OpenBase, FrontBase, etc). Given
this fact, Muldis::DB's most prominent feature is that it provides a common
API for access to those databases, each of which takes a different SQL or
quasi-SQL dialect.  An application written to it should easily port to
alternative relational database engines with minimal effort.

This might seem strange to somebody who has not tried to port between
databases before, especially given that the Perl DBI purports to provide
"Database Independence".  However, the level of DBI's provided independence
is I<Database Driver Independence>, and not I<Database Language
Independence>.  To further demonstrate the difference, it is useful to
compare the DBI and Muldis::DB.  I<Such documentation is currently absent.>

=head1 FEATURE SUPPORT VALIDATION

The Muldis::DB Native Interface declares accessors for a large number of
actual or possible database features, any of which your application can
invoke, and all of which each Muldis::DB Engine would ideally implement or
interface to.

In reality, however, all Engines or underlying databases probably don't
support some features, and if your application tries to invoke any of the
same features that an Engine you are using doesn't support, then you will
have problems ranging from immediate crashes/exceptions to subtle data
corruption over time.

As an official quality assurance (QA) measure, Muldis::DB provides a means
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
with in order for the majority of Muldis::DB Engines/databases to not
support the application outright.

Another benefit of this QA feature is that there can be made a common
comprehensive test suite to run against all Engines in order to tell that
they are implementing the Muldis::DB interface properly or not; said test
suite will be smart enough to only test each Engine's compliance for those
features that the Engine claims to support, and not fail it for non-working
features that it explicitly says it doesn't support.  This common test
suite will save each Engine maker from having to write their own module
tests.  It would be used similarly to how Sun has an official validation
suite for Java Virtual Machines to make sure they implement the official
Java specification.  Please see the L<Muldis::DB::Validator> module(s),
which implements this test suite.

=head1 INTERFACE

The interface of Muldis::DB is fundamentally object-oriented; you use it by
creating objects from its member classes, usually invoking C<new()> on the
appropriate class name, and then invoking methods on those objects.  All of
their attributes are private, so you must use accessor methods.

Muldis::DB also provides the not-exportable wrapper subroutine
C<Muldis::DB::new_dbms> for the C<Muldis::DB::Interface::DBMS> constructor,
which has identical parameters, and exists solely as syntactic sugar.
Similarly, the C<DBMS> methods C<new_var> and C<prepare> exist purely as
syntactic sugar over the C<HostGateVar> and C<HostGateRtn> constructors.
I<TODO: Reimplement these as lexical aliases or compile-time macros
instead, to avoid the overhead of extra routine calls.>

The usual way that Muldis::DB indicates a failure is to throw an exception;
most often this is due to invalid input.  If an invoked routine simply
returns, you can assume that it has succeeded, even if the return value is
undefined.

=head2 The Muldis::DB::Interface::DBMS Class

I<This documentation is pending.>

=head2 The Muldis::DB::Interface::HostGateVar Class

I<This documentation is pending.>

=head2 The Muldis::DB::Interface::HostGateRtn Class

I<This documentation is pending.>

=head2 The Muldis::DB::Engine::Role(|::\w+) Roles

This "Muldis::DB" file also defines a few roles that the public interface
classes of all Engine modules must implement, and explicitly declare that
they are doing so.

The initial Engine class, which users specify in the C<$engine_name>
argument to the C<Muldis::DB::Interface::DBMS> constructor, must compose
the C<Muldis::DB::Engine::Role>, and implement the C<new_dbms> submethod.
The DBMS Engine object returned by C<new_dbms> must compose the
C<Muldis::DB::Engine::Role::DBMS> role, and implement the methods
C<new_var> and C<prepare>.  The HostGateVar Engine object returned by
C<new_var> must compose the C<Muldis::DB::Engine::Role::HostGateVar> role,
and implement the methods C<fetch_ast> and C<store_ast>.  The HostGateRtn
Engine object returned by C<new_var> must compose the
C<Muldis::DB::Engine::Role::HostGateRtn> role, and implement the methods
C<bind_host_params> and C<execute>.

The Muldis::DB Interface classes don't just validate user input on behalf
of Engines (allowing them to be simpler), but they also validate each
requested Engine's APIs and results, to some extent, on behalf of users (so
an application can more gracefully handle a bad Engine); the Engine Role
roles exist to help with the latter kind of validation, and they mainly
just declare shims for the required (sub|)methods, which die on invocation
if the Engine didn't declare its own versions; they don't presently contain
any actual functionality for Engines to use.

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 6.x.y that is at least 6.0.0.

It also requires these Perl 6 classes that are in the current distribution:
L<Muldis::DB::AST-(0.0.0)|Muldis::DB::AST>.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

These documentation files are included in the Muldis::DB distribution:
L<Muldis::DB::Language>.

The Perl 6 module L<Muldis::DB::Validator> is bundled with Muldis::DB and
can be used to test Muldis::DB Engines.

The Perl 6 module L<Muldis::DB::Engine::Example> is bundled with Muldis::DB
and implements a self-contained reference implementation of a Muldis::DB
Engine.

Go to the L<Muldis::DB::SeeAlso> file for the majority of external
references.

=head1 BUGS AND LIMITATIONS

The Muldis::DB framework for Perl 6 is built with a lot of code that should
be superfluous, since the Muldis::DB authors can not yet assume that a
number of desired Perl 6 features are actually available in the language
implementations yet, and so and so Muldis::DB includes its own substituted
implementations of those features, which have been made as part of
Muldis::DB for Perl 5 anyway due to Perl 5's relative deficiencies.  The
reimplemented features include manual type-checks of routine arguments (as
if the parameters were declared C<Any>), and the use of C<Array> rather
than C<Seq> or C<Set> or C<Bag>, and the use of Arrays of Arrays rather
than C<Mapping> or C<Hash> (because non-Str keys may not be supported yet).
Also, explicit clones are made of any "read only" Array or Hash arguments
or return values, so to safeguard the Muldis::DB internals against any
subsequent mutation of them by callers.  Hopefully, Muldis::DB for Perl 6
will be able to have its code base slimmed considerably when the Perl 6
implementations themselves are more mature.

I<This documentation is pending.>

=head1 AUTHOR

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENSE AND COPYRIGHT

This file is part of the Muldis::DB framework.

Muldis::DB is Copyright © 2002-2007, Darren Duncan.  All rights reserved.

Muldis::DB is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License (GPL) as published by the Free
Software Foundation (L<http://www.fsf.org/>); either version 3 of the
License, or (at your option) any later version.  You should have received a
copy of the GPL as part of the Muldis::DB distribution, in the file named
"GPL"; if not, see L<http://www.gnu.org/licenses> or write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

I<WARNING:  The GPL version 3 is still being drafted; the publication of
its final version is expected on Friday, June 29, 2007; until then, that
license can not actually be used.  So the previous paragraph just
represents what the license of Muldis::DB is anticipated to be following an
explicit post June 29th re-distribution of it by the copyright holder.  If
you are seeing Muldis::DB prior to that re-distribution, for any reason or
in any place, then you should consider it as not being licensed at all, and
vanilla copyright law applies.  That said, Muldis::DB is hereby licensed to
you under the following proprietary terms:  You may copy, examine, modify,
and execute Muldis::DB, for the purpose of study or evaluation, during only
the time period prior to July 1st of 2007, after which time all privileges
granted to you under this proprietary license expire.  You should then be
able to acquire a subsequent Muldis::DB release that is licensed with the
GPL version 3.>

Linking Muldis::DB statically or dynamically with other components is
making a combined work based on Muldis::DB.  Thus, the terms and conditions
of the GPL cover the whole combination.  However, if it is not feasible for
your combined work to be distributed subject to the GPL, then the copyright
holders of Muldis::DB can sell you an appropriate proprietary license, so
that it is still possible for you to employ Muldis::DB to meet your needs.

For more information on matters such as licensing, including rationale, see
also the L<Muldis::DB::Copying> file that comes with Muldis::DB.

Any versions of Muldis::DB that you modify and distribute must carry
prominent notices stating that you changed the files and the date of any
changes, in addition to preserving this original copyright notice and other
credits. Muldis::DB is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  However, for an additional fee, the
copyright holders of Muldis::DB can sell you a warranty for it.

While it is by no means required, the copyright holders of Muldis::DB would
appreciate being informed any time you create a modified version of
Muldis::DB that you are willing to distribute, because that is a practical
way of suggesting improvements to the standard version.

=head1 ACKNOWLEDGEMENTS

None yet.

=head1 FORUMS

Several public email-based forums for Muldis::DB now exist, all of which
you can reach via L<http://mm.DarrenDuncan.net/mailman/listinfo>; go there
to manage your subscriptions to, or view the archives of, the following:

=over

=item C<muldis-db-announce@mm.DarrenDuncan.net>

This low-volume list is mainly for official announcements from the
Muldis::DB developers, though developers of Muldis::DB extensions can also
post their announcements here.  This is not a discussion list.

=item C<muldis-db-users@mm.DarrenDuncan.net>

This list is for general discussion among people who are using Muldis::DB,
which is not concerned with the implementation of Muldis::DB itself.  This
is the best place to ask for basic help in getting Muldis::DB installed on
your machine or to make it do what you want.  You could also submit feature
requests or report perceived bugs here, if you don't want to use CPAN's RT
system.

=item C<muldis-db-devel@mm.DarrenDuncan.net>

This list is for discussion among people who are designing or implementing
the Muldis::DB core API (including Muldis D language design), or who are
implementing Muldis::DB Engines, or who are writing core documentation,
tests, or examples.  It is not the place for non-implementers to get help
in using said.

=back

An official IRC channel for Muldis::DB is also intended, but not yet
started.

Alternately, you can purchase more advanced commercial support for
Muldis::DB from its author; contact C<perl@DarrenDuncan.net> for details.

=cut
