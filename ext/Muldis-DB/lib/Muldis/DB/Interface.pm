use v6-alpha;

###########################################################################
###########################################################################

module Muldis::DB::Interface-0.3.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub new_dbms of Muldis::DB::Interface::DBMS
        (Str :$engine_name!, Any :$dbms_config!) {
    return ::Muldis::DB::Interface::DBMS.new(
        :engine_name($engine_name), :dbms_config($dbms_config) );
}

###########################################################################

} # module Muldis::DB::Interface

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
        (Muldis::DB::Literal::_TypeInvo :$decl_type!) {
    return ::Muldis::DB::Interface::HostGateVar.new(
        :dbms(self), :decl_type($decl_type) );
}

method prepare of Muldis::DB::Interface::HostGateRtn
        (Muldis::DB::Literal::HostGateRtn :$rtn_ast!) {
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
        Muldis::DB::Literal::_TypeInvo :$decl_type!) {

    die q{new(): Bad :$dbms arg; it is not an object of a}
            ~ q{ Muldis::DB::Interface::DBMS-doing class.}
        if !$dbms.defined or !$dbms.does(Muldis::DB::Interface::DBMS);
    my $dbms_eng = $dbms!dbms_eng;
    my $dbms_eng_class = $dbms_eng.WHAT;

    die q{new(): Bad :$decl_type arg; it is not an object of a}
            ~ q{ Muldis::DB::Literal::_TypeInvo-doing class.}
        if !$decl_type.defined
            or !$decl_type.does(Muldis::DB::Literal::_TypeInvo);

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

method fetch_ast of Muldis::DB::Literal::Node () {

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

method store_ast (Muldis::DB::Literal::Node :$val_ast!) {

    die q{store_ast(): Bad :$val_ast arg; it is not an object of a}
            ~ q{ Muldis::DB::Literal::Node-doing class.}
        if !$val_ast.defined or !$val_ast.does(Muldis::DB::Literal::Node);

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
    has Muldis::DB::Literal::HostGateRtn $!rtn_ast;
    has Any                          $!rtn_eng;

###########################################################################

submethod BUILD (Muldis::DB::Interface::DBMS :$dbms!,
        Muldis::DB::Literal::HostGateRtn :$rtn_ast!) {

    die q{new(): Bad :$dbms arg; it is not an object of a}
            ~ q{ Muldis::DB::Interface::DBMS-doing class.}
        if !$dbms.defined or !$dbms.does(Muldis::DB::Interface::DBMS);
    my $dbms_eng = $dbms!dbms_eng;
    my $dbms_eng_class = $dbms_eng.WHAT;

    die q{new(): Bad :$rtn_ast arg; it is not an object of a}
            ~ q{ Muldis::DB::Literal::HostGateRtn-doing class.}
        if !$rtn_ast.defined
            or !$rtn_ast.does(Muldis::DB::Literal::HostGateRtn);

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
                ~ q{ Muldis::DB::Literal::EntityName-doing class.}
            if !$param_name.defined
                or !$param_name.does(Muldis::DB::Literal::EntityName);
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
                ~ q{ Muldis::DB::Literal::EntityName-doing class.}
            if !$param_name.defined
                or !$param_name.does(Muldis::DB::Literal::EntityName);
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

Muldis::DB::Interface -
Common public API for Muldis::DB Engines

=head1 VERSION

This document describes Muldis::DB::Interface version 0.3.0 for Perl 6.

It also describes the same-number versions for Perl 6 of
Muldis::DB::Interface::DBMS ("DBMS"), Muldis::DB::Interface::HostGateVar
("HostGateVar"), and Muldis::DB::Interface::HostGateRtn ("HostGateRtn").

It also describes the same-number versions for Perl 6 of
Muldis::DB::Engine::Role, Muldis::DB::Engine::Role::DBMS,
Muldis::DB::Engine::Role::HostGateVar, and
Muldis::DB::Engine::Role::HostGateRtn.

=head1 SYNOPSIS

    use Muldis::DB::Interface;

    # Instantiate a Muldis::DB DBMS / virtual machine.
    my $dbms = Muldis::DB::Interface::new_dbms(
        :engine_name('Muldis::DB::Engine::Example'),
        :dbms_config({}),
    );

    # TODO: Create or connect to a repository and work with it.

I<This documentation is pending.>

=head1 DESCRIPTION

B<Muldis::DB::Interface>, aka I<Interface>, comprises the minimal core of
the Muldis DB framework, the one component that probably every program
would use.  Together with the Muldis D language (see L<Language::MuldisD>),
it defines the common API for Muldis DB implementations to do and which
applications invoke.

I<This documentation is pending.>

=head1 INTERFACE

The interface of Muldis::DB::Interface is fundamentally object-oriented;
you use it by creating objects from its member classes, usually invoking
C<new()> on the appropriate class name, and then invoking methods on those
objects.  All of their attributes are private, so you must use accessor
methods.

Muldis::DB::Interface also provides the not-exportable wrapper subroutine
C<Muldis::DB::new_dbms> for the C<Muldis::DB::Interface::DBMS> constructor,
which has identical parameters, and exists solely as syntactic sugar.
Similarly, the C<DBMS> methods C<new_var> and C<prepare> exist purely as
syntactic sugar over the C<HostGateVar> and C<HostGateRtn> constructors.
I<TODO: Reimplement these as lexical aliases or compile-time macros
instead, to avoid the overhead of extra routine calls.>

The usual way that Muldis::DB::Interface indicates a failure is to throw an
exception; most often this is due to invalid input.  If an invoked routine
simply returns, you can assume that it has succeeded, even if the return
value is undefined.

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

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

Go to L<Muldis::DB> for the majority of distribution-internal references,
and L<Muldis::DB::SeeAlso> for the majority of distribution-external
references.

=head1 BUGS AND LIMITATIONS

The Muldis DB framework for Perl 6 is built with a lot of code that should
be superfluous, since the Muldis DB authors can not yet assume that a
number of desired Perl 6 features are actually available in the language
implementations yet, and so and so Muldis DB includes its own substituted
implementations of those features, which have been made as part of Muldis
DB for Perl 5 anyway due to Perl 5's relative deficiencies.  The
reimplemented features include manual type-checks of routine arguments (as
if the parameters were declared C<Any>), and the use of C<Array> rather
than C<Seq> or C<Set> or C<Bag>, and the use of Arrays of Arrays rather
than C<Mapping> or C<Hash> (because non-Str keys may not be supported yet).
Also, explicit clones are made of any "read only" Array or Hash arguments
or return values, so to safeguard the Muldis DB internals against any
subsequent mutation of them by callers.  Hopefully, Muldis DB for Perl 6
will be able to have its code base slimmed considerably when the Perl 6
implementations themselves are more mature.

I<This documentation is pending.>

=head1 AUTHOR

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENSE AND COPYRIGHT

This file is part of the Muldis DB framework.

Muldis DB is Copyright © 2002-2007, Darren Duncan.

See the LICENSE AND COPYRIGHT of L<Muldis::DB> for details.

=head1 ACKNOWLEDGEMENTS

The ACKNOWLEDGEMENTS in L<Muldis::DB> apply to this file too.

=cut
