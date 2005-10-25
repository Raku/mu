#!/usr/bin/pugs
use v6;

# External packages used by packages in this file, that don't export symbols:
# (None Yet)

###########################################################################
###########################################################################

# Constant values used by packages in this file:
my $EMPTY_STR is readonly = q{};

###########################################################################
###########################################################################

package Locale::KeyedText-1.7.0 {
    # Note: This given version applies to all of this file's packages.
} # package Locale::KeyedText

###########################################################################
###########################################################################

class Locale::KeyedText::Message {

    # External packages used by the Locale::KeyedText::Message class, that do export symbols:
    # (None Yet)

    # Attributes of every Locale::KeyedText::Message object:
    has Str $:msg_key;
        # Str
        # The machine-readable key that uniquely ident this message.
    has Str %:msg_vars;
        # Hash(Str) of Str
        # Named variables for messages, if any, go here.

###########################################################################

submethod BUILD ($message: Str +$:msg_key, Str +%:msg_vars) {

    throw 'invalid arg'
        if !$:msg_key.defined or $:msg_key eq '';

    throw 'invalid arg'
        if !%:msg_vars.defined or %:msg_vars.exists('');

    return;
}

###########################################################################

method get_msg_key ($message:) returns Str {
    return $:msg_key;
}

method get_msg_var ($message: Str $var_name) returns Str {
    throw 'invalid arg'
        if !$var_name.defined or $var_name eq '';
    return %:msg_vars{$var_name};
}

method get_msg_vars ($message:) returns Hash of Str {
    return hash(%:msg_vars);
}

###########################################################################

} # class Locale::KeyedText::Message

###########################################################################
###########################################################################

class Locale::KeyedText::Translator {

    # External packages used by the Locale::KeyedText::Translator class, that do export symbols:
    # (None Yet)

    # Attributes of every Locale::KeyedText::Translator object:
    has Str @:set_names;
        # Array of Str
        # List of Template module Set Names to search.
    has Str @:member_names;
        # Array of Str
        # List of Template module Member Names to search.

###########################################################################

submethod BUILD ($message: Str +@:set_names, Str +@:member_names) {

    throw 'invalid arg'
        if !@:set_names.defined or +@:set_names == 0;
    for @:set_names -> $set_name {
        throw 'invalid arg'
            if !$set_name.defined or $set_name eq '';
    }

    throw 'invalid arg'
        if !@:member_names.defined or +@:member_names == 0;
    for @:member_names -> $member_name {
        throw 'invalid arg'
            if !$member_name.defined or $member_name eq '';
    }

    return;
}

###########################################################################

method get_set_names ($translator:) returns Array of Str {
    return [@:set_names];
}

method get_member_names ($translator:) returns Array of Str {
    return [@:member_names];
}

######################################################################

method translate_message ($translator: Locale::KeyedText::Message $message)
        returns Str {

    throw 'invalid arg'
        if !$message.defined or !$message.does(Locale::KeyedText::Message);

    my Str $text = undef;
# TODO: Pugs won't parse the following line yet:
#    MEMBER:
    for @:member_names -> $member_name {
# TODO: Pugs won't parse the following line yet:
#        SET:
        for @:set_names -> $set_name {
            my Str $module_name = $set_name ~ $member_name;

            # Determine if requested template module is already loaded.
            # It may have been embedded in a core program file and hence
            # should never be loaded by translate_message().
            my $module_is_loaded = defined ::($module_name);

            # Try to load an external Perl template module; on a require
            # failure, we assume that module intentionally doesn't exist,
            # and so skip to the next candidate module name.
            if (!$module_is_loaded) {
                # Note: We have to invoke this 'require' in an eval string
                # because we need the bareword semantics, where 'require'
                # will munge the package name into file system paths.
                eval "require $module_name;";
                next SET
                    if $!;
            }

            # Try to fetch template text for the given message key from the
            # successfully loaded template module; on a function call
            # death, assume module is damaged and say so; an undefined
            # ret val means module doesn't define key, skip to next module.
            try {
                $text = &::($module_name)::get_text_by_key(
                    $message.get_msg_key() );
                CATCH {
                    throw "damaged template '$module_name': $!";
                }
            };
            next SET
                if !$text.defined;

            # We successfully got template text for the message key, so
            # interpolate the message vars into it and return that.
            for $message.get_msg_vars().kv
                    -> $var_name, $var_value is copy {
                $var_value //= $EMPTY_STR;
                $text ~~ s:perl5:g/\{$var_name\}/$var_value/; # r Pugs only
#                $text ~~ s:g/\{$var_name\}/$var_value/; # req PGE/Parrot
            }
            last MEMBER;
        }
    }

    return $text;
}

###########################################################################

} # class Locale::KeyedText::Translator

###########################################################################
###########################################################################

=pod

=head1 NAME

Locale::KeyedText - Refer to user messages in programs by keys

=head1 VERSION

This document describes Locale::KeyedText version 1.7.0.

It also describes the same-number versions of Locale::KeyedText::Message
and Locale::KeyedText::Translator.

I<Note that the "Locale::KeyedText" package serves only as the name-sake
representative for this whole file, which can be referenced as a unit by
documentation or 'use' statements or Perl archive indexes.  Aside from
'use' statements, you should never refer directly to "Locale::KeyedText" in
your code; instead refer to other above-named packages in this file.>

=head1 SYNOPSIS

    use Locale::KeyedText;

    main();

    sub main () {
        # Create a translator.
        my $translator = Locale::KeyedText::Translator.new({
            'set_names' => ['MyLib::Lang::', 'MyApp::Lang::'],
                # set package prefixes for localized app components
            'member_names' => ['Eng', 'Fr', 'De', 'Esp'],
                # set list of available languages in order of preference
        });

        # This will print 'Enter 2 Numbers' in the first of the four
        # languages that has a matching template available.
        print $translator.translate_message(
            Locale::KeyedText::Message.new({
                'msg_key' => 'MYAPP_PROMPT' }) );

        # Read two numbers from the user.
        my Num ($first, $second) = $*IN;

        # Print a statement giving the operands and their sum.
        MyLib.add_two( $first, $second, $translator );
    }

    module MyLib;

    sub add_two (Num $first, Num $second,
            Locale::KeyedText::Translator $translator) {
        my Num $sum = $first + $second;

        # This will print '<FIRST> plus <SECOND> equals <RESULT>' in
        # the first possible language.  For example, if the user
        # inputs '3' and '4', it the output will be '3 plus 4 equals 7'.
        print $translator.translate_message(
            Locale::KeyedText::Message.new({ 'msg_key' => 'MYLIB_RESULT',
                'msg_vars' => { 'FIRST' => $first, 'SECOND' => $second,
                'RESULT' => $sum } }) );
    }

=head1 DESCRIPTION

Many times during a program's operation, the program (or a module it uses)
will need to display a message to the user, or generate a message to be
shown to the user.  Sometimes this is an error message of some kind, but it
could also be a prompt or response message for interactive systems.

If the program or any of its components are intended for widespread use
then it needs to account for a variance of needs between its different
users, such as their preferred language of communication, or their
privileges regarding access to information details, or their technical
skills.  For example, a native French or Chinese speaker often prefers to
communicate in those languages.  Or, when viewing an error message, the
application's developer should see more details than joe public would.

Alternately, sometimes a program will raise a condition or error that,
while resembling a message that would be shown to a user, is in fact meant
to be interpreted by the machine itself and not any human user.  In some
situations, a shared program component may raise such a condition, and one
application may handle it internally, while another one displays it to the
user instead.

Locale::KeyedText provides a simple but effective mechanism for
applications and modules that empowers single binaries to support N locales
or user types simultaneously, and that allows any end users to add support
for new languages easily and without a recompile (such as by simply copying
files), often even while the program is executing.

Locale::KeyedText gives your application the maximum amount of control as
to what the user sees; it never outputs anything by itself to the user, but
rather returns its results for calling code to output as it sees fit.  It
also does not make direct use of environment variables, which can aid in
portability.

Practically speaking, Locale::KeyedText doesn't actually do a lot
internally; it exists mainly to document a certain localization methodology
in an easily accessable manner, such that would not be possible if its
functionality was subsumed into a larger module that would otherwise use
it.  Hereafter, if any other module or application says that it uses
Locale::KeyedText, that is a terse way of saying that it subscribes to the
localization methodology that is described here, and hence provides these
benefits to developers and users alike.

For some practical examples of Locale::KeyedText in use, see my dependent
CPAN modules whose problem domain is databases and/or SQL.

=head1 FUNCTIONS AND METHODS

I<This documentation is pending; this section may also be split into several.>

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 6.x.y that is at least 6.0.0.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

I<This documentation is pending.>

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHOR

Darren R. Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the Locale::KeyedText library.

Locale::KeyedText is Copyright (c) 2003-2005, Darren R. Duncan.  All rights
reserved.  Address comments, suggestions, and bug reports to
C<perl@DarrenDuncan.net>, or visit L<http://www.DarrenDuncan.net/> for more
information.

Locale::KeyedText is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License (LGPL) as
published by the Free Software Foundation (L<http://www.fsf.org/>); either
version 2.1 of the License, or (at your option) any later version.  You
should have received a copy of the LGPL as part of the Locale::KeyedText
distribution, in the file named "LGPL"; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.

Any versions of Locale::KeyedText that you modify and distribute must carry
prominent notices stating that you changed the files and the date of any
changes, in addition to preserving this original copyright notice and other
credits. Locale::KeyedText is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the LGPL for more
details.

While it is by no means required, the copyright holders of
Locale::KeyedText would appreciate being informed any time you create a
modified version of Locale::KeyedText that you are willing to distribute,
because that is a practical way of suggesting improvements to the standard
version.

=head1 ACKNOWLEDGEMENTS

None yet.

=cut
