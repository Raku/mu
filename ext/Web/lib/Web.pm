use v6-alpha;

use HTTP-(0.0.1...);

###########################################################################
###########################################################################

class Web::Request {
    is HTTP::Request;
}

###########################################################################
###########################################################################

class Web::Response {
    is HTTP::Response;

    has $.type where { $_ eq any <html xhtml wml raw> } is rw;
    # sets Content-Type too, assumes UTF-8

    # do something with .headers<Content-Type> to extract charset
    # kill .content, because we're streaming
    # add .print
}

###########################################################################
###########################################################################

role Web::Session {
    has %.session;

    ...
}

###########################################################################
###########################################################################

role Web::Tags {
    method tag_end () {
        given $.response.type {
            when 'html'  { return '>' }
            when 'xhtml' { return '/>' }
            when 'wml'   { return '/>' }
        }
    }

    method img (...) {
        return '<img ' ~ ... ~ .tag_end;
    }
}

###########################################################################
###########################################################################

# Fill .request, .session
# Bind $*OUT and .response.print to whatever actually sends the data
role Web::Init::ModPerl {
    submethod BUILD { ...; next METHOD or last METHOD }
}

###########################################################################
###########################################################################

role Web::Init::CGI {
    submethod BUILD { ...; next METHOD or last METHOD }
}

###########################################################################
###########################################################################

role Web::Init::Foo {
    submethod BUILD { ...; next METHOD or last METHOD }
}

###########################################################################
###########################################################################

class Web-0.0.1 {
    has Web::Request  $.request  handles ...;
    has Web::Response $.response handles ...;

    does Web::Init::ModPerl;
    does Web::Init::CGI;
    does Web::Init::Foo;


    does Web::Util;   # unless disabled?
    # does Web::Session, if requested
    # does Web::Tags, if requested

    # exports automatically initalized object $web if requested
    # with $response if requested, := $web.response
    # ditto for $request, $session, $cookies

    ...
}

###########################################################################
###########################################################################

role Web::Util {
    method entity ($foo) { ... }
    method unentity ($foo) { ... }
    method uriencode ($foo) { ... }  # Not anything-dependent. Toss it?
    method uridecode ($foo) { ... }

    ...
}

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Web -
A general purpose Web toolkit for Perl 6

=head1 VERSION

This document describes C<Web> version 0.0.1.

It also describes the same-number versions of C<Web::Request>,
C<Web::Response>, C<Web::Session>, C<Web::Tags>, C<Web::Init::ModPerl>,
C<Web::Init::CGI>, C<Web::Init::Foo>, C<Web::Util>.

=head1 SYNOPSIS

    use Web;

I<This documentation is pending.>

=head1 DESCRIPTION

This library provides a general purpose toolkit for the Web.

This library, together with the C<HTTP> library, is intended partly to
serve as the defacto standard successor of Perl 5's CGI.pm library in Perl
6, but one that isn't trying to just emulate the design of the old, but
rather do the job in a much better way, more befitting of Perl 6.

Currently, it is in the prototype development stage, and further
development is waiting partly on the availability of desired features in
the Perl 6 language itself.

See also the files C<docs/Juerd_motivation_email_to_p6u_20060917.txt> and
C<docs/Juerd_pseudocode_email_to_p6u_20060921.txt> files in the "HTTP"
distribution, which are the initial proposals that "HTTP" and "Web" are
based on.

I<This documentation is pending.>

=head1 INTERFACE

I<This documentation is pending.>

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 6.x.y that is at least 6.0.0.

It also requires these Perl 6 libraries: L<HTTP-(0.0.1...)|HTTP>.

I<This documentation is pending.>

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

I<This documentation is pending.>

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHORS

Juerd Waalboer (C<juerd@cpan.org>)

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the Web library.

Web is Copyright (c) 2006-2007, Juerd Waalboer.

Web is free software; you can redistribute it and/or modify it under
the same terms as Perl 6 itself.

=head1 ACKNOWLEDGEMENTS

I<This documentation is pending.>

=head1 FORUMS

The main public discussion to date concerning the HTTP and Web
(collectively) Perl 6 packages are the C<perl6-users@perl.org> email
discussion list and the C<#perl6@freenode.org> IRC channel.

I<This documentation is pending.>

=cut
