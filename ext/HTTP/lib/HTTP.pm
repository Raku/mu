use v6-alpha;

###########################################################################
###########################################################################

package HTTP-0.0.1 {
    # Note: This given version applies to all of this file's packages.
} # package HTTP

###########################################################################
###########################################################################

class HTTP::Headers {
    does Hash;

    ...
}

###########################################################################
###########################################################################

class HTTP::Message {
    has HTTP::Headers $.headers  handles <header>  .= new;
    has buf8          $.content  is rw;
    has HTTP::Message @.parts    is rw;
    has Str           $.protocol is rw;

    ...
}

###########################################################################
###########################################################################

class HTTP::Argument {
    does Array;
    does Str;

    # Possibly, does HTTP::Argument::Upload

    ...
}

###########################################################################
###########################################################################

class HTTP::Request {
    is HTTP::Message;
    does Hash;  # %post{$key} // %get{$key}

    has HTTP::Argument %.get;
    has HTTP::Argument %.post;  # lazy if .method eq 'POST' and
                                # .headers<Content-Length>
                                # > $arbitrary_yet_configurable
    has Str            %.cookies
    has Str            $.method where { $_ eq any <GET POST> };
    has URI            $.uri;

    ...
}

###########################################################################
###########################################################################

role HTTP::Argument::Upload {
    ...
}

###########################################################################
###########################################################################

class HTTP::Response {
    is HTTP::Message;
    has $.encoding = 'UTF-8' is rw;
    ...
}

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

HTTP -
A general purpose HTTP toolkit for Perl 6

=head1 VERSION

This document describes C<HTTP> version 0.0.1.

It also describes the same-number versions of C<HTTP::Headers>,
C<HTTP::Message>, C<HTTP::Argument>, C<HTTP::Request>,
C<HTTP::Argument::Upload>, and C<HTTP::Response>.

I<Note that the "HTTP" package itself is currently just used as a name-sake
representative for this whole file.  That may change later.>

=head1 SYNOPSIS

    use HTTP;

I<This documentation is pending.>

=head1 DESCRIPTION

This library provides a general purpose toolkit for HTTP.

This library, together with the C<Web> library, is intended partly to serve
as the defacto standard successor of Perl 5's CGI.pm library in Perl 6, but
one that isn't trying to just emulate the design of the old, but rather do
the job in a much better way, more befitting of Perl 6.

Currently, it is in the prototype development stage, and further
development is waiting partly on the availability of desired features in
the Perl 6 language itself.

See also the files C<docs/Juerd_motivation_email_to_p6u_20060917.txt> and
C<docs/Juerd_pseudocode_email_to_p6u_20060921.txt> files in this
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

I<This documentation is pending.>

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

These Perl 6 packages are the initial main dependents of HTTP: L<Web>.

I<This documentation is pending.>

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHORS

Juerd Waalboer (C<juerd@cpan.org>)

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the HTTP library.

HTTP is Copyright (c) 2006-2007, Juerd Waalboer.

HTTP is free software; you can redistribute it and/or modify it under
the same terms as Perl 6 itself.

=head1 ACKNOWLEDGEMENTS

I<This documentation is pending.>

=head1 FORUMS

The main public discussion to date concerning the HTTP and Web
(collectively) Perl 6 packages are the C<perl6-users@perl.org> email
discussion list and the C<#perl6@freenode.org> IRC channel.

I<This documentation is pending.>

=cut
