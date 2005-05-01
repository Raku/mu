package Perl6::Pugs;
use 5.006;
use strict;

our $VERSION = 6.002002;

=head1 NAME

Perl6::Pugs - A Perl 6 Implementation

=head1 VERSION

This document describes version 6.2.2 of Pugs, released May 1, 2005.

=head1 SYNOPSIS

    % pugs -e "{ 'Hello, ', @^x }.('World!').say"
    Hello, World!

=head1 DESCRIPTION

Pugs is an implementation of Perl 6, written in Haskell. It aims to support
the full Perl 6 specification, as detailed in the Synopses.

The Pugs project was started on February 1st 2005.  It is under active
development on IRC (irc.freenode.net #perl6).

Please read the Pugs Apocrypha in the F<docs/> directory for more details.

=head1 SEE ALSO

The Pugs homepage is at L<http://pugscode.org/>.

The mailing list for Pugs is perl6-compiler.  Subscribe by sending mail to
E<lt>perl6-compiler-subscribe@perl.orgE<gt>. It is archived at
L<http://www.nntp.perl.org/group/perl.perl6.compiler>
and available via NNTP at L<nntp://nntp.perl.org/perl.perl6.compiler>.

You can also read the list via Google Groups at
L<http://groups-beta.google.com/group/perl.perl6.compiler>

Please submit bug reports to E<lt>pugsbugs@perl.org<gt>.

=head1 COPYRIGHT

Copyright 2005 by Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>.

This code is free software; you can redistribute it and/or modify it under
the terms of either:

    a) the GNU General Public License, version 2, or
    b) the Artistic License, version 2.0beta5.

For the full license text, please see the F<GPL-2> and F<Artistic-2> files
under the F<LICENSE> directory in the Pugs distribution.

=cut
