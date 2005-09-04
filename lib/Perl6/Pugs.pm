package Perl6::Pugs;
use 5.006;
use strict;

our $VERSION = 6.002009;

=head1 NAME

Perl6::Pugs - A Perl 6 Implementation

=head1 VERSION

This document describes version 6.2.9 of Pugs, released August 3, 2005.

=head1 SYNOPSIS

    % pugs -e "{ 'Hello, ', @^x }.('World!').say"
    Hello, World!

=head1 DESCRIPTION

Pugs is an implementation of L<Perl 6|http://dev.perl.org/perl6>, written in
L<Haskell|http://www.haskell.org/>. It aims to support the full Perl 6
specification, as detailed in the
L<Synopses|http://dev.perl.org/perl6/doc/synopsis.html>.

The Pugs project was started on February 1st 2005.  It is under active
development on IRC (C<irc.freenode.net>, C<#perl6>).

Please read the Pugs Apocrypha in the F<docs/> directory for more details.

=head1 DOCUMENTATION

=head2 Pugs Apocrypha

The Pugs Apocrypha in the F<docs/> directory are a series of documents, written
in question/answer format, to explain the design and implementation of Pugs.

=head2 Quick reference of Perl 6

Quick reference documents of Perl 6 can be found in the F<docs/quickref/>
directory, similar to L<perlcheat>. The table of contents can be found at
F<docs/quickref/README>.

=head2 Talks

There're various talks about Perl 6 and/or Pugs in F<docs/talks/>. Look into
F<docs/talks/README> for links to rendered versions (HTML, PDF).

=head2 Examples

In the F<examples/> directory there're examples of working Perl 6 code. In
F<examples/cookbook/> you can find the Perl 6 version of the Perl 5 cookbook.
If you want to see how Perl 6 modules look like, you can have a look at the
modules in F<ext/>. Another good source of working Perl 6 code is the
comprehensive test suite in the F<t/> directory.

=head2 Apocalypses, Exegeses, and Synopses (AES)

Additionally to the L<official AES|http://dev.perl.org/perl6/>, Pugs hosts a
number of unofficial AES at F<docs/AES/>. You might also want to checkout the
Perl 6 module L<Perl6::Bible>, hosted at L<FreePAN|http://www.freepan.org/>
(L<http://tpe.freepan.org/repos/iblech/Perl6-Bible/>), if you want to read the
AES by using the C<p6bible> command line utility.

=head1 SEE ALSO

The Pugs homepage is at L<http://pugscode.org/>.

The mailing list for Pugs is C<perl6-compiler>. Subscribe by sending mail to
C<E<lt>perl6-compiler-subscribe@perl.orgE<gt>>. It is archived at
L<http://www.nntp.perl.org/group/perl.perl6.compiler> and available via NNTP at
L<nntp://nntp.perl.org/perl.perl6.compiler>.

You can also read the list via Google Groups at
L<http://groups-beta.google.com/group/perl.perl6.compiler>.

Please submit bug reports to C<E<lt>pugsbugs@perl.orgE<gt>>.

=head1 COPYRIGHT

Copyright 2005 by Autrijus Tang C<E<lt>autrijus@autrijus.orgE<gt>>.

This code is free software; you can redistribute it and/or modify it under
the terms of either:

    a) the GNU General Public License, version 2, or
    b) the Artistic License, version 2.0beta5.

For the full license text, please see the F<GPL-2> and F<Artistic-2> files
under the F<LICENSE> directory in the Pugs distribution.

=cut
