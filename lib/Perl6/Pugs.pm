package Perl6::Pugs;
$Pugs::VERSION = '6.0.8';

use strict;

=head1 NAME

Perl6::Pugs - A Perl6 Implementation

=head1 VERSION

This document describes version 6.0.8 of Pugs, released February 21, 2005.

=head1 SYNOPSIS

    % pugs -e "{ 'Hello, ', @^x }.('World!').say"
    Hello, World!

=head1 DESCRIPTION

Pugs is an implementation of Perl 6, written in Haskell. It aims to support
the full Perl6 specification, as detailed in the Synopses.

The Pugs project was started on February 1st 2005.  It is under active
development on IRC (irc.freenode.net #perl6).

=head2 Release Plans

The major/minor version numbers of Pugs converges to 2*pi; each significant
digit in the minor version represents a milestone.  The third digit is
incremented for each release.

The current milestones are:

=over 4

=item 6.0: Initial release.

=item 6.2: Basic IO and control flow elements; mutable variables; assignment.

=item 6.28: Classes and traits.

=item 6.283: Rules and Grammars.

=item 6.2831: Role composition and other runtime features.

=item 6.28318: Macros.

=item 6.283185: Rewrite in Perl6.

=back

=head1 SEE ALSO

The Pugs homepage is at L<http://pugscode.org/>.

The mailing list for Pugs is perl6-compiler.  Subscribe by sending mail to
E<lt>perl6-compiler-subscribe@perl.orgE<gt>. It is archived at
L<http://www.nntp.perl.org/group/perl.perl6.compiler>
and available via NNTP at L<nntp://nntp.perl.org/perl.perl6.compiler>.

You can also read the list via Google Groups at
L<http://groups-beta.google.com/group/perl.perl6.compiler>

Please submit bug reports to E<lt>bug-perl6-pugs@rt.cpan.orgE<gt>.

=head1 COPYRIGHT

Copyright 2005 by Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>.

This program is free software; you can redistribute it and/or modify it
under the same terms as Pugs itself.

=cut
