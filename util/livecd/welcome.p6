#!/bin/pugs

say "
=== What is Pugs?

Pugs is an implementation of Perl 6, written in Haskell. It aims to implement
the full Perl6 specification, as detailed in the Synopses. For more
information, please see http://www.pugscode.org/.

=== Where are we?

The Pugs project was started on February 1st, 2005 by Autrijus Tang. To keep
up to date on the latest developments, join us on IRC (irc.freenode.net
#perl6).
";

pause();

say "
=== How to get the code

Periodic releases will appear on CPAN under the Perl6-Pugs namespace.

For the very latest version of Pugs, check out the source from Subversion
(mirror) or darcs. There is anonymous access to all. Commit access is handed
out liberally; contact the Pugs team for details. (By the way, if you'd like
offline working with the Subversion repository, the svk client may be of
interest. But using vanilla svn is fine.)

=== How to get involved

Pugs development takes place on the perl6-compiler mailing list. If you follow
this list, you'll be able to get an idea of what sort of things are needed.
Subscribe by sending mail to perl6-compiler-subscribe@perl.org or read the
perl6-compiler NNTP archive at http://nntp.perl.org/group/perl.perl6.compiler.
";

pause();

say "
=== Have a lot of fun!

";

sub pause() {
  print "Press any key to continue... ";
  =<>;
}
