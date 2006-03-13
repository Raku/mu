=pod

Tests copied from the perl5 development tree.
13 Mar 2006  r19815
http://cvs.perl.org/viewcvs/perl5/mirror/t/   Sometimes it even works.
http://svn.perl.org/perl5/mirror/t/

This is a "rough cut" set of test files.  Some may be unnecessary.
Some useful ones may be absent.

Anything which tests regexps should be here.  Because the regexp
struct is largely an "open api", which many parts of perl understand
and fiddle with, one can't extrapolate from "regexp hook works with X"
to "regexp hook works with Y".  We need to test both.

We should set things up so developers can run the entire perl5 test suite.


The idea is for this file to require some engine, like re::override::pcre,
and run the perl5 re test suite against it.  Hook and engine failures can't
be distinguished, but there is really no other way to test the hook than to
use it.  And creating a stub engine which uses the real perl5 regexp engine
creates too many opportunities for the hook only working because of some
out of band interaction between the engine and the rest of the perl core.

=cut
