package re::override;

use 5.005;
use strict;
use vars qw( @ISA @EXPORT $VERSION );

BEGIN {
    $VERSION = '0.03';

    local $@;
    eval {
        require XSLoader;
        XSLoader::load(__PACKAGE__ => $VERSION);
        1;
    } or do {
        require DynaLoader;
        push @ISA, 'DynaLoader';
        __PACKAGE__->bootstrap($VERSION);
    };
}

our $inserted = 0;
BEGIN { our $regcompp = undef };

sub import {
    my $class = shift;
    my $flavor = shift;
    $flavor =~ s/\W//g;

    die "Usage: use re::override-flavor" unless $flavor;

    unless ($inserted++) {
        regexp_exechook_insert();
        regexp_hook_on();
    }

    no strict 'refs';
    require "re/override/$flavor.pm";
    goto &{"re::override::${flavor}::import"};
}

sub unimport {
    regexp_hook_off();
}

sub make_qr_regexp_pair {
  my($pat,$nparens,$callback)=@_;
  die "bug - no nparens" if !defined($nparens);
  die "bug - no callback" if !defined($callback);
  my $r_address;
  $^H{regcompp} = sub {
    $r_address = $_[0];
    return reverse(13,$pat,$nparens,$callback);
  };
  my $qr = eval 'qr//'; die $@ if $@;
  return ($qr,$r_address);
}

1;

__END__

=head1 NAME 

re::override - Override Perl regular expressions

=head1 VERSION

This document describes version 0.03 of re::override, released
March 12, 2006.

=head1 SYNOPSIS

    use re::override-PCRE;

    if ("Hello, world" =~ /(?<=Hello|Hi), (world)/) {
        print "Greetings, $1!";
    }

    no re::override;
    # back to normal regexes

=head1 DESCRIPTION

This module provides a Perl interface for pluggable regular expression
engines.  It affects all regular expressions I<defined> within its scope;
when those regular expresisons are used, an alternate engine is invoked.

Currently, only the I<PCRE> flavor is supported.

=head1 CAVEATS

This is an experimental, pre-alpha development snapshot.  There are currently
no support for match flags; regexes constructed with this module may cause
segfaults on C<split>, substitution, and/or other uses.

It is currently unsuitable for just about any use other than Pugs development.

An user-supplied regular expression may lead to execution of arbitrary code;
each alternate engine may introduce additional security or memory problems.

Tainting information is discarded.

The perl5 regexp engine is entangled with the rest of the perl core.
Some of the interaction is via global variables.  A lot of it uses a
regexp struct, which gets attached to the op tree.  The struct is used
both to return a compiled regexp, and to provide match results.  This
module is an experiment, to see if it is possible to use the regexp
struct as a new api into the guts of the perl core.  There being no
documentation, one ends up reverse engineering perl source.

We basically use the perl regcompp and regexecp hooks (C functions
which get called on regular expression compilation and execution) to
parasitize the regexp struct - attempting to make it look sufficiently
like what the core expects to avoid problems.  While simultaneously
trying to avoid triggering core behavior which we are not prepared to
support (such as modification of the compiled re op tree the native
compiler creates, but which we obviously don't have).

Whether this will all succeed or not is an open question.  But the
perl5 team has expressed interest, and a potential willingness to do a
core patch if one proves necessary to go the last mile.

=head1 BUGS

Numerous.

=head1 TODO

=over
=item Scoping

You should be able to do both lexically and dynamically scoped
overriding.  You currently can't do lexical.  It's not clear how to do
it.  Possible tools include get_sv, eval_pv, and %^H.  The first thing
needed is a scope.t test file.

=item Memory

One sees both segfaults and memory exhaustion when running against
some of the perl5 t/op/ test files.  Use of the perl api has to be
checked for correctness, and the problems tracked down.

=item Completeness

\G, pos(), etc, are not yet supported.

=back

=head1 AUTHORS

Audrey Tang

=head1 COPYRIGHT

Copyright 2006 by Audrey Tang E<lt>autrijus@autrijus.orgE<gt>.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
