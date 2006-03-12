package re::override;

use 5.005;
use strict;
use vars qw( @ISA @EXPORT $VERSION );

BEGIN {
    $VERSION = '0.02';

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
our $regcompp = undef;

sub import {
    unless ($inserted++) {
        regexp_exechook_insert();
        regexp_hook_on();
    }

    if (@_ > 1 and index(lc($_[1]), 'pcre') > -1) {
        require re::override::PCRE;
        goto &re::override::PCRE::import;
    }
    else {
        die "Usage: use re::override-PCRE";
    }
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

This document describes version 0.02 of re::override, released
March 10, 2006.

=head1 SYNOPSIS

    use re::override-pcre;

    if ("Hello, world" =~ /(?<=Hello|Hi), (world)/) {
        print "Greetings, $1!";
    }

    no re::override;
    # back to normal regexes

=head1 DESCRIPTION

This module provides a Perl interface for pluggable regular expression
engines.  It affects all regular expressions I<defined> within its scope;
when those regular expresisons are used, an alternate engine is invoked.

Currently, only the I<PCRE> flavour is supported.

=head1 AUTHORS

Mitchell N "putter" Charity,
Audrey Tang

=head1 COPYRIGHT

Copyright 2006 by Audrey Tang E<lt>autrijus@autrijus.orgE<gt>.

The F<libpcre> code bundled with this library by I<Philip Hazel>,
under a BSD-style license.  See the F<LICENCE> file for details.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
