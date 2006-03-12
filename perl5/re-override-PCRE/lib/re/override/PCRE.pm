package re::override::PCRE;
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

sub import {
    $^H{regcompp} = sub {
        my $re = compile($_[0], 0);
        return reverse(13, "b", 1, sub {
            my $match = execute($re, $_[0], 0);
            my @rv = ((@$match ? 1 : 0), undef, undef, @$match);
            return reverse(@rv);
        });
    };
}

1;

__END__

=head1 NAME 

re::override::PCRE - Perl-compatible regular expressions

=head1 SYNOPSIS

    use re::override-PCRE;

    if ("Hello, world" =~ /(?<=Hello|Hi), (world)/) {
        print "Greetings, $1!";
    }

=head1 DESCRIPTION

This module provides a Perl interface to use the B<libpcre> library for
regular expressions.

=head1 AUTHORS

Audrey Tang

=head1 COPYRIGHT

Copyright 2006 by Audrey Tang E<lt>autrijus@autrijus.orgE<gt>.

The F<libpcre> code bundled with this library by I<Philip Hazel>,
under a BSD-style license.  See the F<LICENCE> file for details.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
