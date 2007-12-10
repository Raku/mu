#!/usr/bin/env perl

=head1 NAME

script/perl6_to_perl6.pl

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

 pugs/v6/v6-KindaPerl6> perl script/perl6_to_perl6.pl test.t

perl6_to_perl6.pl will convert test.pl (perl6) to perl6 via KindaPerl6.

perl --backend=perl6 script/kp6 test.t | perl script/kp6.t

The purpose of this test is to insure that the perl6 backend for kp6 produces
VALID perl6 code.  It is conceivable that the perl6 backend will NOT produce
valid perl6 code that kp6 will still execute, however, we hope that this will
not happen.

What perl6 code will the perl6 backend produce that is not valid perl6?
Anything that is a KindaPerl6 specific feature.

See v6-KindaPerl6/docs/kindaperl6-spec.pod

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

use strict;
use warnings;
use English;

use File::Temp qw/ tempfile /;

@ARGV = grep { !m!script/kp6! } @ARGV;                  # exclude script/kp6
@ARGV = grep { !m!^-Bperl6|--backend=perl6! } @ARGV;    # exclude backend args

# print STDERR "\nExecuted as: $0 ", join ' ', @ARGV, "\n";

# read in the file
my ( $source, @args ) = load_source();

my ( $fh, $filename ) = tempfile();

print $fh $source;
close $fh;

my $args = join ' ', @args;
my $exec = "perl script/kp6 --backend=perl6 $args $filename | perl script/kp6";

# print STDERR "Executing: $exec\n";
system($exec);

unlink $filename;    # delete file

# WARNING: This load_source is different from the other similar copies.
# in kp6 and mp6.pl
sub load_source {
    my ( $source, @args );
    @args = @ARGV;
    if ( -t STDIN ) {

        # STDIN is open to a terminal, i.e. we're being run as `kp6
        # file.p6'. slurp the file
        my $file = pop @args;

        # print STDERR "Reading in $file\n";
        if ($file) {
            $source = slurp($file);
        }
        else {
            $source = '';
        }
    }
    else {

        # Called as `kp6 < file.p6', get source code from STDIN
        local $/;

        $source = <>;
    }

    # this only removes a fraction in a second off of the parsing time
    # I am leaving this here, so that when our perl6 scripts get larger
    # or we parse the AST/Vistor/Grammar code, it will run faster.
    $source =~ s/^ +//gm;
    return ( $source, @args );
}

# Eat that File::Slurp!
sub slurp {
    do {
        local ( @ARGV, $/ ) = $_[0];
        scalar <>;
    };
}
