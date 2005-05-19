package Inline::Pugs;

use strict;
use vars qw<$VERSION @ISA>;
use constant MAGIC =>
    'my$Z= =$*IN;while 1{$_=perl eval eval=$*IN;print$Z;say$!//$_;print$Z;flush$*OUT}';
use constant COOKIE => rand();
use Perl6::Pugs;
use IPC::Open2;
use Data::Dumper;

@ISA     = 'Inline';
$VERSION = 0.01;

=head1 NAME

Inline::Pugs - Inline Perl 6 code in Perl 5

=head1 SYNOPSIS

    use Inline Pugs => '
        sub postfix:<!> { [*] 1..$_ }
        sub sum_factorial { [+] 0..$_! }
    ';
    print sum_factorial(3); # 21

=head1 DESCRIPTION

Is it Perl 5?  Is it Perl 6?  It's neither, it's both.  It's Inline::Pugs!

The Inline::Pugs module allows you to insert Perl 6 source code directly 
I<inline> in a Perl 5 script or module.

=head1 CAVEATS

Currently, only the Perl 5 side can invoke subroutines defined from the
Perl 6 size, but not vise versa.  This whole thing is just a proof of
concept -- use it at your own risk. :-)

=cut

sub register {
    return {
        language => 'Pugs',
        aliases  => [ qw(pugs) ],
        type     => 'interpreted',
        suffix   => 'p6',
    };
}

sub validate { }

sub build {
    my $self = shift;
    my $path = "$self->{API}{install_lib}/auto/$self->{API}{modpname}";
    my $obj  = $self->{API}{location};
    $self->mkpath($path)                   unless -d $path;
    $self->mkpath($self->{API}{build_dir}) unless -d $self->{API}{build_dir};
    local *OBJECT;
    open(OBJECT, ">$obj") or die "Unable to open object file: $obj : $!";
    close(OBJECT) or die "Unable to close object file: $obj : $!";
}

sub load {
    my $self = shift;
    my $code = $self->{API}{code};
    my $pkg  = $self->{API}{pkg} || 'main';
    my $pid  = $self->{pid} ||= $self->init_pugs;
    $self->eval_pugs($code);

    # now try to figure out what functions are toplevel...
    # XXX - bloody hack for now

    no strict 'refs';
    foreach my $sym ($code =~ /^\s*sub\s+(\w+)\s+/mg) {
        *{"$pkg\::$sym"} = sub {
            local $Data::Dumper::Terse = 1;
            my @args = map { $self->quote_pugs(Dumper($_)).'.eval' } @_;
            $self->eval_pugs(
                "$sym(".join(',', @args).")"
            );
        }
    }
}

sub init_pugs {
    my $self = shift;
    my $pid = open2(\*OUT, \*IN, 'pugs', '-e', MAGIC);
    print IN COOKIE, "\n";
    return $pid;
}

sub eval_pugs {
    my $self = shift;
    print IN $self->quote_pugs($_[0]), "\n";
    local $/ = COOKIE . "\n";
    print substr(<OUT>, 0, -length($/));
    my $out = substr(<OUT>, 0, -length($/));
    $out =~ s{\n+$}{};
    $out =~ s{^\n+}{};
    die $out if $out =~ /\n/;
    return eval $out;
}

sub quote_pugs {
    my $self = shift;
    my $q = join '', map { sprintf("\\x%02X", ord) } split(//, $_[0]);
    return qq["$q"];
}

1;

=head1 AUTHORS

Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>

=head1 COPYRIGHT

Copyright 2005 by Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>.

This code is free software; you can redistribute it and/or modify it under
the terms of either:

    a) the GNU General Public License, version 2, or
    b) the Artistic License, version 2.0beta5.

For the full license text, please see the F<GPL-2> and F<Artistic-2> files
under the F<LICENSE> directory in the Pugs distribution.

=cut
