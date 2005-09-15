#!/usr/bin/perl

package Blondie::Backend::Perl::Silly;
use base qw/Blondie::Backend::Perl/;

use strict;
use warnings;

use Term::ReadKey;

my %masked_ops = (
    map { ("&infix:<$_>" => 1) } grep {
        print "Use perl version of &infix:<$_> instead of Blondie one from prelude? ";
        ReadMode(3);
        my $y = lc(ReadKey(0)) eq 'y';
        ReadMode(0);
        print '' . ($y ? "compiling" : "skipping") . "...\n";
        !$y;
    } qw(* ** /),
);

sub provides {
    my $self = shift;
    my $digest = shift;

    my $prim = $self->SUPER::provides($digest);
    ($prim && $prim->can("name") && exists $masked_ops{$prim->name}) ? undef : $prim;
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Backend::Perl::Silly - 

=head1 SYNOPSIS

    use Blondie::Backend::Perl::Silly;

=head1 DESCRIPTION

=cut


