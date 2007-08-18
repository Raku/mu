#!/usr/bin/perl

=head1 NAME

evalbot.pl - a pluggable p5 evalbot

=head1 SYNOPSIS

perl -Ilib evalbot.pl <configfile>

=head1 DESCRIPTION

evalbot.pl is a perl5 based evalbot, intended for the different Perl 6
implementations.

Take a look at the example config file that is hopefully is the same 
directory.

=head1 AUTHOR

Written by Moritz Lenz.

Copyright (C) 2007 by Moritz Lenz and the pugs authors.

This file may be distributed under the same terms as perl or pugs itself.

=cut

use warnings;
use strict;

use Bot::BasicBot;
use Config::File;
use EvalbotExecuter;
use Carp qw(confess);
use Data::Dumper;


package Evalbot;
{
    use base 'Bot::BasicBot';
    use File::Temp qw(tempfile);
    my $prefix = '\#';

    my %executer = (
            echo    => \&exec_echo,
            kp6     => \&exec_kp6,
            );
    my $regex = $prefix . '(' . join('|',  keys %executer) . ')';
#    warn "Regex: ", $regex, "\n";

    sub said {
        my $self = shift;
        my $e = shift;
        my $message = $e->{body};
        if ($message =~ m/\A$regex\s+(.*)\z/){
#            return "Stub: eval'ing <$2> with $1";
            my ($e, $str) = ($executer{$1}, $2);
            warn "String: $str\n";
#            warn "Executer: $e\n";
            return EvalbotExecuter::run($str, $e);
        }
        return undef;
    }

    sub emoted {
        my $self = shift;
        my $e = shift;
        return undef;

    }

    sub exec_echo {
        my ($program, $fh, $filename) = @_;
        print $fh $program;
        close $fh;
    }

    sub exec_kp6 {
        my ($program, $fh, $filename) = @_;
        chdir('../../v6/v6-KindaPerl6/')
            or confess("Can't chdir to kp6 dir: $!");
        my ($tmp_fh, $name) = tempfile();
        print $tmp_fh $program;
        close $tmp_fh;
        system "perl kp6-perl5.pl < $name | perl -Ilib > $filename 2>&1";
        close $fh;
        unlink $name;
        return;
    }

}

package main;

my $config_file = shift @ARGV 
    or confess("Usage: $0 <config_file>");
my %conf = %{ Config::File::read_config_file($config_file) };

#warn Dumper(\%conf);

my $bot = Evalbot->new(
        server => $conf{server},
        port   => $conf{port} || 6667,
        channels  => [ map { "#$_" } split m/\s+/, $conf{channels} ],
        nick      => $conf{nick},
        alt_nicks => [ split m/\s+/, $conf{alt_nicks} ],
        username  => "evalbot",
        name      => "combined, experimental evalbot",
        charset   => "utf-8",
        );
$bot->run();


# vim: ts=4 sw=4 expandtab
