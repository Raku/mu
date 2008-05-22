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
use Carp qw(confess);
use Data::Dumper;
use FindBin;
use lib 'lib';
use EvalbotExecuter;


package Evalbot;
{
    use base 'Bot::BasicBot';
    use File::Temp qw(tempfile);
    use Carp qw(confess);
    my $prefix  = '';
    my $postfix = ':';

    my %executer = (
#            echo    => \&exec_echo,
            kp6     => \&exec_kp6,
            pugs    => \&exec_pugs,
            perl6   => \&exec_eval,
            nqp     => \&exec_nqp,
            rakudo  => \&exec_p6,
            );
    my $regex = $prefix . '(' . join('|',  keys %executer) . ")$postfix";

    sub help {
        return "Usage: <$regex \$perl6_program>";
    }
#    warn "Regex: ", $regex, "\n";

    sub said {
        my $self = shift;
        my $e = shift;
        my $message = $e->{body};
        if ($message =~ m/\A$regex\s+(.*)\z/){
            my ($eval_name, $str) = ($1, $2);
            my $e = $executer{$eval_name};
            warn "Eval: $str\n";
            if ($eval_name eq 'kp6') {
                my $rev_string = 'r' . get_revision() . ': ';
                return $rev_string . EvalbotExecuter::run($str, $e);
            } elsif ($eval_name eq 'perl6'){
                my $pugs_out = EvalbotExecuter::run($str, $executer{pugs});
                my $kp6_out  = EvalbotExecuter::run($str, $executer{kp6});
                my $p6_out   = EvalbotExecuter::run($str, $executer{rakudo});
#                my $nqp_out  = EvalbotExecuter::run($str, $executer{nqp});
                return "kp6: $kp6_out\npugs: $pugs_out\nrakudo: $p6_out";
            } elsif ($eval_name eq 'rakudo' ){
                return filter_rakudo(EvalbotExecuter::run($str, $e));
            } else {
                return EvalbotExecuter::run($str, $e);
            }
        }
        return;
    }

    sub exec_echo {
        my ($program, $fh, $filename) = @_;
        print $fh $program;
    }

    sub exec_kp6 {
        my ($program, $fh, $filename) = @_;
        chdir('../../v6/v6-KindaPerl6/')
            or confess("Can't chdir to kp6 dir: $!");
        my ($tmp_fh, $name) = tempfile();
        print $tmp_fh $program;
        close $tmp_fh;
        system "perl script/kp6 --secure < $name >$filename 2>&1";
        unlink $name;
        chdir $FindBin::Bin;
        return;
    }

    sub exec_pugs {
        my ($program, $fh, $filename) = @_;
        chdir('../../')
            or confess("Can't chdir to pugs base dir: $!");
        my ($tmp_fh, $name) = tempfile();
        print $tmp_fh $program;
        close $tmp_fh;
        system "PUGS_SAFEMODE=true ./pugs $name >> $filename 2>&1";
        unlink $name;
        chdir $FindBin::Bin;
        return;
    }

    sub exec_nqp {
        my ($program, $fh, $filename) = @_;
        chdir('../../../parrot/')
            or confess("Can't chdir to parrot base dir: $!");
        my ($tmp_fh, $name) = tempfile();
        print $tmp_fh $program;
        close $tmp_fh;
        system "./parrot compilers/nqp/nqp.pbc $name >> $filename 2>&1";
        unlink $name;
        chdir $FindBin::Bin;
        return;
    }

    sub exec_p6 {
        my ($program, $fh, $filename) = @_;
        chdir('../../../parrot/')
            or confess("Can't chdir to parrot base dir: $!");
        my ($tmp_fh, $name) = tempfile();
        print $tmp_fh $program;
        close $tmp_fh;
        system "./parrot languages/perl6/perl6.pbc $name >> $filename 2>&1";
        unlink $name;
        chdir $FindBin::Bin;
        return;
    }


    sub exec_eval {
        # not really called, see the "dispatcher"
        return;
        my ($program, $fh, $filename) = @_;
        print $fh "pugs:[";
        exec_pugs(@_);
        print $fh "] kp6:[";
        exec_kp6(@_);
        print $fh "]";
        return;
    }

    sub get_revision {
        my $info = qx/svn info/;
        if ($info =~ m/^Revision:\s+(\d+)$/smx){
            return $1;
        } else {
            return "_unknown";
        }
    }

    sub filter_rakudo {
        my $str = shift;
        $str =~ s/\ncalled from Sub.*//s;
        return $str;

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
