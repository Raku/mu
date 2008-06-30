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

Written by Moritz Lenz, many good contributions came from the #perl6 folks, 
notably diakopter, Mitchell Charity (putter), Adrian Kreher (Auzon), rhr, 
and all those that I forgot.

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
    use Scalar::Util qw(reftype);
    my $prefix  = '';
    my $postfix = ':';

    our %impls = (
            pixie => {
                chdir       => '../pixie/',
                cmd_line    => "$^X pixie --quiet \%program >> \%out 2>&1",
            },
            elf => {
                chdir       => '../elf',
                cmd_line    => './elf_f %program >> %out 2>&1',
                revision    => \&get_revision,
            },
            kp6 => {
                chdir       => '../../v6/v6-KindaPerl6/',
                cmd_line    => "$^X script/kp6 --secure < \%program >\%out 2>&1",
                revision    => \&get_revision,
                filter      => \&filter_kp6,
            },
            rakudo => {
                chdir       => '../../../parrot/',
                cmd_line    => './parrot languages/perl6/perl6.pbc %program >> %out 2>&1',
                revision    => \&get_rakudo_revision,
                filter      => \&filter_pct,
            },
            nqp   => {
                chdir       => '../../../parrot/',
                cmd_line    => './parrot compilers/nqp/nqp.pbc %program >> %out 2>&1',
                filter      => \&filter_pct,
            },
            pugs => {
                cmd_line    => 'PUGS_SAFEMODE=true /home/evalenv/.cabal/bin/pugs %program >> %out 2>&1',
            },
            yap6 => \&exec_yap6,
    );

    my $evalbot_version = get_revision();

    my $regex = $prefix . '(' . join('|',  keys %impls) . ")$postfix";

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
            my $e = $impls{$eval_name};
            warn "Eval: $str\n";
            my $result = EvalbotExecuter::run($str, $e, $eval_name);
            my $revision = '';
            if (reftype($e) eq 'HASH' && $e->{revision}){
                $revision = ' ' . $e->{revision}->();
            }
            return sprintf "%s%s: %s", $eval_name, $revision, $result;
        } elsif ( $message =~ m/\Aperl6:\s+(.+)\z/ ){
            my $str = $1;
            return "Program empty" unless length $str;
            warn "Perl6: $str\n";
            my $result = '';
            for my $eval_name qw(elf kp6 pugs rakudo){
                my $e = $impls{$eval_name};
                my $tmp_res = EvalbotExecuter::run($str, $e, $eval_name);
                my $revision = '';
                if (reftype($e) eq 'HASH' && $e->{revision}){
                    $revision = ' ' . $e->{revision}->();
                }
                $result .= sprintf "%s%s: %s\n", $eval_name, $revision, $tmp_res;
            }
            return $result;

        } elsif ( $message =~ m/\Aevalbot\s*control\s+(\w+)/) {
            my $command = $1;
            if ($command eq 'restart'){
                warn "Restarting $0 (by user request\n";
                # we do hope that evalbot is started in an endless loop ;-)
                exit;
            } elsif ($command eq 'version'){
                return "This is evalbot revision $evalbot_version";
            }

        }
        return;
    }

    sub exec_yap6 {
        my ($program, $fh, $filename) = @_;
        chdir('../yap6/src')
            or confess("Can't chdir to elf base dir: $!");
        my ($tmp_fh, $name) = tempfile();
        if ($program =~ m/\|\|\|/){
            confess "Disabled due to security concerns";
        }
        my ($gram,$inp) = split('\|\|\|',$program);
        my $is_custom = $inp?1:0;
        $inp ||= $gram;
        $gram = $is_custom?$gram:'STD_hand';
        print $tmp_fh $inp;
        close $tmp_fh;
        system "perl -Ilib bin/test $gram $name >> $filename 2>&1";
        unlink $name;
        chdir $FindBin::Bin;
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

    sub get_rakudo_revision {
        my $file = '/home/evalenv/parrot/languages/perl6/rakudo_svn_revision';
        open my $f, '<', $file or die "Can't open file '$file': $!";
        my $res = <$f>;
        close $f;
        chomp $res;
        return $res;
    }

    sub filter_pct {
        my $str = shift;
        $str =~ s/called from Sub.*//ms;
        return $str;
    }

    sub filter_kp6 {
        my $str = shift;
        $str =~ s/KindaPerl6::Runtime.*//ms;
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
