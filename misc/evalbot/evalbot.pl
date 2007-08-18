#!/usr/bin/perl
use warnings;
use strict;

use Config::File;
use Bot::BasicBot;
use EvalbotExecuter;


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
#        system 'perl', 'kp6-perl5.pl', 
    }

}

package main;

my $bot = Evalbot->new(
        server => 'irc.freenode.org',
#        server => 'irc.perl.org',
        port   => 6667,
#        channels => ['#perl6', '#perl6de'],
#        channels => ['#perl6de'],
        channels  => ['#perl6'],
        nick      => 'exp_evalbot',
        alt_nicks => ["comb_evalbot", "evalbot_"],
        username  => "evalbot",
        name      => "combined, experimental evalbot",
        charset   => "utf-8", 
        );
$bot->run();


# vim: ts=4 sw=4 expandtab
