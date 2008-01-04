#!/usr/bin/perl
use strict;
use warnings;

use WWW::Mechanize;
use Getopt::Long qw(GetOptions);

my %opts = (site => 'sial'); # the other site was not tested yet

usage() if not @ARGV;
GetOptions(\%opts, 
    "file=s", 
    "username=s",
    "title=s",
    "help",
) or usage();
usage() if $opts{help};
usage("Required: --file FILENAME\n") if not $opts{file};

# TODO enforce other fields?


{
    open my $fh, '<', $opts{file}
        or usage("Could not open file '$opts{file}' $!");
    local $/ = undef;
    $opts{text} = <$fh>;
}

my $w = WWW::Mechanize->new();

if ($opts{site} eq 'sial') {
    my $url = 'http://sial.org/pbot/perl6';
    $w->get($url);
    $w->submit_form(
        fields => {
            channel => '#perl6',
            nick    => $opts{username},
            summary => $opts{title},
            paste   => $opts{text},
        }
    );
} elsif ($opts{site} eq 'lisp') {
    # not working yet
    my $url = 'http://paste.lisp.org/new/perl6';
    $w->get($url);
    $w->submit_form(
        fields => {
            captcha     => 'lisp',
            username    => $opts{username},
            title       => $opts{title},
            text        => $opts{text},
        },
    );
} else {
    die "Unknown site '$opts{site}'\n";
}

sub usage {
    my $msg = shift || '';
    
    print <<"END_USAGE";
$msg
Usage: $0
        --file FILENAME
        --username USERNAME
        --title TITLE

        
        --help
END_USAGE

    exit;
}

