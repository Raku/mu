package PugsBuild::MiniYAML;
use strict;
use warnings;
our $VERSION = '0.01';

sub new { bless {}, shift }

sub load {
    my($self, $stream) = @_;
    my $doc;
    for (split /\r?\n/, $stream) {
        s/#.*//;
        next unless /\S/;
        /^---$/ && do {
            die "sorry, multiple documents not supported\n" if $doc;
            $doc = {};
            next;
        };
        /^(\S+?) \s* : \s* \[(.*?) \s* \] \s* $/x && do { # hello, full YAML spec.
            my($key, $val) = ($1, $2);
            $doc->{$key} = [ map { /\s*(.*?)\s*$/; $1 } split /,/, $val ];
            next;
        };
        /^(\S+?) \s* : \s* (.*?) \s* $/x && do {
            my($key, $val) = ($1, $2);
            $val = 1 if $val =~ /y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON/;
            $val = 0 if $val =~ /n|N|no|No|NO|false|False|FALSE|off|Off|OFF/;
            $val = undef if $val eq "~";
            die "sorry, only scalars are supported: $key: $val\n" if
                $val =~ /^[\[{]/;
            die "Expected separator '---'" unless $doc;
            $doc->{$key} = $val;
            next;
        };
        /^\s/ && die "sorry, nesting not supported: $_\n";
        die "can't parse line: $_\n";
    }
    return $doc;
}

1;
