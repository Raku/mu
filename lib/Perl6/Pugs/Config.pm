package Perl6::Pugs::Config;
use strict;
use warnings;
our $VERSION = '0.01';

# change these two lines if it turns out we need the real YAML.pm
use Perl6::Pugs::Config::MiniYAML ();
our $YAML = Perl6::Pugs::Config::MiniYAML->new;

sub import {
    my($class, $filename) = @_;
    my $conf = $class->read($filename);
    my $caller = caller;
    no strict 'refs';
    *{$caller . '::BuildPrefs'} = $conf;
}

sub read {
    my($class, $filename) = @_;
    my $stream;
    open my $fh, ($filename ||= "config.yml") or die "open: $filename: $!";
    { local $/; $stream = <$fh> }
    my $conf = $YAML->load($stream);
}

1;
