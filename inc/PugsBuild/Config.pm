package PugsBuild::Config;
use strict;
use warnings;
our $VERSION = '0.01';

use File::Copy;

# change these two lines if it turns out we need the real YAML.pm
use PugsBuild::MiniYAML ();
our $YAML = PugsBuild::MiniYAML->new;
our $Conf;

our %DEFAULTS = (
    optimization          => '-O',
    smoke_concurrent      => 1,
    inline_prelude_source => 0,
    precompile_prelude    => 1,
    precompile_modules    => [],
    ghc_heap_size         => '',       # use GHC's default
);

sub import {
    my($class, $filename) = @_;
    $Conf = $class->read($filename);
    my $caller = caller;
    no strict 'refs';
    *{$caller . '::BuildPrefs'} = $Conf;
}

sub pretty_print {
    # ah yes so THAT's why we wanted YAML.pm.
    join "", map { ref $Conf->{$_} ?
            "\t$_ => [". (join ", ", @{ $Conf->{$_} }) ."]\n" :
            "\t$_ => $Conf->{$_}\n" }
        sort keys %$Conf;
}

sub read {
    my($class, $filename) = @_;
    my $stream;
    open my $fh, ($filename ||= "config.yml") or die "open: $filename: $!";
    { local $/; $stream = <$fh> }
    my $conf = $YAML->load($stream);
    $class->env_override($conf);
    $class->defaults($conf);
    return $conf;
}

sub env_override {
    my($class, $conf) = @_;
    my %env = map { split /=/ } (split /:/, ($ENV{PUGS_BUILD_OPTS} || ''));
    $conf->{$_} = $env{$_} for keys %env;
}

sub defaults {
    my($class, $conf) = @_;
    do { $conf->{$_} = $DEFAULTS{$_} unless defined $conf->{$_} } for
        keys %DEFAULTS;
}

sub lookup {
    my($class, $what) = @_;
    my $value = $Conf->{$what};
    die "unknown option: $what" unless defined $value;
    return $value;
}

1;
