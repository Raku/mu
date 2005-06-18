module Module::Pluggable::Fast-6.0.0;

use File::Spec;
use File::Find;

has Array $.search;
has Int   $.require;
has Code  $.callback;

submethod BUILD {
    # XXX: $.search should default to caller namespace
    $.callback = sub ($class) {
        my $obj = $class;
        try {
            $obj = $class.new();
            CATCH { warn qq/Couldn't instantiate "$class", "$!"/ };
        };
        return $obj;
    };
    $.require ||= 0;
}

method plugins {
    my %plugins;
    for @*INC -> $dir {
        # XXX: add blib support
        for $.search -> $search {
           for ./find_packages($dir) -> $package {
               my $rel = abs2rel( $package, $dir );
               my $class = splitdir($rel).join('::');
               $class ~~ s:P5/\.pm$//;
               next unless $class ~~ m:P5/^$search(\:\:|$){1}/;
               try {
                   $class.require;
                   CATCH { die qq/Couldn't require "$class", "$!"/ };
               };
               unless %plugins{$class} {
                   %plugins{$class} =
                       $.require ?? $class :: $.callback.($class);
               }
               # XXX: add advanced class detection
           }
        }
    }
    return %plugins.values;
}

method find_packages ( Str $path ) {
    return () unless -e $path;
    my $f = File::Find.new;
    $f.dirs = ($path);
    $f.wanted_file = sub ( $file, $path, $pathfile ) {
        return 1 if $file ~~ m:P5/\.pm$/;
        return 0;
    };
    return $f.find;
}

=head1 NAME

Module::Pluggable::Fast - Fast plugins

=head1 SYNOPSIS

    use Module::Pluggable::Fast;

    my $p = Module::Pluggable::Fast.new;
    $p.search = qw/MyApp::Plugins MyApp::MorePlugins/;
    my @plugins = $p.plugins;

=head1 DESCRIPTION

Perl6 port of the C<Module::Pluggable::Fast> library.

=head1 AUTHOR

Sebastian Riedel <sri@oook.de>

=head1 LICENSE

This library is free software . You can redistribute it and/or modify
it under the same terms as perl itself.

=cut
