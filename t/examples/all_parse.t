use v6-alpha;
use File::Find;
use Test;

=pod

Test examples

This loads all the scripts of the examples/ dir and verifies they
can compile.  It does not verify run ability or verify output.

=cut

my $f = File::Find.new(
    wanted_file => sub ($file, $path, $pathfile) {
        return 1 if $file ~~ m:P5/^.*\.pl$/;
    },
    wanted_dir => sub ($dir, $path, $pathdir) {
        return 0 if $dir ~~ m:P5/^\.svn$/;
        return 1;
    },
    dirs => qw/examples/
);


# This should be removed ASAP
# Currently (2006-08-21) only way Win32 works
if $*OS eq any(<MSWin32 mingw msys cygwin>) {
    $f.debug = 1;
}

my @files = $f.find;

plan +@files;

if $*OS eq "browser" {
    skip_rest "Programs running in browsers don't have access to regular IO.";
    exit;
}

my $pugs = "./pugs";
if $*OS eq any(<MSWin32 mingw msys cygwin>) {
    $pugs = 'pugs.exe';  
};

# The following is ugly and should be rewritten
# Specifically, there should be a way to test
# $! instead of this yucky workaround
for @files -> $ex is rw {

    try {
        if $*OS eq any(<MSWin32 mingw msys cygwin>) {
            $ex ~~ s:g:P5/\\/\//;
        }
        my $cmd = "$pugs -c $ex";
        my $out = `$cmd`;

        if $out ~~ m:P5/syntax OK\s*$/ {
            is 'parse passed', 'parse passed', "$ex parsed correctly";
        }
        else {
            is 'parse failed', 'parse passed', "$ex fails to parse correctly";
        }
    }
}
