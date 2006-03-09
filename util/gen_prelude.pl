#!/usr/bin/perl -w

use strict;
use IPC::Open2;
use Getopt::Long;
use Config ();

# helper code to inline the Standard Prelude in pugs.

# Sets up either the inlined Perl 6 source Prelude fallback, or a real
# precompiled AST of Prelude.pm.

our %Config;
our $TEMP_PRELUDE = "Prelude.pm"; # XXX: move this to config.yml?
END { unlink $TEMP_PRELUDE unless $Config{keep} };

GetOptions \%Config, qw(--inline --pugs|p=s --precompile|i=s@ --verbose|v --touch --output|o=s --keep|k);

touch() if $Config{touch};

setup_output();

inline(), exit 0 if $Config{inline};
precomp(), exit 0 if $Config{precompile};
usage();
exit 1;

sub setup_output {
    if ($Config{output}) {
        open OUT, "> $Config{output}" or
            die "open: $Config{output}: $!";
    } else {
        *OUT = *STDOUT;
    }
    binmode OUT;
}

# XXX: with yaml precompilation, this may be bogus.
sub touch {
    # XXX: *ugly* hack! ghc doesn't spot that the include file was changed,
    #      so we need to mark as stale some obj files to trigger a rebuild.
    #      The alternative seems to be to delete them *and* the pugs
    #      executable.
    print STDERR "Triggering rebuild... " if $Config{verbose};
    unlink "blib6/lib/Prelude.pm.yml";
    unlink "blib6/lib/Prelude.pm.yml.pm";
    #unlink "src/Pugs/PreludePC.hs";
    #unlink "src/Pugs/Run.hi";
    #unlink "src/Pugs/Run.o";
    #unlink "dist/build/Pugs/Run.hi";
    #unlink "dist/build/Pugs/Run.o";
    #unlink "dist/build/src/Pugs/Run.hi";
    #unlink "dist/build/src/Pugs/Run.o";
    #unlink "pugs$Config::Config{_exe}";
    print STDERR "done.\n" if $Config{verbose};
}

sub inline {
    print STDERR "Generating inlined source Prelude... " if $Config{verbose};

    gen_source($TEMP_PRELUDE);
    open IN, $TEMP_PRELUDE or
        die "Couldn't open temp prelude ($TEMP_PRELUDE): $!"; 
    my $program = do { local $/; <IN> };
    close IN;

    strip_comments($program);
    $program =~ s{(["\\])}{\\$1}g;
    $program =~ s{\r?\n}{\\n\\\n\\}g;

    print OUT <<'.';
module Pugs.Prelude where

{-
    Prelude bootstap. 

>   The world was young, the mountains green,
>   No stain yet on the Moon was seen,
>   No words were laid on stream or stone,
>   When Durin woke and walked alone.

-}

----------------------------------------------------------------
-- Do not modify this file; it is generated automatically by  --
--                  util/gen_prelude.pl                       --
----------------------------------------------------------------

preludeStr :: String
.
    print OUT qq<preludeStr = "$program"\n\n>;
    close OUT;

    print STDERR "done.\n" if $Config{verbose};
}

# concatenate source files. hardcode special treatment to the Prelude,
# which is assumed to be the first module in the list.
sub gen_source {
    my($target) = @_;
    open my $ofh, ">", $target or die "open: $target: $!";

    {
        my $prelude = shift @{ $Config{precompile} };
        warn "*** warning: Prelude.pm should probably be the first --include\n"
            unless $prelude =~ /Prelude/;
        open my $ifh, $prelude or die "open: $prelude: $!";
        print $ofh $_ while <$ifh>;
    }

    # manhandle the rest of the inlined modules.
    # we make a guess about what to put in %*INC. it's not perfect.
    # When module return values are specced, we can make this much
    # less hacky :-)
    for my $file (@{ $Config{precompile} }) {
        my $module; # guess what to put in %*INC
        open my $ifh, $file or die "open: $file: $!";
        
        print $ofh "\n{\n";
        my $program;
        while (<$ifh>) {
            $module ||= $1 if /^(?:package|module|class) \s+ ([^-;]+)/x;
            $program .= $_;
        }

        die "could not guess module name: $file" unless $module;

        strip_comments($program);
        print $ofh $program;

        print STDERR ", $module" if $Config{verbose};
        $module =~ s#::#/#g;
        print $ofh "\n};\n%*INC<${module}.pm> = '<precompiled>';\n\n";
        # (the need for a semicolon in "};" is probably a bug.)
    }
    print STDERR "... " if $Config{verbose};
}

# Strip comments and docs while preserving the line counts
sub strip_comments {
    $_[0] =~ s{^[ \t]*#.*}{}mg;
    $_[0] =~ s{^=\w(.*?)^=cut$}{"\n" x ($1 =~ y/\n//)}mesg;
}

sub precomp {
    print STDERR "Generating precompiled Prelude" if $Config{verbose};
    die "*** Error: $0 needs an already compiled Pugs to precompile the Prelude\n"
        unless $Config{pugs};
    gen_source($TEMP_PRELUDE);
    $ENV{PUGS_COMPILE_PRELUDE} = 1;

    my ($rh, $wh, $lines);
    my $pid = open2($rh, $wh, $Config{pugs}, -C => 'Parse-YAML', $TEMP_PRELUDE);
    $lines += print OUT while <$rh>;
    #my $program = do { local $/; <$rh> };
    waitpid($pid, 0);

    exit 1 unless length $lines;

    die "Pugs ".(($?&255)?"killed by signal $?"
         :"exited with error code ".($?>>8)) if $?;
    print STDERR "done.\n" if $Config{verbose};
}

sub usage {
    print STDERR <<".";
usage: $0 --inline src/perl6/Prelude.pm [options]
       $0 --precompile src/perl6/Prelude.pm --pugs ./pugs.exe [options]

Creates a Prelude.hs fallback or a Prelude.pm.yml file (written to stdout),
to be loaded by Run.hs.

When pugs is built, a fallback Prelude.hs that contains only a quoted
version of the Prelude code is inlined into the executable, to be
"eval"ed when pugs starts.  After the executable is ready, the Standard
Prelude is precompiled and stored in YAML format in a (conjecturally)
well-defined location for latter runs of pugs to pick up and load quickly.

Additional options:
    --verbose, -v     print progress to stderr
    --touch,   -t     mark Run.hi and Run.o stale, triggering pugs rebuild
    --output,  -o     file to write output to (stdout by default)
.
}
