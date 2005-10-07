module FindBin-6.0.0;

use v6;
use File::Spec;

my Str $Bin;
my Str $Dir;
my Str $Script;
my Str $RealBin;
my Str $RealDir;
my Str $RealScript;

our Str $FindBin::Bin        := $Bin;
our Str $FindBin::Dir        := $Dir;
our Str $FindBin::Script     := $Script;
our Str $FindBin::RealBin    := $RealBin;
our Str $FindBin::RealDir    := $RealDir;
our Str $FindBin::RealScript := $RealScript;

sub init {
    $Dir := $Bin;
    $RealDir := $RealBin;
    if ( $*PROGRAM_NAME eq '-e' || $*PROGRAM_NAME eq '-' ) {
        $Script = $RealScript = $*PROGRAM_NAME;
        $Bin    = $RealBin    = File::Spec.cwd;
    }
    else {
        my Str $script = $*PROGRAM_NAME;

        # XXX: Insert VMS support here

        my Int $dosish = ( $?OS eq 'MSWin32' or $?OS eq 'os2' );
        &readlink := { undef } if $dosish;

        unless ( ( $script ~~ m:P5#/# || ( $dosish && $script ~~ m:P5#\\# ) )
                 && -f $script )
        {
            for File::Spec.path() -> $dir {
                my Str $scr = catfile( $dir, $script );
                if -r $scr && ( !$dosish || -x _ ) {
                    $script = $scr;
                    if -f $*PROGRAM_NAME {
                        $script = $*PROGRAM_NAME unless -T $script;
                    }
                    last;
                }
            }
        }
        warn "Cannot find current script '$*PROGRAM_NAME'" unless -f $script;
        $script = catfile( File::Spec.cwd(), $script )
          unless file_name_is_absolute($script);
        my @path = splitpath($script);
        $Script = pop @path;
        $Bin = catdir(@path);
        loop {
            my $linktext = try { readlink($script) };
            my @path = splitpath($script);
            $RealScript = pop @path;
            $RealBin = catdir(@path);
            last unless defined $linktext;
            $script = file_name_is_absolute($linktext)
              ?? $linktext
              !! catfile( $RealBin, $linktext );
        }
        $Bin     = rel2abs($Bin)     if $Bin;
        $RealBin = rel2abs($RealBin) if $RealBin;
    }
}

BEGIN { init() }

=head1 NAME

FindBin - Locate directory of original perl script

=head1 SYNOPSIS

    use FindBin;
    use lib "$FindBin::Bin/../lib";

=head1 DESCRIPTION

Perl 6 port of the C<FindBin> library.

    $Bin         - path to bin directory from where script was invoked
    $Script      - basename of script from which perl was invoked
    $RealBin     - $Bin with all links resolved
    $RealScript  - $Script with all links resolved

=head1 AUTHOR

Sebastian Riedel <sri@oook.de>

Based upon C<FindBin> by Graham Barr and Nick Ing-Simmons

=head1 LICENSE

This library is free software . You can redistribute it and/or modify
it under the same terms as perl itself.

=cut
