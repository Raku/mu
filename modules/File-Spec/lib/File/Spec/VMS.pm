#!pugs
use v6;

require File::Spec::Unix-0.0.1;

class File::Spec::VMS-0.0.1 is File::Spec::Unix;

## XXX need to address these dependencies
# use File::Basename;
# use VMS::Filespec;

# make these public (read-only by default) 
# attributes, and they will auto-generate 
# their own accessors for us to use
has Str  $.curdir        = '[]';
has Str  $.updir         = '[-]'; 
has Str  $.rootdir       = 'SYS$DISK:[000000]';
has Str  $.devnull       = '_NLA0:';
has Bool $.case_tolerant = 1;

method eliminate_macros (Str $path) returns Str {
    return '' unless $path;
    my %stash;
    if ($path ~~ /\s/) {
      return $path.split(/\s+/).map:{ .eliminate_macros($_) }.join(' ');
    }
    my ($npath) = .unixify($path);
    my ($complex) = 0;
    my ($head, $macro, $tail);
    # perform m##g in scalar context so it acts as an iterator
    while ($npath ~~ m#(.*?)\$\((\S+?)\)(.*)#gs) { 
        if (%stash{$2}) {
            ($head, $macro, $tail) = ($1, $2, $3);
            if (ref(%stash{$macro}) eq 'Str') {
                if (ref(%stash{$macro}) eq 'Array') {
                    $macro = %stash{$macro}.join(' ');
                }
                else {
                    print "Note: can't expand macro \$($macro) containing ",ref(%stash{$macro}),
                          "\n\t(using MMK-specific deferred substitutuon; MMS will break)\n";
                    $macro = "\cB$macro\cB";
                    $complex = 1;
                }
            }
            else { 
                ($macro = .unixify(%stash{$macro})) ~~ s#/\Z(?!\n)##; 
            }
            $npath = "$head$macro$tail";
        }
    }
    if ($complex) { 
        $npath ~~ s#\cB(.*?)\cB#\${$1}#gs; 
    }
    $npath;
}


method fixpath (Str $path, Bool $force_path) returns Str  {
    return '' unless $path;
    my %stash;
    my($fixedpath, $prefix, $name);
    if ($path ~~ /\s/) {
        return $path.split(/\s+/).map:{ .fixpath($_, $force_path) }.join(' ');
    }

    if ($path ~~ m#^\$\([^\)]+\)\Z(?!\n)#s || $path =~ m#[/:>\]]#) { 
        if ($force_path or $path ~~ /(?:DIR\)|\])\Z(?!\n)/) {
            $fixedpath = .vmspath(.eliminate_macros($path));
        }
        else {
            $fixedpath = .vmsify(.eliminate_macros($path));
        }
    }
    elsif ((($prefix, $name) = ($path ~~ m#^\$\(([^\)]+)\)(.+)#s)) && $self->{$prefix}) {
        my($vmspre) = .eliminate_macros("\$($prefix)");
        # is it a dir or just a name?
        $vmspre = ($vmspre ~~ m|/| or $prefix ~~ /DIR\Z(?!\n)/) ?? vmspath($vmspre) :: '';
        $fixedpath = ($vmspre ?? $vmspre :: $self->{$prefix}) ~ $name;
        $fixedpath = .vmspath($fixedpath) if $force_path;
    }
    else {
        $fixedpath = $path;
        $fixedpath = .vmspath($fixedpath) if $force_path;
    }
    # No hints, so we try to guess
    if (!$force_path.defined and !($fixedpath ~~ /[:>(.\]]/)) {
        $fixedpath = .vmspath($fixedpath) if -d $fixedpath;
    }

    # Trim off root dirname if it's had other dirs inserted in front of it.
    $fixedpath ~~ s/\.000000([\]>])/$1/;
    # Special case for VMS absolute directory specs: these will have had device
    # prepended during trip through Unix syntax in eliminate_macros(), since
    # Unix syntax has no way to express "absolute from the top of this device's
    # directory tree".
    if ($path ~~ /^[\[>][^.\-]/) { 
        $fixedpath ~~ s/^[^\[<]+//; 
    }
    $fixedpath;
}


method canonpath (Str $path) returns Str {
    if ($path ~~ m|/|) { # Fake Unix
        my $pathify = $path ~~ m|/\Z(?!\n)|;
        $path = .SUPER::canonpath($path);
        if ($pathify) { 
            return .vmspath($path); 
        }
        else { 
            return .vmsify($path);  
        }
    }
    else {
        $path ~~ tr/<>/[]/;			# < and >       ==> [ and ]
        $path ~~ s/\]\[\./\.\]\[/g;		# ][.		==> .][
        $path ~~ s/\[000000\.\]\[/\[/g;		# [000000.][	==> [
        $path ~~ s/\[000000\./\[/g;		# [000000.	==> [
        $path ~~ s/\.\]\[000000\]/\]/g;		# .][000000]	==> ]
        $path ~~ s/\.\]\[/\./g;			# foo.][bar     ==> foo.bar
        1 while ($path ~~ s/([\[\.])(-+)\.(-+)([\.\]])/$1$2$3$4/);
                            # That loop does the following
                            # with any amount of dashes:
                            # .-.-.		==> .--.
                            # [-.-.		==> [--.
                            # .-.-]		==> .--]
                            # [-.-]		==> [--]
        1 while ($path ~~ s/([\[\.])[^\]\.]+\.-(-+)([\]\.])/$1$2$3/);
                            # That loop does the following
                            # with any amount (minimum 2)
                            # of dashes:
                            # .foo.--.	==> .-.
                            # .foo.--]	==> .-]
                            # [foo.--.	==> [-.
                            # [foo.--]	==> [-]
                            #
                            # And then, the remaining cases
        $path ~~ s/\[\.-/[-/;			# [.-		==> [-
        $path ~~ s/\.[^\]\.]+\.-\./\./g;	# .foo.-.	==> .
        $path ~~ s/\[[^\]\.]+\.-\./\[/g;	# [foo.-.	==> [
        $path ~~ s/\.[^\]\.]+\.-\]/\]/g;	# .foo.-]	==> ]
        $path ~~ s/\[[^\]\.]+\.-\]/\[\]/g;	# [foo.-]	==> []
        $path ~~ s/\[\]//;			# []		==>
        return $path;
    }
}

methos catdir (*@dirs) returns Str {
    my $dir = @dirs.pop ;
    @dirs = @dirs.grep($_);
    my $rslt;
    if (@dirs) {
        my $path = (@dirs == 1 ?? @dirs[0] :: .catdir(@dirs));
        my ($spath, $sdir) = ($path, $dir);
        $spath ~~ s/\.dir\Z(?!\n)//; 
        $sdir ~~ s/\.dir\Z(?!\n)//; 
        $sdir = .eliminate_macros($sdir) unless $sdir ~~ /^[\w\-]+\Z(?!\n)/s;
        $rslt = .fixpath(.eliminate_macros($spath) ~ "/$sdir", 1);

        # Special case for VMS absolute directory specs: these will have had device
        # prepended during trip through Unix syntax in eliminate_macros(), since
        # Unix syntax has no way to express "absolute from the top of this device's
        # directory tree".
        if ($spath ~~ /^[\[<][^.\-]/s) { 
            $rslt ~~ s/^[^\[<]+//s; 
        }
    }
    else {
        if (!$dir.defined || !$dir.bytes) { 
            $rslt = ''; 
        }
        elsif ($dir ~~ /^\$\([^\)]+\)\Z(?!\n)/s) { 
            $rslt = $dir; 
        }
        else { 
            $rslt = .vmspath($dir); 
        }
    }
    return .canonpath($rslt);
}

method catfile (*@files) returns Str {
    my $file = .canonpath(@files.pop);
    @files = @files.grep($_);
    my $rslt;
    if (@files) {
        my $path = (@files == 1 ?? @files[0] :: .catdir(@files));
        my $spath = $path;
        $spath ~~ s/\.dir\Z(?!\n)//;
        if ($spath ~~ /^[^\)\]\/:>]+\)\Z(?!\n)/s && .basename($file) eq $file) {
            $rslt = "$spath$file";
        }
        else {
            $rslt = .eliminate_macros($spath);
            $rslt = .vmsify($rslt ~ ($rslt ?? '/' :: '') ~ unixify($file));
        }
    }
    else { 
        $rslt = ($file.defined && $file.bytes) ?? .vmsify($file) :: ''; 
    }
    return .canonpath($rslt);
}

my $tmpdir;
method tmpdir () returns Str {
    return $tmpdir if $tmpdir.defined;
    $tmpdir = ._tmpdir('sys$scratch:', %*ENV{'TMPDIR'});
    return $tmpdir;
}

method path () returns Array {
    my (@dirs, $dir, $i);
    while ($dir = %*ENV{'DCL$PATH;' ~ $i++}) { 
        @dirs.push($dir); 
    }
    return @dirs;
}

method file_name_is_absolute (Str $file) returns Bool {
    # If it's a logical name, expand it.
    $file = %*ENV{$file} while $file ~~ /^[\w\$\-]+\Z(?!\n)/s && %*ENV{$file};
    return ?(
            $file ~~ m!^/!s             ||
            $file ~~ m![<\[][^.\-\]>]!  ||
            $file ~~ /:[^<\[]/
            );
}

method splitpath (Str $path) returns Array {
    my($dev, $dir, $file) = ('', '', '');
    .vmsify($path) ~~ /(.+:)?([\[<].*[\]>])?(.*)/s;
    return ($1 || '', $2 || '', $3);
}

method splitdir (Str $dirspec) returns Array {
    $dirspec ~~ tr/<>/[]/;			# < and >	==> [ and ]
    $dirspec ~~ s/\]\[\./\.\]\[/g;		# ][.		==> .][
    $dirspec ~~ s/\[000000\.\]\[/\[/g;		# [000000.][	==> [
    $dirspec ~~ s/\[000000\./\[/g;		# [000000.	==> [
    $dirspec ~~ s/\.\]\[000000\]/\]/g;		# .][000000]	==> ]
    $dirspec ~~ s/\.\]\[/\./g;			# foo.][bar	==> foo.bar
    1 while ($dirspec ~~ s/(^|[\[\<\.])\-(\-+)($|[\]\>\.])/$1-.$2$3/g);
						# That loop does the following
						# with any amount of dashes:
						# .--.		==> .-.-.
						# [--.		==> [-.-.
						# .--]		==> .-.-]
						# [--]		==> [-.-]
    $dirspec = "[$dirspec]" unless $dirspec ~~ /[\[<]/; # make legal
    my (@dirs) = .vmspath($dirspec).split('\.');
    @dirs[0]  ~~ s/^[\[<]//s;  
    @dirs[-1] ~~ s/[\]>]\Z(?!\n)//s;
    return @dirs;
}

method catpath (Str $dev, Str $dir, Str $file) returns Str {
    # We look for a volume in $dev, then in $dir, but not both
    my ($dir_volume, $dir_dir, $dir_file) = .splitpath($dir);
    $dev = $dir_volume unless $dev.bytes;
    $dir = $dir_file.bytes ?? .catfile($dir_dir, $dir_file) :: $dir_dir;
    
    if ($dev ~~ m|^/+([^/]+)|) { 
        $dev = "$1:"; 
    }
    else { 
        $dev ~= ':' unless $dev eq '' || $dev ~~ /:\Z(?!\n)/; 
    }
    if ($dev.bytes || $dir.bytes) {
        $dir = "[$dir]" unless $dir ~~ /[\[<\/]/;
        $dir = .vmspath($dir);
    }
    return "$dev$dir$file";
}

method abs2rel (*@args) returns Str {
    return .vmspath(.SUPER::abs2rel(@args)) if @args.grep(m{/});
    my($path, $base) = @args;
    $base = ._cwd() unless $base.defined && $base.bytes;
    for ($path, $base) { $_ = .canonpath($_) }
    # Are we even starting $path on the same (node::)device as $base?  Note that
    # logical paths or nodename differences may be on the "same device" 
    # but the comparison that ignores device differences so as to concatenate 
    # [---] up directory specs is not even a good idea in cases where there is 
    # a logical path difference between $path and $base nodename and/or device.
    # Hence we fall back to returning the absolute $path spec
    # if there is a case blind device (or node) difference of any sort
    # and we do not even try to call $parse() or consult %ENV for $trnlnm()
    # (this module needs to run on non VMS platforms after all).
    my ($path_volume, $path_directories, $path_file) = .splitpath($path);
    my ($base_volume, $base_directories, $base_file) = .splitpath($base);
    return $path unless $path_volume.lc eq $base_volume.lc;
    for ($path, $base) { $_ = .rel2abs($_) }
    # Now, remove all leading components that are the same
    my @pathchunks = .splitdir($path_directories);
    @pathchunks.unshift('000000') unless @pathchunks[0] eq '000000';
    my @basechunks = .splitdir($base_directories);
    @basechunks.unshift('000000') unless @basechunks[0] eq '000000';
    while ( @pathchunks && 
            @basechunks && 
            @pathchunks[0].lc eq @basechunks[0].lc
          ) {
        @pathchunks.shift;
        @basechunks.shift;
    }
    # @basechunks now contains the directories to climb out of,
    # @pathchunks now has the directories to descend in to.
    $path_directories = ('-' x @basechunks, @pathchunks).join('.');
    return .canonpath(.catpath('', $path_directories, $path_file));
}

methods rel2abs (Str $path, Str $base) returns Str {
    return undef unless $path.defined;
    if ($path ~~ m/\//) {
        $path = (-d $path || $path ~~ m/\/\z/	# educated guessing about
                    ?? .vmspath($path) 			# whether it's a directory
                    :: .vmsify($path));
    }
    $base = .vmspath($base) if $base.defined && $base ~~ m/\//;
    # Clean up and split up $path
    if (!.file_name_is_absolute($path)) {
        # Figure out the effective $base and clean it up.
        if (!$base.defined || $base eq '') {
            $base = ._cwd;
        }
        elsif (!.file_name_is_absolute($base)) {
            $base = .rel2abs($base);
        }
        else {
            $base = .canonpath($base);
        }

        # Split up paths
        my ($path_directories, $path_file) = (.splitpath($path))[1,2];

        my ($base_volume, $base_directories) = .splitpath($base);

        $path_directories = '' if $path_directories eq '[]' ||
                                  $path_directories eq '<>';
        my $sep = '';
        $sep = '.'
            if ($base_directories ~~ m{[^.\]>]\Z(?!\n)} &&
                $path_directories ~~ m{^[^.\[<]}s);
        $base_directories = "$base_directories$sep$path_directories";
        $base_directories ~~ s{\.?[\]>][\[<]\.?}{.};

        $path = .catpath($base_volume, $base_directories, $path_file);
    }
    return .canonpath($path);
}

1;

__END__

=head1 NAME

File::Spec::VMS - methods for VMS file specs

=head1 SYNOPSIS

 require File::Spec::VMS; # Done internally by File::Spec if needed

=head1 DESCRIPTION

See File::Spec::Unix for a documentation of the methods provided
there. This package overrides the implementation of these methods, not
the semantics.

=over 4

=item eliminate_macros

Expands MM[KS]/Make macros in a text string, using the contents of
identically named elements of C<%$self>, and returns the result
as a file specification in Unix syntax.

=item fixpath

Catchall routine to clean up problem MM[SK]/Make macros.  Expands macros
in any directory specification, in order to avoid juxtaposing two
VMS-syntax directories when MM[SK] is run.  Also expands expressions which
are all macro, so that we can tell how long the expansion is, and avoid
overrunning DCL's command buffer when MM[KS] is running.

If optional second argument has a TRUE value, then the return string is
a VMS-syntax directory specification, if it is FALSE, the return string
is a VMS-syntax file specification, and if it is not specified, fixpath()
checks to see whether it matches the name of a directory in the current
default directory, and returns a directory or file specification accordingly.

=back

=head2 Methods always loaded

=over 4

=item canonpath (override)

Removes redundant portions of file specifications according to VMS syntax.

=item catdir

Concatenates a list of file specifications, and returns the result as a
VMS-syntax directory specification.  No check is made for "impossible"
cases (e.g. elements other than the first being absolute filespecs).

=item catfile

Concatenates a list of file specifications, and returns the result as a
VMS-syntax file specification.

=item curdir (override)

Returns a string representation of the current directory: '[]'

=item devnull (override)

Returns a string representation of the null device: '_NLA0:'

=item rootdir (override)

Returns a string representation of the root directory: 'SYS$DISK:[000000]'

=item tmpdir (override)

Returns a string representation of the first writable directory
from the following list or '' if none are writable:

    sys$scratch:
    $ENV{TMPDIR}

Since perl 5.8.0, if running under taint mode, and if $ENV{TMPDIR}
is tainted, it is not used.

=item updir (override)

Returns a string representation of the parent directory: '[-]'

=item case_tolerant (override)

VMS file specification syntax is case-tolerant.

=item path (override)

Translate logical name DCL$PATH as a searchlist, rather than trying
to C<split> string value of C<$ENV{'PATH'}>.

=item file_name_is_absolute (override)

Checks for VMS directory spec as well as Unix separators.

=item splitpath (override)

Splits using VMS syntax.

=item splitdir (override)

Split dirspec using VMS syntax.

=item catpath (override)

Construct a complete filespec using VMS syntax

=item abs2rel (override)

Use VMS syntax when converting filespecs.

=item rel2abs (override)

Use VMS syntax when converting filespecs.

=back

=head1 SEE ALSO

See L<File::Spec> and L<File::Spec::Unix>.  This package overrides the
implementation of these methods, not the semantics.

An explanation of VMS file specs can be found at
L<"http://h71000.www7.hp.com/doc/731FINAL/4506/4506pro_014.html#apps_locating_naming_files">.

=cut
