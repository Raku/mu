#!pugs
use v6;

require File::Spec::Unix-0.0.1;

class File::Spec::Win32-0.0.1 File::Spec::Unix;

# make these public (read-only by default) 
# attributes, and they will auto-generate 
# their own accessors for us to use
has Str  $.devnull       = 'nul';
has Bool $.case_tolerant = 1;

method file_name_is_absolute (Str $file) returns Bool { ?($file ~~ m{^([a-z]:)?[\\/]}is) }

my $tmpdir;
method tmpdir () returns Str {
    return $tmpdir if $tmpdir.defined;
    $tmpdir = ._tmpdir( %*ENV{'TMPDIR', 'TEMP', 'TMP'}, 'SYS:/temp', 'C:/temp', '/tmp', '/');
    return $tmpdir;
}

method catfile (*@path) returns Str {
    my $file = .canonpath(@path.pop);
    return $file unless @path;
    my $dir = .catdir(@path);
    $dir ~= "\\" unless substr($dir,-1) eq "\\";
    return "$dir$file";
}

method catdir (*@path) returns Str {
    for (@path) {
        tr[/][\\];
        # append a backslash to each argument unless it has one there
        $_ ~= "\\" unless m{\\$};
    }
    return .canonpath(@path.join(''));
}

method path () returns Array {
    my $path = $ENV{'PATH'} || $ENV{'Path'} || $ENV{'path'};
    my @path = $path.split(';');
    for (@path) { $_ = '.' if $_ eq '' }
    return @path;
}

method canonpath (Str $path) returns Str {
    my $orig_path = $path;
    $path ~~ s/^([a-z]:)/\u$1/s;
    $path ~~ s|/|\\|g;
    $path ~~ s|([^\\])\\+|$1\\|g;                  # xx\\\\xx  -> xx\xx
    $path ~~ s|(\\\.)+\\|\\|g;                     # xx\.\.\xx -> xx\xx
    $path ~~ s|^(\.\\)+||s unless $path eq ".\\";  # .\xx      -> xx
    $path ~~ s|\\\Z(?!\n)||
	unless $path ~~ m{^([A-Z]:)?\\\Z(?!\n)}s;  # xx\       -> xx
    # xx1/xx2/xx3/../../xx -> xx1/xx
    $path ~~ s|\\\.\.\.\\|\\\.\.\\\.\.\\|g; # \...\ is 2 levels up
    $path ~~ s|^\.\.\.\\|\.\.\\\.\.\\|g;    # ...\ is 2 levels up
    return $path if $path ~~ m|^\.\.|;      # skip relative paths
    return $path unless $path ~~ /\.\./;    # too few .'s to cleanup
    return $path if $path ~~ /\.\.\.\./;    # too many .'s to cleanup
    $path ~~ s{^\\\.\.$}{\\};                      # \..    -> \
    1 while $path ~~ s{^\\\.\.}{};                 # \..\xx -> \xx

    my ($vol, $dirs, $file) = .splitpath($path);
    my @dirs = .splitdir($dirs);
    my (@base_dirs, @path_dirs);
    my $dest = \@base_dirs;
    for (@dirs) -> $dir {
        $dest = \@path_dirs if $dir eq .updir();
        $dest.push($dir);
    }
    # for each .. in @path_dirs pop one item from 
    # @base_dirs
    while (my $dir = @path_dirs.shift){ 
        unless ($dir eq .updir){
            @path_dirs.unshift($dir);
            last;
        }
        @base_dirs.pop;
    }
    $path = .catpath($vol, .catdir(@base_dirs, @path_dirs), $file);
    return $path;
}

method splitpath (Str $path, Bool $nofile) returns Str {
    my ($volume, $directory, $file) = ('','','');
    if ($nofile) {
        $path ~~ 
            m{^( (?:[a-zA-Z]:|(?:\\\\|//)[^\\/]+[\\/][^\\/]+)? ) 
                 (.*)
             }xs;
        $volume    = $1;
        $directory = $2;
    }
    else {
        $path ~~ 
            m{^ ( (?: [a-zA-Z]: |
                      (?:\\\\|//)[^\\/]+[\\/][^\\/]+
                  )?
                )
                ( (?:.*[\\/](?:\.\.?\Z(?!\n))?)? )
                (.*)
             }xs;
        $volume    = $1;
        $directory = $2;
        $file      = $3;
    }
    return ($volume, $directory, $file);
}


method splitdir (Str $directories) returns Array {
    #
    # split() likes to forget about trailing null fields, so here we
    # check to be sure that there will not be any before handling the
    # simple case.
    #
    if (!($directories ~~ m|[\\/]\Z(?!\n)|)) {
        return $directories.split(m|[\\/]|);
    }
    else {
        #
        # since there was a trailing separator, add a file name to the end, 
        # then do the split, then replace it with ''.
        #
        my(@directories) = "${directories}dummy".split( m|[\\/]|);
        @directories[+@directories] = '';
        return @directories;
    }
}

method catpath (Str $volume, Str $directory, Str $file) returns Str {
    # If it's UNC, make sure the glue separator is there, reusing
    # whatever separator is first in the $volume
    $volume ~= $1
        if ($volume    ~~ m@^([\\/])[\\/][^\\/]+[\\/][^\\/]+\Z(?!\n)@s &&
            $directory ~~ m@^[^\\/]@s);
    $volume ~= $directory;
    # If the volume is not just A:, make sure the glue separator is 
    # there, reusing whatever separator is first in the $volume if possible.
    if ( !($volume ~~ m@^[a-zA-Z]:\Z(?!\n)@s) &&
           $volume ~~ m@[^\\/]\Z(?!\n)@       &&
           $file   ~~ m@[^\\/]@
       ) {
        $volume ~~ m@([\\/])@ ;
        my $sep = $1 ?? $1 :: '\\';
        $volume ~= $sep ;
    }
    $volume ~= $file;
    return $volume;
}


method abs2rel (Str $path, Str $base) returns Str {
    $base = ._cwd() unless $base.defined and $base.bytes;
    for ($path, $base) { $_ = .canonpath($_) }
    my ($path_volume) = .splitpath($path, 1);
    my ($base_volume) = .splitpath($base, 1);
    # Can't relativize across volumes
    return $path unless $path_volume eq $base_volume;
    for ($path, $base) { $_ = .rel2abs($_) }
    my $path_directories = (.splitpath($path, 1))[1];
    my $base_directories = (.splitpath($base, 1))[1];
    # Now, remove all leading components that are the same
    my @pathchunks = .splitdir($path_directories);
    my @basechunks = .splitdir($base_directories);
    while ( @pathchunks && 
            @basechunks && 
            @pathchunks[0].lc eq @basechunks[0].lc 
          ) {
        @pathchunks.shift;
        @basechunks.shift;
    }
    # XXX: not sure if I got this   ---v--- right
    my $result_dirs = .catdir((.updir) x @basechunks, @pathchunks);
    return .canonpath(.catpath('', $result_dirs, ''));
}


method rel2abs (Str $path, Str $base) returns Str {
    if (!.file_name_is_absolute($path)) {
        if (!$base.defined || $base eq '') {
            # XXX not sure how to deal with this Cwd stuff
            require Cwd;
            $base = Cwd::getdcwd((.splitpath($path))[0]) if defined &Cwd::getdcwd;
            $base = ._cwd() unless defined $base;
        }
        elsif (!.file_name_is_absolute($base)) {
            $base = .rel2abs($base);
        }
        else {
            $base = .canonpath($base);
        }

        my ($path_directories, $path_file) = (.splitpath($path, 1))[1,2];

        my ($base_volume, $base_directories) = .splitpath($base, 1);

        $path = .catpath($base_volume, .catdir($base_directories, $path_directories), $path_file);
    }
    return .canonpath($path);
}

1;

__END__

=head1 NAME

File::Spec::Win32 - methods for Win32 file specs

=head1 SYNOPSIS

 require File::Spec::Win32; # Done internally by File::Spec if needed

=head1 DESCRIPTION

See File::Spec::Unix for a documentation of the methods provided
there. This package overrides the implementation of these methods, not
the semantics.

=over 4

=item devnull

Returns a string representation of the null device.

=item tmpdir

Returns a string representation of the first existing directory
from the following list:

    $ENV{TMPDIR}
    $ENV{TEMP}
    $ENV{TMP}
    SYS:/temp
    C:/temp
    /tmp
    /

The SYS:/temp is preferred in Novell NetWare (the File::Spec::Win32
is used also for NetWare).

Since Perl 5.8.0, if running under taint mode, and if the environment
variables are tainted, they are not used.

=item catfile

Concatenate one or more directory names and a filename to form a
complete path ending with a filename


=item canonpath

No physical check on the filesystem, but a logical cleanup of a
path. On UNIX eliminated successive slashes and successive "/.".
On Win32 makes 

	dir1\dir2\dir3\..\..\dir4 -> \dir\dir4 and even
	dir1\dir2\dir3\...\dir4   -> \dir\dir4

=item splitpath

    ($volume,$directories,$file) = File::Spec->splitpath( $path );
    ($volume,$directories,$file) = File::Spec->splitpath( $path, $no_file );

Splits a path into volume, directory, and filename portions. Assumes that 
the last file is a path unless the path ends in '\\', '\\.', '\\..'
or $no_file is true.  On Win32 this means that $no_file true makes this return 
( $volume, $path, '' ).

Separators accepted are \ and /.

Volumes can be drive letters or UNC sharenames (\\server\share).

The results can be passed to L</catpath> to get back a path equivalent to
(usually identical to) the original path.

=item splitdir

The opposite of L<catdir()|File::Spec/catdir()>.

    @dirs = File::Spec->splitdir( $directories );

$directories must be only the directory portion of the path on systems 
that have the concept of a volume or that have path syntax that differentiates
files from directories.

Unlike just splitting the directories on the separator, leading empty and 
trailing directory entries can be returned, because these are significant
on some OSs. So,

    File::Spec->splitdir( "/a/b/c" );

Yields:

    ( '', 'a', 'b', '', 'c', '' )
    
=item catpath

Takes volume, directory and file portions and returns an entire path. Under
Unix, $volume is ignored, and this is just like catfile(). On other OSs,
the $volume become significant.

=back

=head2 Note For File::Spec::Win32 Maintainers

Novell NetWare inherits its File::Spec behaviour from File::Spec::Win32.

=head1 SEE ALSO

See L<File::Spec> and L<File::Spec::Unix>.  This package overrides the
implementation of these methods, not the semantics.

=cut

