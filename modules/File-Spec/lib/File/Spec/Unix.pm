#!pugs
use v6;

class File::Spec::Unix-0.0.1;

method curdir  () returns Str { '.' }
method updir   () returns Str { '..' }
method rootdir () returns Str { '/' }
method devnull () returns Str { '/dev/null' }

method case_tolerant () returns Bool { 0 }

method canonpath ($path) returns Str {
    # Handle POSIX-style node names beginning with double slash (qnx, nto)
    # Handle network path names beginning with double slash (cygwin)
    # (POSIX says: "a pathname that begins with two successive slashes
    # may be interpreted in an implementation-defined manner, although
    # more than two leading slashes shall be treated as a single slash.")
    my $node = '';
    if ( $?OS ~~ m/^(?:qnx|nto|cygwin)$/ && $path ~~ s:^(//[^/]+)(/|\z):/:s ) {
      $node = $1;
    }
    # This used to be
    # $path =~ s|/+|/|g unless($^O eq 'cygwin');
    # but that made tests 29, 30, 35, 46, and 213 (as of #13272) to fail
    # (Mainly because trailing "" directories didn't get stripped).
    # Why would cygwin avoid collapsing multiple slashes into one? --jhi
    $path ~~ s|/+|/|g;                             # xx////xx  -> xx/xx
    $path ~~ s@(/\.)+(/|\Z(?!\n))@/@g;             # xx/././xx -> xx/xx
    $path ~~ s|^(\./)+||s unless $path eq "./";    # ./xx      -> xx
    $path ~~ s|^/(\.\./)+|/|s;                     # /../../xx -> xx
    $path ~~ s|/\Z(?!\n)|| unless $path eq "/";          # xx/       -> xx
    return "$node$path";
}

method catdir (*@path) return Str {
    @path.push(''); # '' because need a trailing '/'
    $.canonpath(@path.join('/')); 
}

method catfile (*@path) return Str {
    my $file = $.canonpath(@path.pop);
    return $file unless ?@path;
    my $dir = $.catdir(@path);
    $dir .= "/" unless substr($dir, -1) eq "/";
    return $dir.$file;
}


my $tmpdir;
method _tmpdir (*@dirlist) returns Str {
    return $tmpdir if defined $tmpdir;
    ## QUESTION: How does Perl6 handle tainting??
    # {
    #     no strict 'refs';
    #     if (${"\cTAINT"}) { # Check for taint mode on perl >= 5.8.0
    #             require Scalar::Util;
    #         @dirlist = grep { ! Scalar::Util::tainted($_) } @dirlist;
    #     }
    # }
    for @dirlist -> $dir {
        next unless defined($dir) && -d -w $dir;
        $tmpdir = $dir;
        last;
    }
    $tmpdir = $.curdir unless defined $tmpdir;
    $tmpdir = defined $tmpdir && $.canonpath($tmpdir);
    return $tmpdir;
}

method tmpdir () returns Str {
    return $tmpdir if defined $tmpdir;
    $tmpdir = $._tmpdir( %*ENV{TMPDIR}, "/tmp" );
    return $tmpdir;
}

method no_upwards (*@filenames) returns Array {
    return @filenames.grep:{ !/^\.{1,2}\Z(?!\n)/s };
}

method file_name_is_absolute ($file) returns Bool {
    return ?($file ~~ m:^/:s);
}

method path () returns Array {
    return () unless exists %*ENV{PATH};
    my @path = %*ENV{PATH}.split(':');
    for @path { $_ = '.' if $_ eq '' }
    return @path;
}

method join (*@path) return Str  {
    return $.catfile(@path);
}

method splitpath (Str $path, $nofile) returns Array {
    my ($volume, $directory, $file) = ('','','');
    if ($nofile) {
        $directory = $path;
    }
    else {
        $path ~~ m|^ ( (?: .* / (?: \.\.?\Z(?!\n) )? )? ) ([^/]*) |xs;
        $directory = $1;
        $file      = $2;
    }
    return ($volume, $directory, $file);
}

method splitdir ($dir) returns Array {
    return $dir.split('/');  # Preserve trailing fields
}

method catpath (Str $volume, Str $directory, Str $file) returns Str {
    if ( $directory ne ''                && 
         $file ne ''                     && 
         substr( $directory, -1 ) ne '/' && 
         substr( $file, 0, 1 ) ne '/' 
    ) {
        $directory .= "/$file" ;
    }
    else {
        $directory .= $file ;
    }
    return $directory ;
}

method abs2rel (Str $path, Str $base) return Str {
    # Clean up $path
    if ( ! $.file_name_is_absolute( $path ) ) {
        $path = $.rel2abs( $path );
    }
    else {
        $path = $.canonpath( $path );
    }

    # Figure out the effective $base and clean it up.
    if ( !defined( $base ) || $base eq '' ) {
        $base = $._cwd();
    }
    elsif ( ! $.file_name_is_absolute( $base ) ) {
        $base = $.rel2abs( $base );
    }
    else {
        $base = $.canonpath( $base );
    }

    # Now, remove all leading components that are the same
    my @pathchunks = $.splitdir( $path);
    my @basechunks = $.splitdir( $base);

    while (@pathchunks && @basechunks && $pathchunks[0] eq $basechunks[0]) {
        @pathchunks.shift;
        @basechunks.shift;
    }

    $path = @pathchunks.join( '/' );
    $base = @basechunks.join( '/' );

    # $base now contains the directories the resulting relative path 
    # must ascend out of before it can descend to $path_directory.  So, 
    # replace all names with $parentDir
    $base ~~ s|[^/]+|..|g;

    # Glue the two together, using a separator if necessary, and preventing an
    # empty result.
    if ( $path ne '' && $base ne '' ) {
        $path = "$base/$path";
    } else {
        $path = "$base$path";
    }

    return $.canonpath( $path );
}

method rel2abs (Str $path, Str $base) returns Str {

    # Clean up $path
    if ( ! $.file_name_is_absolute( $path ) ) {
        # Figure out the effective $base and clean it up.
        if ( !defined( $base ) || $base eq '' ) {
	    $base = $._cwd();
        }
        elsif ( ! $.file_name_is_absolute( $base ) ) {
            $base = $.rel2abs( $base ) ;
        }
        else {
            $base = $.canonpath( $base ) ;
        }

        # Glom them together
        $path = $.catdir( $base, $path ) ;
    }

    return $.canonpath( $path ) ;
}

# Internal routine to File::Spec, no point in making this public since
# it is the standard Cwd interface.  Most of the platform-specific
# File::Spec subclasses use this.
sub _cwd {
    require Cwd-0.0.1;
    Cwd::cwd();
}

1;

__END__

=head1 NAME

File::Spec::Unix - File::Spec for Unix, base for other File::Spec modules

=head1 SYNOPSIS

 require File::Spec::Unix; # Done automatically by File::Spec

=head1 DESCRIPTION

Methods for manipulating file specifications.  Other File::Spec
modules, such as File::Spec::Mac, inherit from File::Spec::Unix and
override specific methods.

=head1 METHODS

=over 2

=item canonpath()

No physical check on the filesystem, but a logical cleanup of a
path. On UNIX eliminates successive slashes and successive "/.".

    $cpath = File::Spec->canonpath( $path ) ;

=item catdir()

Concatenate two or more directory names to form a complete path ending
with a directory. But remove the trailing slash from the resulting
string, because it doesn't look good, isn't necessary and confuses
OS2. Of course, if this is the root directory, don't cut off the
trailing slash :-)

=item catfile

Concatenate one or more directory names and a filename to form a
complete path ending with a filename

=item curdir

Returns a string representation of the current directory.  "." on UNIX.

=item devnull

Returns a string representation of the null device. "/dev/null" on UNIX.

=item rootdir

Returns a string representation of the root directory.  "/" on UNIX.

=item tmpdir

Returns a string representation of the first writable directory from
the following list or the current directory if none from the list are
writable:

    $ENV{TMPDIR}
    /tmp

Since perl 5.8.0, if running under taint mode, and if $ENV{TMPDIR}
is tainted, it is not used.

=item updir

Returns a string representation of the parent directory.  ".." on UNIX.

=item no_upwards

Given a list of file names, strip out those that refer to a parent
directory. (Does not strip symlinks, only '.', '..', and equivalents.)

=item case_tolerant

Returns a true or false value indicating, respectively, that alphabetic
is not or is significant when comparing file specifications.

=item file_name_is_absolute

Takes as argument a path and returns true if it is an absolute path.

This does not consult the local filesystem on Unix, Win32, OS/2 or Mac 
OS (Classic).  It does consult the working environment for VMS (see
L<File::Spec::VMS/file_name_is_absolute>).


=item path

Takes no argument, returns the environment variable PATH as an array.

=item join

join is the same as catfile.

=item splitpath

    ($volume,$directories,$file) = File::Spec->splitpath( $path );
    ($volume,$directories,$file) = File::Spec->splitpath( $path, $no_file );

Splits a path into volume, directory, and filename portions. On systems
with no concept of volume, returns '' for volume. 

For systems with no syntax differentiating filenames from directories, 
assumes that the last file is a path unless $no_file is true or a 
trailing separator or /. or /.. is present. On Unix this means that $no_file
true makes this return ( '', $path, '' ).

The directory portion may or may not be returned with a trailing '/'.

The results can be passed to L</catpath()> to get back a path equivalent to
(usually identical to) the original path.

=item splitdir

The opposite of L</catdir()>.

    @dirs = File::Spec->splitdir( $directories );

$directories must be only the directory portion of the path on systems 
that have the concept of a volume or that have path syntax that differentiates
files from directories.

Unlike just splitting the directories on the separator, empty
directory names (C<''>) can be returned, because these are significant
on some OSs.

On Unix,

    File::Spec->splitdir( "/a/b//c/" );

Yields:

    ( '', 'a', 'b', '', 'c', '' )


=item catpath()

Takes volume, directory and file portions and returns an entire path. Under
Unix, $volume is ignored, and directory and file are concatenated.  A '/' is
inserted if needed (though if the directory portion doesn't start with
'/' it is not added).  On other OSs, $volume is significant.

=item abs2rel

Takes a destination path and an optional base path returns a relative path
from the base path to the destination path:

    $rel_path = File::Spec->abs2rel( $path ) ;
    $rel_path = File::Spec->abs2rel( $path, $base ) ;

If $base is not present or '', then L<cwd()|Cwd> is used. If $base is
relative, then it is converted to absolute form using
L</rel2abs()>. This means that it is taken to be relative to
L<cwd()|Cwd>.

On systems that have a grammar that indicates filenames, this ignores the 
$base filename. Otherwise all path components are assumed to be
directories.

If $path is relative, it is converted to absolute form using L</rel2abs()>.
This means that it is taken to be relative to L<cwd()|Cwd>.

No checks against the filesystem are made.  On VMS, there is
interaction with the working environment, as logicals and
macros are expanded.

Based on code written by Shigio Yamaguchi.

=item rel2abs()

Converts a relative path to an absolute path. 

    $abs_path = File::Spec->rel2abs( $path ) ;
    $abs_path = File::Spec->rel2abs( $path, $base ) ;

If $base is not present or '', then L<cwd()|Cwd> is used. If $base is
relative, then it is converted to absolute form using
L</rel2abs()>. This means that it is taken to be relative to
L<cwd()|Cwd>.

On systems that have a grammar that indicates filenames, this ignores
the $base filename. Otherwise all path components are assumed to be
directories.

If $path is absolute, it is cleaned up and returned using L</canonpath()>.

No checks against the filesystem are made.  On VMS, there is
interaction with the working environment, as logicals and
macros are expanded.

Based on code written by Shigio Yamaguchi.

=back

=head1 SEE ALSO

L<File::Spec>

=cut