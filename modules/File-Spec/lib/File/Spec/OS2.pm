#!pugs
use v6;

require File::Spec::Unix-0.0.1;

class File::Spec::OS2 is File::Spec::Unix;

method devnull () returns Str { '/dev/nul' }

method case_tolerant () returns Bool { 1 }

method file_name_is_absolute (Str $file) returns Bool {
    return ?($file ~~ m{^([a-z]:)?[\\/]}is);
}

method path () returns Array {
    my $path = %*ENV{'PATH'};
    $path ~~ s:\\:/:g;
    my @path = $path.split(';');
    for (@path) { $_ = '.' if $_ eq '' }
    return @path;
}

method _cwd () returns Str {
    # In OS/2 the "require Cwd" is unnecessary bloat.
    return Cwd::sys_cwd();
}

my $tmpdir;
method tmpdir () returns Str {
    return $tmpdir if defined $tmpdir;
    $tmpdir = ._tmpdir( %*ENV{'TMPDIR', 'TEMP', 'TMP'}, '/tmp', '/');
    return $tmpdir;
}

method catdir (*@path) returns Str {
    my @args = @path;
    for (@args) {
        tr[\\][/];
        # append a backslash to each argument unless it has one there
        $_ ~= "/" unless m{/$};
    }
    return .canonpath(@args.join(''));
}

method canonpath (Str $path) returns Str {
    $path ~~ s/^([a-z]:)/\l$1/s;
    $path ~~ s|\\|/|g;
    $path ~~ s|([^/])/+|$1/|g;                  # xx////xx  -> xx/xx
    $path ~~ s|(/\.)+/|/|g;                     # xx/././xx -> xx/xx
    $path ~~ s|^(\./)+(?=[^/])||s;		# ./xx      -> xx
    $path ~~ s|/\Z(?!\n)||
             unless $path ~~ m#^([a-z]:)?/\Z(?!\n)#si;# xx/       -> xx
    $path ~~ s{^/\.\.$}{/};                     # /..    -> /
    1 while $path ~~ s{^/\.\.}{};               # /../xx -> /xx
    return $path;
}


method splitpath (Str $path, Bool $nofile) returns Array {
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
                ( (?:.*[\\\\/](?:\.\.?\Z(?!\n))?)? )
                (.*)
             }xs;
        $volume    = $1;
        $directory = $2;
        $file      = $3;
    }
    return ($volume, $directory, $file);
}


method splitdir (Str $directories) returns Array {
    return $directories.split m|[\\/]|;
}


method catpath (Str $volume, Str $directory, Str $file) returns Str {
    # If it's UNC, make sure the glue separator is there, reusing
    # whatever separator is first in the $volume
    $volume ~= $1
        if ($volume ~~ m@^([\\/])[\\/][^\\/]+[\\/][^\\/]+\Z(?!\n)@s 
            &&
            $directory @~ m@^[^\\/]@s);

    $volume ~= $directory;
    # If the volume is not just A:, make sure the glue separator is 
    # there, reusing whatever separator is first in the $volume if possible.
    if (
        !($volume ~~ m@^[a-zA-Z]:\Z(?!\n)@s) &&
         $volume ~~ m@[^\\/]\Z(?!\n)@        &&
         $file   ~~ m@[^\\/]@
       ) {
        $volume ~~ m@([\\/])@;
        my $sep = $1 ?? $1 :: '/';
        $volume ~= $sep;
    }
    $volume ~= $file;
    return $volume;
}

method abs2rel (Str $path, Str $base) returns Str {
    # Clean up $path
    if (!.file_name_is_absolute($path)) {
        $path = .rel2abs($path);
    } 
    else {
        $path = .canonpath($path);
    }

    # Figure out the effective $base and clean it up.
    if (!$base.defined || $base eq '') {
        $base = ._cwd();
    } 
    elsif (!.file_name_is_absolute($base)) {
        $base = .rel2abs($base);
    } 
    else {
        $base = .canonpath($base);
    }

    # Split up paths
    my ($path_volume, $path_directories, $path_file) = .splitpath($path, 1);
    my ($base_volume, $base_directories) = .splitpath($base, 1);
    return $path unless $path_volume eq $base_volume;

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

    # No need to catdir, we know these are well formed.
    $path_directories = @pathchunks.join( '/');
    $base_directories = @basechunks.join( '/');

    # $base_directories now contains the directories the resulting relative
    # path must ascend out of before it can descend to $path_directory.  So, 
    # replace all names with $parentDir

    #FA Need to replace between backslashes...
    $base_directories ~~ s|[^\\/]+|..|g ;

    # Glue the two together, using a separator if necessary, and preventing an
    # empty result.

    #FA Must check that new directories are not empty.
    if ( $path_directories ne '' && $base_directories ne '' ) {
        $path_directories = "$base_directories/$path_directories" ;
    } else {
        $path_directories = "$base_directories$path_directories" ;
    }

    return .canonpath(.catpath("", $path_directories, $path_file));
}


method rel2abs (Str $path, Str $base) returns Str {
    if (!.file_name_is_absolute($path)) {
        if (!$base.defined || $base eq '') {
            $base = ._cwd();
        }
        elsif (!.file_name_is_absolute($base)) {
            $base = .rel2abs($base);
        }
        else {
            $base = .canonpath($base);
        }

        my ($path_directories, $path_file) = (.splitpath($path, 1))[1,2];

        my ($base_volume, $base_directories) = .splitpath($base, 1);

        $path = .catpath( 
            $base_volume, 
            .catdir($base_directories, $path_directories), 
            $path_file
        );
    }

    return .canonpath($path);
}

1;

__END__

=head1 NAME

File::Spec::OS2 - methods for OS/2 file specs

=head1 SYNOPSIS

 require File::Spec::OS2; # Done internally by File::Spec if needed

=head1 DESCRIPTION

See L<File::Spec> and L<File::Spec::Unix>.  This package overrides the
implementation of these methods, not the semantics.

Amongst the changes made for OS/2 are...

=over 4

=item tmpdir

Modifies the list of places temp directory information is looked for.

    $ENV{TMPDIR}
    $ENV{TEMP}
    $ENV{TMP}
    /tmp
    /

=item splitpath

Volumes can be drive letters or UNC sharenames (\\server\share).

=back

=cut
