use v6;

module File::Spec::Unix-0.0.1;

sub curdir        returns Str  is export { '.'         }
sub updir         returns Str  is export { '..'        }
sub rootdir       returns Str  is export { '/'         }
sub devnull       returns Str  is export { '/dev/null' }
sub case_tolerant returns Bool is export { 0           }

## Splitting

sub splitdir (Str $dir) returns Array is export { split('/', $dir) }

sub splitpath (Str $path, Bool ?$nofile) returns Array is export {
    my ($volume, $directory, $file) = ('','','');
    if ($nofile) {
        $directory = $path;
    }
    else {
        $path ~~ rx:perl5{^ ( (?: .* / (?: \.\.?\Z(?!\n) )? )? ) ([^/]*) };
        $directory = $1;
        $file      = $2;
    }
    return ($volume, $directory, $file);
}

## Concatenating

sub catdir (*@path) returns Str is export { canonpath(join('/', (@path, ''))) }

sub catfile (*@_path) returns Str is export {
    my @path = @_path; # XXX: I shouldnt need to do this
    my $file = pop(@path);
    return $file unless ?@path;
    my $dir = catdir(@path);
    $dir ~= "/" unless substr($dir, -1) eq "/";
    return $dir ~ $file;
}

sub catpath (Str $volume, Str $directory, Str $file) returns Str is export {
    if (''  ne ($directory & $file) && 
        '/' ne (substr($directory, -1) & substr($file, 0, 1))) {
        return $directory ~ "/$file";
    }
    else {
        return $directory ~ $file;
    }
}

## real to absolute

sub rel2abs (Str $path, Str ?$base) returns Str is export {
    if (!file_name_is_absolute($path)) {
        if (!$base.defined || $base eq '') {
            $base = cwd();
        }
        elsif (!file_name_is_absolute($base)) {
            $base = rel2abs($base);
        }
        else {
            $base = canonpath($base);
        }
        $path = catdir($base, $path);
    }
    return canonpath($path);
}

sub abs2rel (Str $path, Str $base) returns Str is export {
    if (!file_name_is_absolute($path)) {
        $path = frel2abs($path);
    }
    else {
        $path = fcanonpath($path);
    }

    # Figure out the effective $base and clean it up.
    if (!$base.defined || $base eq '') {
        $base = cwd();
    }
    elsif (!file_name_is_absolute($base)) {
        $base = rel2abs($base);
    }
    else {
        $base = canonpath($base);
    }

    # Now, remove all leading components that are the same
    my @pathchunks = splitdir($path);
    my @basechunks = splitdir($base);

    while (@pathchunks && @basechunks && @pathchunks[0] eq @basechunks[0]) {
        shift(@pathchunks);
        shift(@basechunks);
    }

    $path = join('/', @pathchunks);
    $base = join('/', @basechunks);

    # $base now contains the directories the resulting relative path 
    # must ascend out of before it can descend to $path_directory.  So, 
    # replace all names with $parentDir
    $base ~~ s:perl5{[^/]+}{..}; # needs to be :g

    # Glue the two together, using a separator if necessary, and preventing an
    # empty result.
    if ('' ne ($path & $base)) { # <<< refactored into junction
        $path = "$base/$path";
    } else {
        $path = "$base$path";
    }
    return canonpath($path);
}

## Misc.

# Refacted this into a Junction instead of the
# regexp since all it does it remove . and ..
sub no_upwards (*@filenames) returns Array is export { 
    @filenames.grep:{ $_ ne ('.' & '..') }
}

sub file_name_is_absolute (Str $file) returns Bool is export { 
    ?($file ~~ rx:perl5{^/})  # needs to work in the multi-line string
}

sub path returns Array is export {
    return unless %*ENV{'PATH'}.defined;
    return split(':', %*ENV{'PATH'}).map:{ $_ eq '' ?? '.' :: $_ };
}

sub canonpath (Str $_path) returns Str is export {
    my $path = $_path; 
    # these all need to be :g
    $path ~~ s:perl5{/+}{/};                            # xx////xx  -> xx/xx   
    $path ~~ s:perl5{(/\.)+(/|\Z(?!\n))}{/};            # xx/././xx -> xx/xx
    $path ~~ s:perl5{^(\./)+}{} unless $path eq "./";   # ./xx      -> xx
    $path ~~ s:perl5{^/(\.\./)+}{/};                    # /../../xx -> xx
    $path ~~ s:perl5{/\Z(?!\n)}{} unless $path eq "/";  # xx/       -> xx
    return $path;
}

# This HACK is worse than 
# the File::Spec platform hack 
sub cwd returns Str { system("pwd") }

=kwid

## TODO:
# Refactor _tmpdir and tmpdir into class attributes
my Str $tmpdir;
method _tmpdir (*@dirlist) returns Str {
    return $tmpdir if $tmpdir.defined;
    ## QUESTION: How does Perl6 handle tainting??
    # {
    #     no strict 'refs';
    #     if (${"\cTAINT"}) { # Check for taint mode on perl >= 5.8.0
    #             require Scalar::Util;
    #         @dirlist = grep { ! Scalar::Util::tainted($_) } @dirlist;
    #     }
    # }
    for @dirlist -> $dir {
        next unless $dir.defined && -d -w $dir;
        $tmpdir = $dir;
        last;
    }
    $tmpdir = $.curdir unless $tmpdir.defined;
    $tmpdir = $tmpdir.defined && .canonpath($tmpdir);
    return $tmpdir;
}

method tmpdir () returns Str {
    return $tmpdir if $tmpdir.defined;
    $tmpdir = ._tmpdir(%*ENV{'TMPDIR'}, "/tmp");
    return $tmpdir;
}

=cut