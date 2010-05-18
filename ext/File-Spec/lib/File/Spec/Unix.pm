use v6;

module File::Spec::Unix;

sub curdir        returns Str  is export { '.'         }
sub updir         returns Str  is export { '..'        }
sub rootdir       returns Str  is export { '/'         }
sub devnull       returns Str  is export { '/dev/null' }
sub case_tolerant returns Bool is export { 0           }

## Splitting

sub splitdir (Str $dir) returns Array is export { split('/', $dir) }

sub splitpath (Str $path, Bool $nofile?) returns Array is export {
    my $volume    = '';
    my $directory = '';
    my $file      = '';
    if ($nofile) {
        $directory = $path;
    }
    else {
        #$path ~~ m:P5"^((?:.*/(?:\.\.?\Z(?!\n))?)?)([^/]*)";
	$path ~~ rx{ :s (.*\/) ( <-[ / ]> ) };
        $directory = ~$0;
        $file      = ~$1;
    }
    return ($volume, $directory, $file);
}

## Concatenating

sub catdir (*@path) returns Str is export { canonpath(join('/', (@path, ''))) }

sub catfile (*@_path) returns Str is export {
    # take a copy of our args here, maybe
    # replace this with 'is copy' parameter
    # trait at some point
    my @path = @_path;
    my $file = canonpath(pop(@path));
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

sub rel2abs (Str $_path, Str $_base?) returns Str is export {
    # take a copy of our args here, maybe 
    # replace this with 'is copy' parameter 
    # trait at some point    
    my $path = $_path;
    my $base = $_base;
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

sub abs2rel (Str $_path, Str $_base) returns Str is export {
    # take a copy of our args here, maybe 
    # replace this with 'is copy' parameter 
    # trait at some point    
    my $path = $_path;
    my $base = $_base;
    if (!file_name_is_absolute($path)) {
        $path = rel2abs($path);
    }
    else {
        $path = canonpath($path);
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

    while (@pathchunks and @basechunks) {
        @pathchunks[0] eq @basechunks[0] or last;
        shift(@pathchunks);
        shift(@basechunks);
    }

    $path = join('/', @pathchunks);
    $base = join('/', @basechunks);

    # $base now contains the directories the resulting relative path
    # must ascend out of before it can descend to $path_directory.  So,
    # replace all names with $parentDir
    #$base ~~ s:P5:g"[^/]+"..";
    #$base ~~ s:g"[^/]+"..";
    $base .= subst(/ <-[ / ]>+ /, "..", :g);

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

sub canonpath (Str $_path) returns Str is export {
    # take a copy of our args here, maybe 
    # replace this with 'is copy' parameter 
    # trait at some point    
    my $path = $_path;
#    $path ~~ s:P5:g"/+"/";                            # xx////xx  -> xx/xx
#    $path ~~ s:P5:g"(/\.)+(/|\Z(?!\n))"/";            # xx/././xx -> xx/xx
#    $path ~~ s:P5:g"^(\./)+"" unless $path eq "./";   # ./xx      -> xx
#    $path ~~ s:P5:g"^/(\.\./)+"/";                    # /../../xx -> xx
#    $path ~~ s:P5:g"/\Z(?!\n)"" unless $path eq "/";  # xx/       -> xx
    $path .= subst(/ \/+ /, "/", :g);		          # xx////xx  -> xx/xx
    $path .= subst( rx{ ( \/\. )+ \/ }, "/", :g);         # xx/././xx -> xx/xx
    $path .= subst(/ (\.\/)+ /, "",:g) eq "./";           # ./xx      -> xx
    $path .= subst(/ ^\/(\.\.\/)+/, "", :g);              # /../../xx -> xx
    $path .= subst(/ \/$ /, "", :g) unless $path eq "/";  # xx/       -> xx
    return $path;
}

# Refacted this into a Junction instead of the
# regexp since all it does it remove . and ..
sub no_upwards (*@filenames) returns Array is export {
    @filenames.grep:{ $_ ne ('.' & '..') }
}

sub file_name_is_absolute (Str $file) returns Bool is export {
#    ?($file ~~ m:P5"^/")  # needs to work in the multi-line string
    ?( $file ~~ m/ ^\/ / )  # needs rework to find the absolute path...
}

sub path returns Array is export {
    return unless %*ENV{'PATH'}.defined;
    return split(':', %*ENV{'PATH'}).map:{ $_ eq '' ?? '.' !! $_ };
}

# This HACK is worse than
# the File::Spec platform hack
#sub cwd returns Str { system("pwd") }
sub cwd returns Str is export {
  # This seems wrong - limbic_region 2006-08-17
  #return '\\';
  return $*CWD;
}

sub tmpdir returns Str is export {
  return '';
}

#
# ## TODO:
# # Refactor _tmpdir and tmpdir into class attributes
# my Str $tmpdir;
# method _tmpdir (*@dirlist) returns Str {
#     return $tmpdir if $tmpdir.defined;
#     ## QUESTION: How does Perl6 handle tainting??
#     # {
#     #     no strict 'refs';
#     #     if (${"\cTAINT"}) { # Check for taint mode on perl >= 5.8.0
#     #             require Scalar::Util;
#     #         @dirlist = grep { ! Scalar::Util::tainted($_) }, @dirlist;
#     #     }
#     # }
#     for @dirlist -> $dir {
#         next unless $dir.defined && $dir~~(:d|:w);
#         $tmpdir = $dir;
#         last;
#     }
#     $tmpdir = $.curdir unless $tmpdir.defined;
#     $tmpdir = $tmpdir.defined && .canonpath($tmpdir);
#     return $tmpdir;
# }
#
# method tmpdir () returns Str {
#     return $tmpdir if $tmpdir.defined;
#     $tmpdir = ._tmpdir(%*ENV{'TMPDIR'}, "/tmp");
#     return $tmpdir;
# }

=begin File::Spec::Unix Documentation
=kwid

= NAME

File::Spec::Unix - Part of Perl 6/Pugs Portable file handling

= SYNOPOSIS

  use File::Spec::Unix;

= DESCRIPTION

This is a very primative port of the Perl 5 File::Spec::Unix module.

= FUNCTIONS

- `curdir returns Str`

- `updir returns Str`

- `rootdir returns Str`

- `devnull returns Str`

- `case_tolerant returns Bool`

- `splitdir (Str $dir) returns Array`

- `splitpath (Str $path, Bool $nofile?) returns Array`

- `catdir (*@path) returns Str`

- `catfile (*@_path) returns Str`

- `catpath (Str $volume, Str $directory, Str $file) returns Str`

- `rel2abs (Str $path, Str $base?) returns Str`

- `abs2rel (Str $path, Str $base) returns Str`

- `no_upwards (*@filenames) returns Array`

- `file_name_is_absolute (Str $file) returns Bool`

- `path returns Array`

- `canonpath (Str $_path) returns Str`

- `cwd returns Str`

= SEE ALSO

The Perl 5 version of File::Spec::Unix, although this version is more
akin to File::Spec::Functions.

= AUTHOR

Stevan Little <stevan@iinteractive.com>

Max Maischein <corion@cpan.org>

= ACKNOWLEDGEMENTS

This is a port of the Perl 5 File::Spec::Unix module which is currently
maintained by Ken Williams <KWILLIAMS@cpan.org>, and is written
by a number of people. Please see that module for more information.

= COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
=end File::Spec::Unix Documentation
## vim: ft=perl6
