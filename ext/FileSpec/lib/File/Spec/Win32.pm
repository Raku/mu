use v6;

module File::Spec::Win32-0.0.1;

sub curdir        returns Str  is export { '.'   }
sub updir         returns Str  is export { '..'  }
sub rootdir       returns Str  is export { '/'   }
sub devnull       returns Str  is export { 'nul' }
sub case_tolerant returns Bool is export { 1     }

## Splitting

sub splitdir (Str $directories) returns Array is export { split("\\", $directories) }

sub splitpath (Str $path, Bool ?$nofile) returns Array is export {
    my ($volume, $directory, $file) = ('','','');
    if ($nofile) {
        $path ~~ rx:perl5{^( (?:[a-zA-Z]:|(?:\\\\|//)[^\\/]+[\\/][^\\/]+)? ) (.*)};
        $volume    = $1;
        $directory = $2;
    }
    else {
        $path ~~ rx:perl5{^ ( (?: [a-zA-Z]: | (?:\\\\|//)[^\\/]+[\\/][^\\/]+)?)( (?:.*[\\/](?:\.\.?\Z(?!\n))?)? )(.*)};
        $volume    = $1;
        $directory = $2;
        $file      = $3;
    }
    return ($volume, $directory, $file);
}

## Concatenating

sub catdir (*@path) returns Str is export {
    my @new_path;
    for (@path) -> $e {
        $e ~~ s:perl5:g{/}{\\};
        # append a backslash to each argument unless it has one there
        unless (substr($e, -1) eq "\\") {
            push(@new_path, $e ~ "\\");
        }
        else {
            push(@new_path, $e);
        }
    }
    return canonpath(join('', @new_path));
}

sub catfile (*@_path) returns Str is export {
    my @path = @_path;
    my $file = canonpath(pop(@path));
    return $file unless +@path;
    my $dir = catdir(@path);
    $dir ~= "\\" unless substr($dir, -1) eq "\\";
    return $dir ~ $file;
}

sub catpath (Str $volume, Str $directory, Str $file) returns Str is export {
    # If it's UNC, make sure the glue separator is there, reusing
    # whatever separator is first in the $volume
    my $vol = $volume;
    $vol ~= $1 if ($vol ~~ rx:perl5{^([\\/])[\\/][^\\/]+[\\/][^\\/]+$} && $directory ~~ rx:perl5{^[^\\/]});
    $vol ~= $directory;
    # If the volume is not just A:, make sure the glue separator is 
    # there, reusing whatever separator is first in the $volume if possible.
    if ( !($vol  ~~ rx:perl5{^[a-zA-Z]:$}) &&
           $vol  ~~ rx:perl5{[^\\/]$}      &&
           $file ~~ rx:perl5{[^\\/]}
       ) {
        $vol ~~ rx:perl5{([\\/])};
        my $sep = $1 ?? $1 :: "\\";
        $vol ~= $sep;
    }
    $vol ~= $file;
    return $vol;
}

## Misc

sub canonpath (Str $_path) returns Str is export {
    my $path = $_path;
    my $orig_path = $path;
#     $path ~~ s/^([a-z]:)/\u$1/s;
    $path ~~ s:perl5{/}{\\}; #g
#     $path ~~ s|([^\\])\\+|$1\\|g;                                                 # xx\\\\xx  -> xx\xx
    $path ~~ s:perl5{(\\\.)+\\}{\\}; #g                                             # xx\.\.\xx -> xx\xx
    $path ~~ s:perl5{^(\.\\)+}{} unless $path eq ".\\";                             # .\xx      -> xx
    $path ~~ s:perl5{\\\Z(?!\n)}{} unless $path ~~ rx:perl5{^([A-Z]:)?\\\Z(?!\n)};  # xx\       -> xx
    # xx1/xx2/xx3/../../xx -> xx1/xx
    $path ~~ s:perl5{\\\.\.\.\\}{\\\.\.\\\.\.\\}; #g                                # \...\ is 2 levels up
    $path ~~ s:perl5{^\.\.\.\\}{\.\.\\\.\.\\}; #g                                   # ...\ is 2 levels up
    return $path if $path ~~ rx:perl5{^\.\.};                                       # skip relative paths
    return $path unless $path ~~ rx:perl5{\.\.};                                    # too few .'s to cleanup
    return $path if $path ~~ rx:perl5{\.\.\.\.};                                    # too many .'s to cleanup
    $path ~~ s:perl5{^\\\.\.$}{\\};                                                 # \..    -> \
    1 while $path ~~ s:perl5{^\\\.\.}{};                                            # \..\xx -> \xx

    my ($vol, $dirs, $file) = splitpath($path);
    my @dirs = splitdir($dirs);
    my (@base_dirs, @path_dirs);
    my $dest = @base_dirs;
    for (@dirs) -> $dir {
        $dest = @path_dirs if $dir eq updir();
        push($dir, $dest);
    }
    # for each .. in @path_dirs pop one item from 
    # @base_dirs
    while (my $dir = shift(@path_dirs)){ 
        unless ($dir eq updir()){
            unshift(@path_dirs, $dir);
            last();
        }
        pop(@base_dirs);
    }
    $path = catpath($vol, catdir(@base_dirs, @path_dirs), $file);
    return $path;
}

sub path returns Array is export {
    my $path = %*ENV{'PATH'} || %*ENV{'Path'} || %*ENV{'path'};
    return split(';', $path).map:{ $_ eq '' ?? '.' :: $_ };
}

sub file_name_is_absolute (Str $file) returns Bool is export { 
	?($file ~~ rx:perl5{^([a-zA-Z]:)?[\\/]} ) 
}

# my $tmpdir;
# method tmpdir () returns Str {
#     return $tmpdir if $tmpdir.defined;
#     $tmpdir = ._tmpdir( %*ENV{'TMPDIR', 'TEMP', 'TMP'}, 'SYS:/temp', 'C:/temp', '/tmp', '/');
#     return $tmpdir;
# }
# 
# method abs2rel (Str $path, Str $base) returns Str {
#     $base = ._cwd() unless $base.defined and $base.bytes;
#     for ($path, $base) { $_ = .canonpath($_) }
#     my ($path_volume) = .splitpath($path, 1);
#     my ($base_volume) = .splitpath($base, 1);
#     # Can't relativize across volumes
#     return $path unless $path_volume eq $base_volume;
#     for ($path, $base) { $_ = .rel2abs($_) }
#     my $path_directories = (.splitpath($path, 1))[1];
#     my $base_directories = (.splitpath($base, 1))[1];
#     # Now, remove all leading components that are the same
#     my @pathchunks = .splitdir($path_directories);
#     my @basechunks = .splitdir($base_directories);
#     while ( @pathchunks && 
#             @basechunks && 
#             @pathchunks[0].lc eq @basechunks[0].lc 
#           ) {
#         @pathchunks.shift;
#         @basechunks.shift;
#     }
#     # XXX: not sure if I got this   ---v--- right
#     my $result_dirs = .catdir((.updir) x @basechunks, @pathchunks);
#     return .canonpath(.catpath('', $result_dirs, ''));
# }
# 
# 
# method rel2abs (Str $path, Str $base) returns Str {
#     if (!.file_name_is_absolute($path)) {
#         if (!$base.defined || $base eq '') {
#             # XXX not sure how to deal with this Cwd stuff
#             require Cwd;
#             $base = Cwd::getdcwd((.splitpath($path))[0]) if defined &Cwd::getdcwd;
#             $base = ._cwd() unless defined $base;
#         }
#         elsif (!.file_name_is_absolute($base)) {
#             $base = .rel2abs($base);
#         }
#         else {
#             $base = .canonpath($base);
#         }
# 
#         my ($path_directories, $path_file) = (.splitpath($path, 1))[1,2];
# 
#         my ($base_volume, $base_directories) = .splitpath($base, 1);
# 
#         $path = .catpath($base_volume, .catdir($base_directories, $path_directories), $path_file);
#     }
#     return .canonpath($path);
# }


=kwid

= NAME

File::Spec::Win32 - Part of Perl6/Pugs Portable file handling

= SYNOPOSIS

  use File::Spec::Win32;

= DESCRIPTION

This is a very primative port of the perl5 File::Spec::Win32 module.

= FUNCTIONS

- `curdir returns Str`

- `updir returns Str`

- `rootdir returns Str`

- `devnull returns Str`

- `case_tolerant returns Bool`

- `splitdir (Str $dir) returns Array`

- `splitpath (Str $path, Bool ?$nofile) returns Array`

- `catdir (*@path) returns Str`

- `catfile (*@_path) returns Str`

- `catpath (Str $volume, Str $directory, Str $file) returns Str`

- `rel2abs (Str $path, Str ?$base) returns Str`

- `abs2rel (Str $path, Str $base) returns Str`

- `no_upwards (*@filenames) returns Array`

- `file_name_is_absolute (Str $file) returns Bool`

- `path returns Array`

- `canonpath (Str $_path) returns Str`

- `cwd returns Str`

= SEE ALSO

The Perl5 version of File::Spec::Win32, although this version is more
akin to File::Spec::Functions.

= AUTHOR

Stevan Little <stevan@iinteractive.com>

= ACKNOWLEDGEMENTS

This is a port of the perl5 File::Spec::Win32 module which is currently 
maintained by Ken Williams <KWILLIAMS@cpan.org>, and is written
by a number of people. Please see that module for more information.

= COPYRIGHT 

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
