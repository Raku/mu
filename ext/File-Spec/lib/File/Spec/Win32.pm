use v6;

module File::Spec::Win32-0.0.1;

sub curdir        returns Str  is export { '.'   }
sub updir         returns Str  is export { '..'  }
sub rootdir       returns Str  is export { '\\'  }
sub devnull       returns Str  is export { 'nul' }
sub case_tolerant returns Bool is export { 1     }

## Splitting

sub splitdir (Str $directories) returns Array is export {
    # this is an ugly hack since we dont
    # have split(<regexp>, Str) yet.
    my @dirs = split("/", $directories);
    @dirs = @dirs.map:{ split("\\", $_) };
    if (($directories ~~ rx:perl5{[\\/]\Z(?!\n)})) {
        @dirs[@dirs - 1] = '';
    }
    @dirs = map {~$_ } @dirs;
    return @dirs;
}

sub splitpath (Str $path, Bool ?$nofile) returns Array is export {
    my ($volume, $directory, $file) = ('','','');
    if ($nofile) {
        $path ~~ rx:perl5{^((?:[a-zA-Z]:|(?:\\\\|//)[^\\/]+[\\/][^\\/]+)?)(.*)};
        $volume    = ~$0;
        $directory = ~$1;
    }
    else {
        $path ~~ rx:perl5{^((?:[a-zA-Z]:|(?:\\\\|//)[^\\/]+[\\/][^\\/]+)?)((?:.*[\\/](?:\.\.?\Z(?!\n))?)?)(.*)};
        $volume    = ~$0;
        $directory = ~$1;
        $file      = ~$2;
    }
    return ($volume, $directory, $file);
}

## Concatenating

sub catdir (*@_path) returns Str is export {
    return '' unless +@_path;
    # take a copy of our args here, maybe
    # replace this with 'is copy' parameter
    # trait at some point
    my @path = @_path;
    my @new_path;
    my $i = 0;
    loop ($i = 0; $i < @path; $i++) {
        @path[$i] ~~ s:perl5:g{/}{\\};
        unless (@path[$i] eq "\\" || @path[$i] ~~ rx:perl5{\\$}) {
            push(@new_path, @path[$i] ~ "\\");
        }
        else {
            push(@new_path, @path[$i]);
        }
    }
    return canonpath(join('', @new_path));
}

sub catfile (*@_path) returns Str is export {
    # take a copy of our args here, maybe
    # replace this with 'is copy' parameter
    # trait at some point
    my @path = @_path;
    my $file = canonpath(pop(@path));
    return $file unless ?@path;
    my $dir = catdir(@path[0], @path[1], @path[2], @path[3]);
    $dir ~= "\\" unless substr($dir, -1) eq "\\";
    return $dir ~ $file;
}

sub catpath (Str $volume, Str $directory, Str $file) returns Str is export {
    # If it's UNC, make sure the glue separator is there, reusing
    # whatever separator is first in the $volume
    my $vol = $volume;
    $vol ~= $0 if ($vol ~~ rx:perl5{^([\\/])[\\/][^\\/]+[\\/][^\\/]+$} && $directory ~~ rx:perl5{^[^\\/]});
    $vol ~= $directory;
    # If the volume is not just A:, make sure the glue separator is
    # there, reusing whatever separator is first in the $volume if possible.
    if ( !($vol  ~~ rx:perl5{^[a-zA-Z]:$}) &&
           $vol  ~~ rx:perl5{[^\\/]$}      &&
           $file ~~ rx:perl5{[^\\/]}
       ) {
        $vol ~~ rx:perl5{([\\/])};
        my $sep = $0 ?? $0 !! "\\";
        $vol ~= $sep;
    }
    $vol ~= $file;
    return $vol;
}

## Misc

sub canonpath (Str $_path) returns Str is export {
    # take a copy of our args here, maybe
    # replace this with 'is copy' parameter
    # trait at some point
    my $path = $_path;
    my $orig_path = $path;
    $path ~~ s:perl5[^([a-z]:)][{uc$0}];
    $path ~~ s:perl5:g{/}{\\};
    $path ~~ s:perl5:g{([^\\])\\+}{$0\\};                                                 # xx\\\\xx  -> xx\xx
    $path ~~ s:perl5:g{(\\\.)+\\}{\\};                                           # xx\.\.\xx -> xx\xx
    $path ~~ s:perl5{^(\.\\)+}{} unless $path eq ".\\";                             # .\xx      -> xx
    $path ~~ s:perl5{\\\Z(?!\n)}{} unless $path ~~ rx:perl5{^([A-Z]:)?\\\Z(?!\n)};  # xx\       -> xx
    # xx1/xx2/xx3/../../xx -> xx1/xx
    $path ~~ s:perl5:g{\\\.\.\.\\}{\\\.\.\\\.\.\\};                                # \...\ is 2 levels up
    $path ~~ s:perl5:g{^\.\.\.\\}{\.\.\\\.\.\\};                                   # ...\ is 2 levels up
    return $path if $path ~~ rx:perl5{^\.\.};                                       # skip relative paths
    return $path unless $path ~~ rx:perl5{\.\.};                                    # too few .'s to cleanup
    return $path if $path ~~ rx:perl5{\.\.\.\.};                                    # too many .'s to cleanup
    $path ~~ s:perl5{^\\\.\.$}{\\};                                                 # \..    -> \
    1 while $path ~~ s:perl5{^\\\.\.}{};                                            # \..\xx -> \xx

    my ($vol, $dirs, $file) = splitpath($path);
    my @dirs = splitdir($dirs);
    my (@base_dirs, @path_dirs);
    my $use_base_dirs = 1;
    for (@dirs) -> $dir {
        $use_base_dirs = 0 if $dir eq updir();
        if ($use_base_dirs) {
            push(@base_dirs, $dir);
        }
        else {
            push(@path_dirs, $dir);
        }
    }
    # for each .. in @path_dirs pop one item from
    # @base_dirs
    my $dir;
    while ($dir = shift(@path_dirs)){
        unless ($dir eq updir()){
            unshift(@path_dirs, $dir);
            last();
        }
        pop(@base_dirs);
    }
    $path = catpath($vol, catdir(@base_dirs, @path_dirs), $file);
    return $path;
}

# Refacted this into a Junction instead of the
# regexp since all it does it remove . and ..
sub no_upwards (*@filenames) returns Array is export {
    @filenames.grep:{ $_ ne ('.' & '..') }
}

sub path returns Array is export {
    my $path = %*ENV{'PATH'} || %*ENV{'Path'} || %*ENV{'path'};
    return split(';', $path).map:{ $_ eq '' ?? '.' !! $_ };
}

sub file_name_is_absolute (Str $file) returns Bool is export {
    ?($file ~~ rx:perl5{^([a-zA-Z]:)?[\\/]})
}

# This HACK is worse than
# the File::Spec platform hack
#sub cwd returns Str is export {
#    my @retval = system("cd");
#    my $cwd = @retval[0];
#    chomp($cwd);
#    return $cwd;
#}
sub cwd returns Str is export {
  return File::Spec::cwd();
}

sub tmpdir returns Str is export {
  return File::Spec::tmpdir();
}

sub rel2abs (Str $_path, Str ?$_base) returns Str is export {
    # take a copy of our args here, maybe
    # replace this with 'is copy' parameter
    # trait at some point
    my $path = $_path;
    if (!file_name_is_absolute($path)) {
        my $base;
        if (!$_base.defined || $_base eq '') {
            $base = cwd();
        }
        elsif (!file_name_is_absolute($_base)) {
            $base = rel2abs($_base);
        }
        else {
            $base = canonpath($_base);
        }
        my ($path_directories, $path_file) = (splitpath($path, 1))[1,2];
        my ($base_volume, $base_directories) = splitpath($base, 1);
        $path = catpath($base_volume, catdir($base_directories, $path_directories), $path_file);
    }
    return canonpath($path);
}

sub abs2rel (Str $_path, Str ?$_base) returns Str is export {
    my $base;
    if (defined($_base) && $_base ne '') {
        # take a copy of our args here, maybe
        # replace this with 'is copy' parameter
        # trait at some point
        $base = $_base;
    }
    else {
        $base = cwd();
    }
    my $base = canonpath($base);
    my $path = canonpath($_path);

    my ($path_volume) = splitpath($path, 1);
    my ($base_volume) = splitpath($base, 1);
    # Can't relativize across volumes
    return $path unless $path_volume eq $base_volume;

    $path = rel2abs($path);
    $base = rel2abs($base);

    my $path_directories = (splitpath($path, 1))[1];
    my $base_directories = (splitpath($base, 1))[1];

    # Now, remove all leading components that are the same
    my @pathchunks = splitdir($path_directories);
    my @basechunks = splitdir($base_directories);

    while (@pathchunks && @basechunks && lc(@pathchunks[0]) eq lc(@basechunks[0])) {
        shift(@pathchunks);
        shift(@basechunks);
    }
    my $result_dirs = catdir((updir) xx @basechunks, @pathchunks);
    return canonpath(catpath("", $result_dirs, ""));
}


=kwid

= NAME

File::Spec::Win32 - Part of Perl 6/Pugs Portable file handling

= SYNOPOSIS

  use File::Spec::Win32;

= DESCRIPTION

This is a very primitive port of the Perl 5 File::Spec::Win32 module.

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

The Perl 5 version of File::Spec::Win32, although this version is more
akin to File::Spec::Functions.

= AUTHOR

Stevan Little <stevan@iinteractive.com>

Max Maischein <corion@cpan.org>

= ACKNOWLEDGEMENTS

This is a port of the Perl 5 File::Spec::Win32 module which is currently
maintained by Ken Williams <KWILLIAMS@cpan.org>, and is written
by a number of people. Please see that module for more information.

= COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
