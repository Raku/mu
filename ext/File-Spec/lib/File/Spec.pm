use v6;

module File::Spec-0.0.1;

my $worker_pkg
    =  $?OS eq 'MSWin32' ?? 'File::Spec::Win32'
    !!                      'File::Spec::Unix'
    ;

eval "require $worker_pkg;";
die $!
    if $!;

sub curdir returns Str is export {
    return &::($worker_pkg)::curdir();
}

sub updir returns Str is export {
    return &::($worker_pkg)::updir();
}

sub rootdir returns Str is export {
    return &::($worker_pkg)::rootdir();
}

sub devnull returns Str is export {
    return &::($worker_pkg)::devnull();
}

sub case_tolerant returns Bool is export {
    return &::($worker_pkg)::case_tolerant();
}

sub splitdir (Str $dir) returns Array is export {
    return &::($worker_pkg)::splitdir( $dir );
}

sub splitpath (Str $path, Bool $nofile?) returns Array is export {
    return &::($worker_pkg)::splitpath( $path, $nofile );
}

sub catdir (*@path) returns Str is export {
    return &::($worker_pkg)::catdir( *@path );
}

sub catfile (*@_path) returns Str is export {
    return &::($worker_pkg)::catfile( *@_path );
}

sub catpath (Str $volume, Str $directory, Str $file) returns Str is export {
    return &::($worker_pkg)::catpath( $volume, $directory, $file );
}

sub rel2abs (Str $_path, Str $_base?) returns Str is export {
    return &::($worker_pkg)::rel2abs( $_path, $_base );
}

sub abs2rel (Str $_path, Str $_base?) returns Str is export {
    return &::($worker_pkg)::abs2rel( $_path, $_base );
}

sub canonpath (Str $_path) returns Str is export {
    return &::($worker_pkg)::canonpath( $_path );
}

sub no_upwards (*@filenames) returns Array is export {
    return &::($worker_pkg)::no_upwards( *@filenames );
}

sub file_name_is_absolute (Str $file) returns Bool is export {
    return &::($worker_pkg)::file_name_is_absolute( $file );
}

sub path returns Array is export {
    return &::($worker_pkg)::path();
}

sub cwd returns Str is export {
    return &::($worker_pkg)::cwd();
}

sub tmpdir returns Str is export {
    return &::($worker_pkg)::tmpdir();
}

=kwid

= NAME

File::Spec - Perl 6/Pugs Portable file handling

= SYNOPOSIS

  use File::Spec;
  
  catdir('path', 'to', 'dir');
  catfile('path', 'to', 'file');
  
  my @path = splitdir('/path/to/dir'); 
  
  # ... etc etc etc

= DESCRIPTION

This is a very primative port of the Perl 5 File::Spec module. Since 
we currently do not have objects or fully functioning modules in 
Pugs, this port does it's best to work with those limitations and 
still produce a working version of File::Spec for use as we develop 
Pugs. 

= LIMITATIONS & CAVEATS

Since we don't yet have object support, this module is more like the
Perl 5 File::Spec::Functions module than the base File::Spec. Also the
/hack/ to make it work automagically for platforms is really bad. I 
hope that as Pugs matures this will change, but for now, it works :).

= PLATFORM SUPPORT

Currently we only support Win32 and (basic) Unix since this is what
GHC and Pugs currently run on (yeah no VMS !!). 

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

The Perl 5 version of File::Spec, although this version is more akin 
to File::Spec::Functions.

= AUTHOR

Stevan Little <stevan@iinteractive.com>

Max Maischein <corion@cpan.org>

= ACKNOWLEDGEMENTS

This is a port of the Perl 5 File::Spec module which is currently 
maintained by Ken Williams <KWILLIAMS@cpan.org>, and is written
by a number of people. Please see that module for more information.

= COPYRIGHT 

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
