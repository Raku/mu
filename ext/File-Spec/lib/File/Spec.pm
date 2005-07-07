use v6;

if ($?OS eq 'MSWin32') {
    require File::Spec::Win32;
}
else {
    require File::Spec::Unix;
}

# The "require" lines above is deliberately
# not part of the File::Spec package, because
# we'd like to import on behalf of the caller.
# Again, a horrible hack.

module File::Spec-0.0.1;

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
