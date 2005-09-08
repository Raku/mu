#!/usr/bin/pugs

use v6;

my $pugs = (($*OS ~~ any<MSWin32 cygwin msys>) ?? "pugs.exe" !! "./pugs");
#say "using $pugs";

my $pipe = Pipe::open("$pugs -V", :r);

print "got> $_" for =$pipe;
