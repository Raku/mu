#!/usr/bin/pugs

use v6;

use Prelude;

my $pugs = (($*OS ~~ any<MSWin32 cygwin msys>) ?? "pugs.exe" :: "./pugs");
#say "using $pugs";

my $pipe = openpipe("$pugs -V", :r);

print "got> $_" for =$pipe;
