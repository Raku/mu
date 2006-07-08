use v6-alpha;

my $pugs = (($*OS ~~ any <MSWin32 cygwin msys>) ?? "pugs.exe" !! "./pugs");
#say "using $pugs";

my $pipe = Pipe::open("$pugs -V", :r);

print "got> $_\n" for =$pipe;
