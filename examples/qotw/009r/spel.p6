#!/usr/local/bin/pugs

# Regular QOTW #9
# http://perl.plover.com/qotw/r/solution/009

# This implementation assumes that where the QOTW author refers to
# ".spel files", he is talking about files that end with the text
# ".spel". So, for instance, a filename of "dominus.spel" is considered
# a ".spel file"

my @speldirs = split /\:+/, %ENV<SPELWORDS> || "%ENV<HOME>:.";
my @spelfiles;
for @speldirs -> $d {
   push @spelfiles, map { "$d/$_" } grep { $_ ~~ /.spel$/ } readdir $d;
}

my %WORDS;
for '/usr/dict/words', *@spelfiles -> $f {
   -f $f or next;
   my $F = open $f or next;
   for =$F -> $w is copy { $w.=chomp; %WORDS{lc $w} = 1; }
   $F.close;
}
my %bad;
for =<> {
   my @words = split /\s+/, $_;
   for @words -> $w { %bad{$w}++ unless %WORDS{lc $w}; }
}
say for sort keys %bad;
