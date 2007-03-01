# Compile MiniPerl6 to Pugs, using mp6-pugs.pl

mkdir libpugs-new
mkdir libpugs-new/MiniPerl6
mkdir libpugs-new/MiniPerl6/Grammar
mkdir libpugs-new/MiniPerl6/Emitter
mkdir libpugs-new/MiniPerl6/Perl5
mkdir libpugs-new/MiniPerl6/Pugs
mkdir libpugs-new/MiniPerl6/Parrot
mkdir libpugs-new/MiniPerl6/PAST

cat lib/MiniPerl6/Emitter/Token.pm  |   \
    perl mp6-pugs.pl      >   \
    libpugs-new/MiniPerl6/Emitter/Token.pm

cat lib/MiniPerl6/Grammar/Control.pm  | \
    perl mp6-pugs.pl      >   \
    libpugs-new/MiniPerl6/Grammar/Control.pm

cat lib/MiniPerl6/Grammar/Mapping.pm  | \
    perl mp6-pugs.pl      >   \
    libpugs-new/MiniPerl6/Grammar/Mapping.pm

cat lib/MiniPerl6/Grammar/Regex.pm  |   \
    perl mp6-pugs.pl      >   \
    libpugs-new/MiniPerl6/Grammar/Regex.pm

cat lib/MiniPerl6/Grammar.pm        |   \
    perl mp6-pugs.pl      >   \
    libpugs-new/MiniPerl6/Grammar.pm

cat lib/MiniPerl6/Perl5/Emitter.pm  |   \
    perl mp6-pugs.pl      >   \
    libpugs-new/MiniPerl6/Perl5/Emitter.pm

cat lib/MiniPerl6/Pugs/Emitter.pm  |   \
    perl mp6-pugs.pl      >   \
    libpugs-new/MiniPerl6/Pugs/Emitter.pm

cat lib/MiniPerl6/Parrot/Emitter.pm  |   \
    perl mp6-pugs.pl      >   \
    libpugs-new/MiniPerl6/Parrot/Emitter.pm

cat lib/MiniPerl6/PAST/Emitter.pm  |   \
    perl mp6-pugs.pl      >   \
    libpugs-new/MiniPerl6/PAST/Emitter.pm

cp lib/MiniPerl6/Perl5/Match.pm         \
   libpugs-new/MiniPerl6/Perl5/Match.pm

cp lib/MiniPerl6/Perl5/Runtime.pm       \
   libpugs-new/MiniPerl6/Perl5/Runtime.pm

#cat lib/MiniPerl6/AST/CompUnit.pm  |   \
#    perl mp6-pugs.pl      >   \
#    libpugs-new/MiniPerl6/AST/CompUnit.pm

