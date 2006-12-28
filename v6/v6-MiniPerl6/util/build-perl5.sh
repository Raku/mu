# Compile MiniPerl6 to Perl5, using mp6-perl5-boot.pl

mkdir lib5-new
mkdir lib5-new/MiniPerl6
mkdir lib5-new/MiniPerl6/Grammar
mkdir lib5-new/MiniPerl6/Emitter
mkdir lib5-new/MiniPerl6/Perl5
mkdir lib5-new/MiniPerl6/Perl5MO
mkdir lib5-new/MiniPerl6/Parrot

cat lib/MiniPerl6/Emitter/Token.pm  |   \
    perl mp6-perl5-boot.pl      >   \
    lib5-new/MiniPerl6/Emitter/Token.pm

cat lib/MiniPerl6/Grammar/Control.pm  | \
    perl mp6-perl5-boot.pl      >   \
    lib5-new/MiniPerl6/Grammar/Control.pm

cat lib/MiniPerl6/Grammar/Mapping.pm  | \
    perl mp6-perl5-boot.pl      >   \
    lib5-new/MiniPerl6/Grammar/Mapping.pm

cat lib/MiniPerl6/Grammar/Regex.pm  |   \
    perl mp6-perl5-boot.pl      >   \
    lib5-new/MiniPerl6/Grammar/Regex.pm

cat lib/MiniPerl6/Grammar.pm        |   \
    perl mp6-perl5-boot.pl      >   \
    lib5-new/MiniPerl6/Grammar.pm

cat lib/MiniPerl6/Perl5/Emitter.pm  |   \
    perl mp6-perl5-boot.pl      >   \
    lib5-new/MiniPerl6/Perl5/Emitter.pm

cat lib/MiniPerl6/Perl5MO/Emitter.pm  |   \
    perl mp6-perl5-boot.pl      >   \
    lib5-new/MiniPerl6/Perl5MO/Emitter.pm

cat lib/MiniPerl6/Parrot/Emitter.pm  |   \
    perl mp6-perl5-boot.pl      >   \
    lib5-new/MiniPerl6/Parrot/Emitter.pm

cp lib/MiniPerl6/Perl5/Match.pm         \
   lib5-new/MiniPerl6/Perl5/Match.pm

cp lib/MiniPerl6/Perl5/Runtime.pm       \
   lib5-new/MiniPerl6/Perl5/Runtime.pm

cat lib/MiniPerl6/AST/CompUnit.pm  |   \
    perl mp6-perl5-boot.pl      >   \
    lib5-new/MiniPerl6/AST/CompUnit.pm

