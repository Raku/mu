# Compile MiniPerl6 to Perl5, using v6.pm

mkdir lib5
mkdir lib5/MiniPerl6
mkdir lib5/MiniPerl6/Grammar
mkdir lib5/MiniPerl6/Emitter
mkdir lib5/MiniPerl6/Perl5

cat lib/MiniPerl6/Emitter/Token.pm  |   \
    perl -Ilib mp6-perl5-v6.pl      >   \
    lib5/MiniPerl6/Emitter/Token.pm

cat lib/MiniPerl6/Grammar/Control.pm  | \
    perl -Ilib mp6-perl5-v6.pl      >   \
    lib5/MiniPerl6/Grammar/Control.pm

cat lib/MiniPerl6/Grammar/Mapping.pm  | \
    perl -Ilib mp6-perl5-v6.pl      >   \
    lib5/MiniPerl6/Grammar/Mapping.pm

cat lib/MiniPerl6/Grammar/Regex.pm  |   \
    perl -Ilib mp6-perl5-v6.pl      >   \
    lib5/MiniPerl6/Grammar/Regex.pm

cat lib/MiniPerl6/Grammar.pm        |   \
    perl -Ilib mp6-perl5-v6.pl      >   \
    lib5/MiniPerl6/Grammar.pm

cat lib/MiniPerl6/Perl5/Emitter.pm  |   \
    perl -Ilib mp6-perl5-v6.pl      >   \
    lib5/MiniPerl6/Perl5/Emitter.pm

cp lib/MiniPerl6/Perl5/Match.pm         \
   lib5/MiniPerl6/Perl5/Match.pm

cp lib/MiniPerl6/Perl5/Runtime.pm       \
   lib5/MiniPerl6/Perl5/Runtime.pm
