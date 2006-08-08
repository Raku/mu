package Pugs::Grammar::Base;
use Pugs::Runtime::Match;
use Pugs::Compiler::RegexPerl5;

# This class defines <ws>, unicode character classes, etc

# internal methods - not in spec

# *no_match = Pugs::Compiler::RegexPerl5->compile( 
#    '\²\$\£\%\"\^\¬' 
# )->code;
sub no_match {
    my $grammar = shift;
    my $pos = $_[1]{p} || 0;
    return Pugs::Runtime::Match->new( { 
        bool    => \0,
        str     => \$_[0],
        match   => [],
        from    => \$pos,
        to      => \$pos,
        capture => undef,
    } );
}

*any = Pugs::Compiler::RegexPerl5->compile( 
    '.' 
)->code;

# specced methods

*ws = Pugs::Compiler::RegexPerl5->compile( 
    '(?:(?<!\w)|(?!\w)|\s)\s*' 
)->code;

BEGIN {
    # this list was extracted from 'perlre'
    for ( qw( 
        alpha alnum ascii blank
        cntrl digit graph lower
        print punct space upper
        word  xdigit
    ) ) {
        *{$_} = Pugs::Compiler::RegexPerl5->compile( 
            "[[:$_:]]"
        )->code;
    }
}

1;
