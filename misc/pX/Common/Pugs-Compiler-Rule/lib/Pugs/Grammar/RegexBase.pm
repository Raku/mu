package Pugs::Grammar::RegexBase;
use Pugs::Runtime::Match::Ratchet;
use Pugs::Compiler::RegexPerl5;

# This class defines <ws>, unicode character classes, etc

# internal methods - not in spec

*no_match = Pugs::Compiler::RegexPerl5->compile( 
    '\²\$\£\%\"\^\¬' 
)->code;

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
