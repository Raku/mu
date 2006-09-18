package Pugs::Grammar::Base;
use Pugs::Runtime::Match;
use Pugs::Compiler::RegexPerl5;
use Pugs::Compiler::Regex;
use Data::Dumper;

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

sub prior {
    warn "Error: <prior> is undefined" 
        unless defined $main::_V6_PRIOR_;
    return $main::_V6_PRIOR_->( @_[0,1,2,2] );  # XXX fix parameter list
}

*null = Pugs::Compiler::RegexPerl5->compile( 
    '' 
)->code;

*ws = Pugs::Compiler::RegexPerl5->compile( 
    '(?:(?<!\w)|(?!\w)|\s)\s*' 
)->code;

*ident = Pugs::Compiler::RegexPerl5->compile( 
    '[[:alpha:]_][[:alnum:]_]*' 
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
