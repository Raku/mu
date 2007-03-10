package Pugs::Grammar::Base;
use Pugs::Runtime::Match;
use Pugs::Compiler::RegexPerl5;
use Pugs::Compiler::Regex;
use Data::Dumper;

use charnames ":full";   # support \c[DIGIT SIX]

# This class defines <ws>, unicode character classes, etc

# internal methods - not in spec

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

*fail = \&no_match;

*any = Pugs::Compiler::RegexPerl5->compile( 
    '.' 
)->code;

# <<word
*_wb_left = Pugs::Compiler::RegexPerl5->compile( 
    '\b(?=\w)' 
)->code;

# word>>
*_wb_right = Pugs::Compiler::RegexPerl5->compile( 
    '(?<=\w)\b' 
)->code;

=for later - unused
# \h
*_horizontal_ws = Pugs::Compiler::RegexPerl5->compile( 
    #'XXX - Infinite loop in pugs stdrules.t' .
    '\x20|\t'
    
    #'\x0a|\x0b|\x0c|\x0d|\x85'   
    # from regex_tests, plus \t and ' '
)->code;

# \v
*_vertical_ws = Pugs::Compiler::RegexPerl5->compile( 
    #'XXX - Infinite loop in pugs stdrules.t' .
    '[\n\r]'
    
    #'\x{1680}|\x{180e}|\x{2000}|\x{2001}|\x{2002}|\x{2003}|\x{2004}|\x{2005}|\x{2006}|\x{2007}|\x{2008}|\x{2008}|\x{2009}|\x{200a}|\x{202f}|\x{205f}|\x{3000}'   
    # from regex_tests
)->code;
=cut

# specced methods

sub at {
    my $grammar = shift;
    my $pos = $_[1]{p} || 0;
    my ( $arg ) = keys %{ $_[1]{args} };  # XXX positional
    # print "at: ",Dumper( @_ );
    return Pugs::Runtime::Match->new( { 
        bool    => \( $pos == $arg ),
        str     => \$_[0],
        match   => [],
        from    => \$pos,
        to      => \$pos,
        capture => undef,
    } );
}

sub prior {
    die "Error: <prior> is undefined" 
        unless defined $main::_V6_PRIOR_;

    my $prior = $main::_V6_PRIOR_;
    ## local $main::_V6_PRIOR_;
    $prior->(@_[0, 1, 2, 2]);  # XXX fix parameter list
}

*null = Pugs::Compiler::RegexPerl5->compile( 
    '' 
)->code;

*ws = Pugs::Compiler::RegexPerl5->compile( 
    '(?:(?<!\w)|(?!\w)|\s)\s*' 
)->code;

# <wb> = word boundary - from regex_tests
*wb = Pugs::Compiler::RegexPerl5->compile( 
    '\b' 
)->code;

*ident = Pugs::Compiler::RegexPerl5->compile( 
    '[[:alpha:]_][[:alnum:]_]*' 
)->code;

*name = Pugs::Compiler::RegexPerl5->compile( 
    # from pugs tests
    '(?:[[:alpha:]_][[:alnum:]_]*::)*[[:alpha:]_][[:alnum:]_]*' 
)->code;

*sp = Pugs::Compiler::RegexPerl5->compile( 
    '\x20' 
)->code;

*dot = Pugs::Compiler::RegexPerl5->compile( 
    '\.' 
)->code;

*gt = Pugs::Compiler::RegexPerl5->compile( 
    '>' 
)->code;

*lt = Pugs::Compiler::RegexPerl5->compile( 
    '<' 
)->code;

#BEGIN {
#    # this list was extracted from 'perlre'
#    for ( qw( 
#        alpha alnum ascii blank
#        cntrl digit graph lower
#        print punct space upper
#        word  xdigit
#    ) ) {
#        *{$_} = Pugs::Compiler::RegexPerl5->compile( 
#            "[[:$_:]]"
#        )->code;
#    }
#}

    sub AUTOLOAD {
        #my $self = shift;
        my $meth = $AUTOLOAD;
        $meth =~ s/.*:://;   # strip fully-qualified portion
        
        # is it a Unicode property? "isL"
        {
          local $@;
          eval ' my $s="a"; $s =~ /\p{' . $meth . '}/ ';
          unless ( $@ ) {
            *{$meth} = Pugs::Compiler::RegexPerl5->compile( 
              '\p{' . $meth . '}'
            )->code;
            return $meth->( @_ );
          }
        }
        
        # is it a char class? "digit"
        {
          local $@;
          eval ' my $s="a"; $s =~ /[[:' . $meth . ':]]/ ';
          unless ( $@ ) {
            *{$meth} = Pugs::Compiler::RegexPerl5->compile( 
              '[[:' . $meth . ':]]'
            )->code;
            return $meth->( @_ );
          }
        }
        
        die "unknown rule: <$meth>";
    }    

1;
