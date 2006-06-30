package Pugs::Grammar::Term;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Runtime::Match;
use Pugs::Compiler::Token;

# TODO - implement the "magic hash" dispatcher
# TODO - term:<...>  - yada-yada-yada
# moose=>1
# moose:<elk>
# moose:{antler()}

our %hash;

sub pair {
    my $class = shift;
    return $class->no_match unless $_[0];
    #print "match pair $_[0]\n";
    return Pugs::Runtime::Match->new( { 
        bool  => 1,
        match => $1,
        tail  => $3,
        capture => { 
            pair => { key => { single_quoted => $1 }, value => { single_quoted => $2 } } 
        },
    } )
        if $_[0] =~ /^:([_\w]+)<(.*?)>(.*)$/s;
    return $class->no_match;
};

sub cpan_bareword {
    my $class = shift;
    return $class->no_match unless $_[0];
    return Pugs::Runtime::Match->new( { 
        bool  => 1,
        match => $1,
        tail  => $2,
        capture => { cpan_bareword => $1 },
    } )
        if $_[0] =~ /^ ([_\w\d]+ \- [_\w\d\-\.]+) ( (?: \(|\;|\s|$ ) .*)$/sx;
    return $class->no_match;
};

sub substitution {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( "/" . $_[0], "/" );
    $extracted = substr( $extracted, 1, -1 ) if length($extracted) > 1;
    my $extracted2;
    ($extracted2,$remainder) = Text::Balanced::extract_delimited( "/" . $remainder, "/" );
    $extracted2 = substr( $extracted2, 1, -1 ) if length($extracted2) > 1;
    return Pugs::Runtime::Match->new( { 
        bool  => 1, # ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        capture => [ $extracted, $extracted2 ],
    } );
};

sub single_quoted {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( "'" . $_[0], "'" );
    $extracted = substr( $extracted, 1, -1 ) if length($extracted) > 1;
    return Pugs::Runtime::Match->new( { 
        bool  => 1, # ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        capture => $extracted,
    } );
}

sub double_quoted {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( '"' . $_[0], '"' );
    $extracted = substr( $extracted, 1, -1 ) if length($extracted) > 1;
    return Pugs::Runtime::Match->new( { 
        bool  => 1, # ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        capture => $extracted,
    } );
}

sub angle_quoted {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_bracketed( '<' . $_[0], '<..>' );
    $extracted = substr( $extracted, 1, -1 ) if length($extracted) > 1;
    return Pugs::Runtime::Match->new( { 
        bool  => 1, # ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        capture => $extracted,
    } );
}

*ident = Pugs::Compiler::Regex->compile( q(
        \!      # $!
    |   \??     # $?CALLER
        \*?     # $*x
        # \.?     # $.x  - XXX causes problems with 1..5 for some reason
        \:?     # $:x
        [
            [ \:\: ]?
            [ _ | <?alnum> ]+
        ]+
    |   <before \< | \[ | \{ >   # $<thing> == $/<thing>; $[thing] = $/[thing]
    |   \/      # $/
) )->code;

sub recompile {
    my $class = shift;
    %hash = (
        '$' => Pugs::Compiler::Regex->compile( q(
                <?Pugs::Grammar::Term.ident>
                { return { scalar => '$' . $_[0]->() ,} }
            ) ),
        '$.' => Pugs::Compiler::Regex->compile( q(
                <?Pugs::Grammar::Term.ident>
                { return { scalar => '$.' . $_[0]->() ,} }
            ) ),
        '@' => Pugs::Compiler::Regex->compile( q(
                <?Pugs::Grammar::Term.ident>
                { return { array => "\@" . $_[0]->() ,} }
            ) ),
        '%' => Pugs::Compiler::Regex->compile( q(
                <?Pugs::Grammar::Term.ident>
                { return { hash  => "\%" . $_[0]->() ,} }
            ) ),
        '&' => Pugs::Compiler::Regex->compile( q(
                <?Pugs::Grammar::Term.ident>
                { return { code  => "\&" . $_[0]->() ,} }
            ) ),

        '...' => Pugs::Compiler::Regex->compile( q(
            { 
                return { die => "not implemented" } 
            }
        ) ),
        Inf => Pugs::Compiler::Regex->compile( q(
            { return { num => 'Inf' ,} } 
        ) ),
        NaN => Pugs::Compiler::Regex->compile( q(
            { return { num => 'NaN' ,} } 
        ) ),
        'bool::true' => Pugs::Compiler::Regex->compile( q(
            { return { bool => 1 ,} } 
        ) ),
        'bool::false' => Pugs::Compiler::Regex->compile( q(
            { return { bool => 0 ,} } 
        ) ),
        q(') => Pugs::Compiler::Regex->compile( q(
            <Pugs::Grammar::Term.single_quoted>
            { return { single_quoted => $/{'Pugs::Grammar::Term.single_quoted'}->() ,} }
        ) ),
        q(") => Pugs::Compiler::Regex->compile( q(
            <Pugs::Grammar::Term.double_quoted>
            { return { double_quoted => $/{'Pugs::Grammar::Term.double_quoted'}->() ,} }
        ) ),
        q(s:g/) => Pugs::Compiler::Regex->compile( q(
            <Pugs::Grammar::Term.substitution>
            { return { 
                    substitution => $/{'Pugs::Grammar::Term.substitution'}->(),
                    options => { g => 1 },
                } 
            }
        ) ),
        # angle is handled by the lexer
        #q(<) => Pugs::Compiler::Regex->compile( q(
        #    <Pugs::Grammar::Term.angle_quoted>
        #    { return { angle_quoted => $/{'Pugs::Grammar::Term.angle_quoted'}->() ,} }
        #) ),
        #~ q(.) => Pugs::Compiler::Regex->compile( q(
            #~ <Pugs::Grammar::Term.bareword>
            #~ { return { method => $/{'Pugs::Grammar::Term.bareword'}->() ,} }
        #~ ) ),
        q() => Pugs::Compiler::Regex->compile( q!
                ### floating point
                \d+\.\d+ { return { num => $() ,} } 
            |
                ### number
                \d+ { return { int => $() ,} } 
            |
                ### long:<name> 
                <Pugs::Grammar::Term.pair>
                { return $/{'Pugs::Grammar::Term.pair'}->() }
            #~ |
                #~ ### func(... func.(...
                #~ <Pugs::Grammar::Term.sub_call> 
                #~ { return $/{'Pugs::Grammar::Term.sub_call'}->() }
            |
                ### Test-0.0.6
                <Pugs::Grammar::Term.cpan_bareword> 
                { return $/{'Pugs::Grammar::Term.cpan_bareword'}->() }
            |
                ### Test::More
                <Pugs::Grammar::Term.ident> 
                { return { bareword => $/{'Pugs::Grammar::Term.ident'}->() } }
            #|
            #    ### v6
            #    <Pugs::Grammar::Term.bareword> 
            #    { return $/{'Pugs::Grammar::Term.bareword'}->() }
        ! ),
    );
    $class->SUPER::recompile;
}

BEGIN {
    __PACKAGE__->recompile;
}

1;
