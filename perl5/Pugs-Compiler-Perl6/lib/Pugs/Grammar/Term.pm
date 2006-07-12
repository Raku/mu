package Pugs::Grammar::Term;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Runtime::Match;
use Pugs::Compiler::Token;

# TODO - implement the "magic hash" dispatcher
# TODO - term:<...> !!! ??? 
# moose=>1
# moose:<elk>
# moose:{antler()}

our %hash;

sub pair {
    my $class = shift;
    return $class->no_match unless $_[0];
    #print "match pair $_[0]\n";
    # :foo<bar>
    return Pugs::Runtime::Match->new( { 
        bool  => 1,
        match => $1,
        tail  => $3,
        capture => { 
            pair => { 
                key   => { single_quoted => $1 }, 
                value => { single_quoted => $2 }, 
        } },
    } )
        if $_[0] =~ /^:([_\w]+)<(.*?)>(.*)$/s;
    # :$foo 
    return Pugs::Runtime::Match->new( { 
        bool  => 1,
        match => $1,
        tail  => $2,
        capture => { 
            pair => { 
                key   => { single_quoted => $1 }, 
                value => { scalar => '$'.$1 }, 
        } },
    } )
        if $_[0] =~ /^:\$([_\w]+)(.*)$/s;
    # :foo 
    return Pugs::Runtime::Match->new( { 
        bool  => 1,
        match => $1,
        tail  => $2,
        capture => { 
            pair => { 
                key   => { single_quoted => $1 }, 
                value => { num => 1 },
        } },
    } )
        if $_[0] =~ /^:([_\w]+)(.*)$/s;
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
        if $_[0] =~ /^ ([_\w\d]+ \- [_\w\d\-\.*]+) ( (?: \(|\;|\s|$ ) .*)$/sx;
    return $class->no_match;
};

sub substitution {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my $options;
    while ($_[0] =~ s/^:(\w+)//) {
	$options->{lc($1)} = 1;
    }
    return $grammar->no_match unless substr($_[0], 0 , 1) eq '/';
    substr($_[0], 0, 1, '');
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( "/" . $_[0], "/" );
    return $grammar->no_match unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    my $extracted2;
    ($extracted2,$remainder) = Text::Balanced::extract_delimited( "/" . $remainder, "/" );
    return $grammar->no_match unless length($extracted2) > 0;
    $extracted2 = substr( $extracted2, 1, -1 );
    return Pugs::Runtime::Match->new( { 
        bool    => 1,,
        match   => $extracted,
        tail    => $remainder,
        capture => { options => $options, substitution => [$extracted, $extracted2] },
    } );
};

sub single_quoted {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( "'" . $_[0], "'" );
    return $grammar->no_match unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    return Pugs::Runtime::Match->new( { 
        bool    => 1,,
        match   => $extracted,
        tail    => $remainder,
        capture => $extracted,
    } );
}

sub double_quoted {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( '"' . $_[0], '"' );
    return $grammar->no_match unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    return Pugs::Runtime::Match->new( { 
        bool    => 1, 
        match   => $extracted,
        tail    => $remainder,
        capture => $extracted,
    } );
}

sub angle_quoted {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_bracketed( '<' . $_[0], '<..>' );
    return $grammar->no_match unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    return Pugs::Runtime::Match->new( { 
        bool    => 1,
        match   => $extracted,
        tail    => $remainder,
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
            [ _ | <?alpha> ]
            [ _ | <?alnum> ]*
        ]+
    |   <before \< | \[ | \{ >   # $<thing> == $/<thing>; $[thing] = $/[thing]
    |   \/      # $/
) )->code;

*parenthesis = Pugs::Compiler::Regex->compile( q(
                <?ws>? <Pugs::Grammar::Perl6.perl6_expression> <?ws>? 
                <'\)'>
                { return {
                    op1 => { op => "(" },
                    op2 => { op => ")" },
                    fixity => "circumfix",
                    exp1 => $_[0]{'Pugs::Grammar::Perl6.perl6_expression'}->() 
                } }
            |
                <?ws>? <Pugs::Grammar::Perl6.block> <?ws>? 
                <'\)'>
                { return {
                    op1 => { op => "(" },
                    op2 => { op => ")" },
                    fixity => "circumfix",
                    exp1 => $_[0]{'Pugs::Grammar::Perl6.block'}->() 
                } }
            |
                <?ws>? 
                <'\)'>
                { return {
                    op1 => { op => "(" },
                    op2 => { op => ")" },
                    fixity => "circumfix",
                } }
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

        '(' => Pugs::Compiler::Regex->compile( q(
                <Pugs::Grammar::Term.parenthesis>
                { return $_[0]{'Pugs::Grammar::Term.parenthesis'}->() }
            ) ),
        '{' => Pugs::Compiler::Regex->compile( q(
                <?ws>? <Pugs::Grammar::Perl6.statements_or_null> <?ws>? <'}'>
                { 
                  return { 
                    bare_block => $_[0]{'Pugs::Grammar::Perl6.statements_or_null'}->(),
                } }
            ) ),


        '->' => Pugs::Compiler::Regex->compile( q( 
        [
            <?ws>? <Pugs::Grammar::Perl6.perl6_expression('no_blocks',0)> <?ws>? 
            \{ <?ws>? <Pugs::Grammar::Perl6.statements_or_null> <?ws>? \}
            { return { 
                pointy_block => $_[0]{'Pugs::Grammar::Perl6.statements_or_null'}->(),
                signature    => $_[0]{'Pugs::Grammar::Perl6.perl6_expression'}->(),
            } }
        |
            <?ws>?
            \{ <?ws>? <Pugs::Grammar::Perl6.statements_or_null> <?ws>? \}
            { return { 
                pointy_block => $_[0]{'Pugs::Grammar::Perl6.statements_or_null'}->(),
                signature    => undef,
            } }
        ]
            ) ),

        '.' => Pugs::Compiler::Regex->compile( q(
                # .method op
                <?Pugs::Grammar::Term.ident>
                { return { dot_bareword  => $_[0]->() ,} }
            ) ),
        '...' => Pugs::Compiler::Regex->compile( q(
            { 
                return { die => "not implemented" } 
            }
        ) ),
        q(') =>       # ' 
          Pugs::Compiler::Regex->compile( q(
            <Pugs::Grammar::Term.single_quoted>
            { return { single_quoted => $/{'Pugs::Grammar::Term.single_quoted'}->() ,} }
        ) ),
        q(") => Pugs::Compiler::Regex->compile( q(
            <Pugs::Grammar::Term.double_quoted>
            { return { double_quoted => $/{'Pugs::Grammar::Term.double_quoted'}->() ,} }
        ) ),
        q(s) => Pugs::Compiler::Regex->compile( q(
            <Pugs::Grammar::Term.substitution>
            { return { 
                    substitution => $/{'Pugs::Grammar::Term.substitution'}->(),
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
            # |
                ### long:<name> zzzz
                #\:(\w+) ( \( (<Pugs::Grammar::Perl6.perl6_expression('no_blocks',0)>) \) )?
                #{ warn 'orz'; return { pair => { key => { single_quoted => $_[0][0]->() }, value => defined $_[0][1] ? $_[0][1][0][0]->() : { int => 1 } } } }
            |
                ### :long<name> 
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
