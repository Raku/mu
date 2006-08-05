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

*cpan_bareword = Pugs::Compiler::Regex->compile( '
    ([_\w\d\:]+ \- [_\w\d\-\.*]+) 
    (?= \( | \; | \s | $ ) 
', 
    { Perl5 => 1 } 
)->code;

sub substitution {
    my $grammar = shift;
    return $grammar->no_match(@_) unless $_[0];
    my $pos = $_[1]{p} || 0;
    my $s = substr( $_[0], $pos );
    my $options;
    while ($s =~ s/^:(\w+)//) {
	$options->{lc($1)} = 1;
    }
    return $grammar->no_match(@_) unless substr($s, 0 , 1) eq '/';
    substr($s, 0, 1, '');
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( "/" . $s, "/" );
    return $grammar->no_match(@_) unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    my $extracted2;
    ($extracted2,$remainder) = Text::Balanced::extract_delimited( "/" . $remainder, "/" );
    return $grammar->no_match(@_) unless length($extracted2) > 0;
    $extracted2 = substr( $extracted2, 1, -1 );
    return Pugs::Runtime::Match->new( { 
        bool    => \1,
        str     => \$_[0],
        match   => [],
        from    => \$pos,
        to      => \( length($_[0]) - length($remainder) ),
        capture => \{ options => $options, substitution => [$extracted, $extracted2] },
    } );
};

my %openmatch = ( '/' => '/',
                  '{' => '}',
                  '!' => '!',
                  '\'' => '\'');

sub rx {
    my $grammar = shift;
    return $grammar->no_match(@_) unless $_[0];
    my $options;
    while ($_[0] =~ s/^:(\w+)//) {
	$options->{lc($1)} = 1;
    }
    my $open = substr($_[0], 0 , 1, '');
    my $ret = rx_body($grammar, $_[0], { args => { open => $open } });
    $ret->capture->{options} = $options if $ret->bool;
    return $ret;
}

sub rx_body {
    my $grammar = shift;
    use Data::Dumper;
    my $open = $_[1]->{args}{open};
    return $grammar->no_match(@_) unless exists $openmatch{$open};
    my $pos = $_[1]{p} || 0;
    my $s = substr( $_[0], $pos );
    my ($extracted,$remainder) = $open eq $openmatch{$open}
        ? Text::Balanced::extract_delimited( $open . $s, $openmatch{$open} )
	: Text::Balanced::extract_bracketed( $open . $s, $open.$openmatch{$open} );
    return $grammar->no_match(@_) unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    return Pugs::Runtime::Match->new( { 
        bool    => \1,
        str     => \$_[0],
        match   => [],
        from    => \$pos,
        to      => \( length($_[0]) - length($remainder) ),
        capture => \{ rx => $extracted },
    } );
};

sub single_quoted {
    my $grammar = shift;
    return $grammar->no_match(@_) unless $_[0];
    my $pos = $_[1]{p} || 0;
    my $s = substr( $_[0], $pos );
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( "'" . $s, "'" );
    return $grammar->no_match(@_) unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    return Pugs::Runtime::Match->new( { 
        bool    => \1,
        str     => \$_[0],
        match   => [],
        from    => \$pos,
        to      => \( length($_[0]) - length($remainder) ),
        capture => \$extracted,
    } );
}

sub double_quoted {
    my $grammar = shift;
    return $grammar->no_match(@_) unless $_[0];
    my $pos = $_[1]{p} || 0;
    my $s = substr( $_[0], $pos );
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( '"' . $s, '"' );
    return $grammar->no_match(@_) unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    return Pugs::Runtime::Match->new( { 
        bool    => \1,
        str     => \$_[0],
        match   => [],
        from    => \$pos,
        to      => \( length($_[0]) - length($remainder) ),
        capture => \$extracted,
    } );
}

sub angle_quoted {
    my $grammar = shift;
    return $grammar->no_match(@_) unless $_[0];
    my $pos = $_[1]{p} || 0;
    my $s = substr( $_[0], $pos );
    my ($extracted,$remainder) = Text::Balanced::extract_bracketed( '<' . $s, '<..>' );
    return $grammar->no_match(@_) unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    return Pugs::Runtime::Match->new( { 
        bool    => \1,
        str     => \$_[0],
        match   => [],
        from    => \$pos,
        to      => \( length($_[0]) - length($remainder) ),
        capture => \$extracted,
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

*bare_ident = Pugs::Compiler::Token->compile( q(
        [
            [ \:\: ]?
            [ _ | <?alpha> ]
            [ _ | <?alnum> ]*
        ]+
) )->code;

*parenthesis = Pugs::Compiler::Token->compile( q(
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

*brackets = Pugs::Compiler::Token->compile( q(
                <Pugs::Grammar::Infix.parse> 
                <']'>
                { return {
                    op => $_[0]{'Pugs::Grammar::Infix.parse'}->(),
                    reduce => 1, 
                } }
            |
                <?ws>? <Pugs::Grammar::Perl6.perl6_expression> <?ws>? 
                <']'>
                { return {
                    op1 => { op => "[" },
                    op2 => { op => "]" },
                    fixity => "circumfix",
                    exp1 => $_[0]{'Pugs::Grammar::Perl6.perl6_expression'}->() 
                } }
            |
                <?ws>? <Pugs::Grammar::Perl6.block> <?ws>? 
                <']'>
                { return {
                    op1 => { op => "[" },
                    op2 => { op => "]" },
                    fixity => "circumfix",
                    exp1 => $_[0]{'Pugs::Grammar::Perl6.block'}->() 
                } }
            |
                <?ws>? 
                <']'>
                { return {
                    op1 => { op => "[" },
                    op2 => { op => "]" },
                    fixity => "circumfix",
                } }
) )->code;

sub recompile {
    my $class = shift;
    %hash = (
        '$' => Pugs::Compiler::Token->compile( q(
                [ <?Pugs::Grammar::Term.ident>
                  { return { scalar => '$' . $_[0]->() ,} }
                | (\d+)
                  { return { scalar => '$' . $_[0]->() ,} }
                ]
            ) ),
        '$.' => Pugs::Compiler::Token->compile( q(
                <?Pugs::Grammar::Term.ident>
                { return { scalar => '$.' . $_[0]->() ,} }
            ) ),
        '@' => Pugs::Compiler::Token->compile( q(
                # XXX t/subroutines/multidimensional_arglists.t
                \; <?Pugs::Grammar::Term.ident>
                { return { die => "not implemented" } }
            |
                <?Pugs::Grammar::Term.ident>
                { return { array => "\@" . $_[0]->() ,} }
            ) ),
        '%' => Pugs::Compiler::Token->compile( q(
                <?Pugs::Grammar::Term.ident>
                { return { hash  => "\%" . $_[0]->() ,} }
            ) ),
        '&' => Pugs::Compiler::Token->compile( q(
                <?Pugs::Grammar::Term.ident>
                { return { code  => "\&" . $_[0]->() ,} }
            ) ),

        '(' => Pugs::Compiler::Token->compile( q(
                <Pugs::Grammar::Term.parenthesis>
                { return $_[0]{'Pugs::Grammar::Term.parenthesis'}->() }
            ) ),
        '[' => Pugs::Compiler::Token->compile( q(
                <Pugs::Grammar::Term.brackets>
                { return $_[0]{'Pugs::Grammar::Term.brackets'}->() }
            ) ),
        '{' => Pugs::Compiler::Token->compile( q(
                <?ws>? <Pugs::Grammar::Perl6.statements> <?ws>? <'}'>
                { 
                  return { 
                    bare_block => $_[0]{'Pugs::Grammar::Perl6.statements'}->(),
                } }
            ) ),


        '->' => Pugs::Compiler::Token->compile( q( 
        [
            <?ws>? <Pugs::Grammar::Perl6.signature_no_invocant> <?ws>? 
            \{ <?ws>? <Pugs::Grammar::Perl6.statements> <?ws>? \}
            { return { 
                pointy_block => $_[0]{'Pugs::Grammar::Perl6.statements'}->(),
                signature    => $_[0]{'Pugs::Grammar::Perl6.signature_no_invocant'}->(),
            } }
        |
            <?ws>?
            \{ <?ws>? <Pugs::Grammar::Perl6.statements> <?ws>? \}
            { return { 
                pointy_block => $_[0]{'Pugs::Grammar::Perl6.statements'}->(),
                signature    => undef,
            } }
        ]
            ) ),

        '.' => Pugs::Compiler::Token->compile( q(
                # .method op
                <?Pugs::Grammar::Term.ident>
                { return { dot_bareword  => $_[0]->() ,} }
            ) ),
        '...' => Pugs::Compiler::Token->compile( q(
            { 
                return { die => "not implemented" } 
            }
        ) ),
        q(') =>       # ' 
          Pugs::Compiler::Token->compile( q(
            <Pugs::Grammar::Term.single_quoted>
            { return { single_quoted => $/{'Pugs::Grammar::Term.single_quoted'}->() ,} }
        ) ),
        q(") => Pugs::Compiler::Token->compile( q(
            <Pugs::Grammar::Term.double_quoted>
            { return { double_quoted => $/{'Pugs::Grammar::Term.double_quoted'}->() ,} }
        ) ),
        q(s) => Pugs::Compiler::Token->compile( q(
            <Pugs::Grammar::Term.substitution>
            { return { 
                    substitution => $/{'Pugs::Grammar::Term.substitution'}->(),
                } 
            }
        ) ),
        q(rx) => Pugs::Compiler::Token->compile( q(
            <Pugs::Grammar::Term.rx>
            { return { 
                    rx => $/{'Pugs::Grammar::Term.rx'}->(),
                } 
            }
        ) ),
        q(m) => Pugs::Compiler::Token->compile( q(
            <Pugs::Grammar::Term.rx>
            { return { 
                    rx => $/{'Pugs::Grammar::Term.rx'}->(),
                } 
            }
        ) ),
        q(/) => Pugs::Compiler::Token->compile( q(
            <Pugs::Grammar::Term.rx_body('open','/')>
            { return { 
                    rx => $/{'Pugs::Grammar::Term.rx_body'}->(),
                } 
            }
        ) ),
        q(<) => Pugs::Compiler::Token->compile( q(
            <Pugs::Grammar::Term.angle_quoted>
            { return { 
                    angle_quoted => $/{'Pugs::Grammar::Term.angle_quoted'}->(),
                } 
            }
        ) ),
        # q(.) => ...
        q() => Pugs::Compiler::Regex->compile( q!
                ### floating point
                \d+\.\d+ { return { num => $() ,} } 
            |
                ### number
                \d+ { return { int => $() ,} } 
            |
                ### pair - long:<name> 
                \:
                [
                # :foo<bar>
                ([_|\w]+) \< (.*?) \>
                { return {
                    pair => { 
                        key   => { single_quoted => $/[0]() }, 
                        value => { single_quoted => $/[1]() }, 
                } } }
                |
                # :foo(exp)
                ([_|\w]+) \(  
                    <?ws>? <Pugs::Grammar::Perl6.perl6_expression> <?ws>? 
                \)
                { 
                    return {
                      pair => { 
                        key   => { single_quoted => $/[0]() }, 
                        value => $/{'Pugs::Grammar::Perl6.perl6_expression'}->(), 
                } } }
                |
                # :$foo 
                \$ ((_|\w)+)
                { return {
                    pair => { 
                        key   => { single_quoted => $/[0]() }, 
                        value => { scalar  => '$' . $/[0]() }, 
                } } }
                |
                # :foo 
                ((_|\w)+)
                { return {
                    pair => { 
                        key   => { single_quoted => $/[0]() }, 
                        value => { num => 1 }, 
                } } }
                ]            

            |
                <Pugs::Grammar::Perl6.sub_decl>
                    { return $_[0]{'Pugs::Grammar::Perl6.sub_decl'}->();
                    }
            |
                <Pugs::Grammar::Perl6.class_decl>
                    { return $_[0]{'Pugs::Grammar::Perl6.class_decl'}->();
                    }
            |
                ### perl5:Test::More
                perl5 \: <Pugs::Grammar::Term.bare_ident> 
                { return { 
                        bareword => $/{'Pugs::Grammar::Term.bare_ident'}->(),
                        lang => 'perl5',
                } }
            |
                ### Test-0.0.6
                <Pugs::Grammar::Term.cpan_bareword> 
                { return { cpan_bareword => $/{'Pugs::Grammar::Term.cpan_bareword'}->() } }
            |
                ### Test::More
                <Pugs::Grammar::Term.bare_ident> 
                { return { bareword => $/{'Pugs::Grammar::Term.bare_ident'}->() } }
        ! ),
    );
    $class->SUPER::recompile;
}

BEGIN {
    __PACKAGE__->recompile;
}

1;
