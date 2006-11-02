package Pugs::Grammar::Quote;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Runtime::Match;
use Pugs::Compiler::Token;
use Pugs::Grammar::Term;
use Text::Balanced;

sub angle_quoted {
    my $grammar = shift;
    return $grammar->no_match(@_) unless $_[0];
    my $pos = $_[1]{p} || 0;
    my $s = substr( $_[0], $pos );
    my ($extracted,$remainder) = Text::Balanced::extract_bracketed( '<' . $s, '<..>' );
    return $grammar->no_match(@_) unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    my @items = split( /\s+/, $extracted );
    my $ast;
    if ( @items > 1 ) {
      $ast = {
        'assoc' => 'list',
        'list' => [
            map {
              { 'single_quoted' => $_ }
            } @items
        ],
        'op1' => ','
      };
    }
    else {
      $ast = { 'single_quoted' => $items[0] }
    }
    return Pugs::Runtime::Match->new( { 
        bool    => \1,
        str     => \$_[0],
        match   => [],
        from    => \$pos,
        to      => \( length($_[0]) - length($remainder) ),
        capture => \$ast,
    } );
}

*double_quoted_expression = Pugs::Compiler::Token->compile(q(
        <before [ <'$'> | <'@'> | <'%'> | <'&'> ] >
        <Pugs::Grammar::Term.parse>
        { return $/{'Pugs::Grammar::Term.parse'}() }
))->code;

*double_quoted_text = Pugs::Compiler::Token->compile(q(
    (
        <!before [ <'$'> | <'@'> | <'%'> | <'&'> | <'"'> ] >
        [ <'\"'> | . ]
    )+
    { return { double_quoted => $/() ,} }
))->code;

*double_quoted = Pugs::Compiler::Token->compile(q(
     
    [  $<q1> := <double_quoted_expression>
    |  $<q1> := <double_quoted_text>
    ]
    
    [   $<q2> := <double_quoted> 
        { return { 
            exp1 => $/{q1}(), 
            exp2 => $/{q2}(),
            'fixity' => 'infix',
            'op1' => '~',
        } } 
    |   { return $/{q1}() } 
    ]
))->code;

*double_angle_quoted_text = Pugs::Compiler::Token->compile(q(
    (
        <!before [ <'$'> | <'@'> | <'%'> | <'&'> | <'»'> | \>\> ] >
        [ <'\»'> | . ]
    )+
    { return { double_quoted => $/() ,} }
))->code;

*double_angle_quoted = Pugs::Compiler::Token->compile(q(
     
    [  $<q1> := <double_quoted_expression>
    |  $<q1> := <double_angle_quoted_text>
    ]
    
    [   $<q2> := <double_angle_quoted> 
        { return { 
            exp1 => $/{q1}(), 
            exp2 => $/{q2}(),
            'fixity' => 'infix',
            'op1' => '~',
        } } 
    |   { return $/{q1}() } 
    ]
))->code;

BEGIN {

    __PACKAGE__->add_rule(
        q(') =>  q(
            <Pugs::Grammar::Rule.literal>
            \'
            { return { single_quoted => $/{'Pugs::Grammar::Rule.literal'}->() ,} }
        ) );
    __PACKAGE__->add_rule(
        q(") =>  q( 
            <Pugs::Grammar::Quote.double_quoted> <'"'>
            { return $/{'Pugs::Grammar::Quote.double_quoted'}->() }
          | <'"'>
            { return { double_quoted => '' } }
        ) );
    __PACKAGE__->add_rule(
        q(<) => q(
            <Pugs::Grammar::Quote.angle_quoted>
            { return $/{'Pugs::Grammar::Quote.angle_quoted'}->() }
        ) );
    __PACKAGE__->add_rule(
        q(«) => q(
            <Pugs::Grammar::Quote.double_angle_quoted> <'»'>
            { return { 
                    double_angle_quoted => $/{'Pugs::Grammar::Quote.double_angle_quoted'}->(),
                } 
            }
        ) );
    __PACKAGE__->add_rule(
        q(<<) => q(
            <Pugs::Grammar::Quote.double_angle_quoted> \>\>
            { return { 
                    double_angle_quoted => $/{'Pugs::Grammar::Quote.double_angle_quoted'}->(),
                } 
            }
        ) );

    __PACKAGE__->recompile;
}


1;
