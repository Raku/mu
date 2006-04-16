package Pugs::Grammar::Term;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Runtime::Match;

use Pugs::Grammar::Var;

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
        capture => { pair => { key => { single_quoted => $1 }, value => { single_quoted => $2 } } },
    } )
        if $_[0] =~ /^([_\w]+):<(.*?)>(.*)$/s;
    return $class->no_match;
};

#~ sub sub_call {
    #~ my $class = shift;
    #~ return $class->no_match unless $_[0];
    #~ return Pugs::Runtime::Match->new( { 
        #~ bool  => 1,
        #~ match => $1,
        #~ tail  => $2,
        #~ capture => { op => 'CALL', name => { single_quoted => $1 }, },
    #~ } )
        #~ if $_[0] =~ /^ ([_\w]+) \.? \( (.*)$/sx;
    #~ return $class->no_match;
#~ };

sub bareword {
    my $class = shift;
    return $class->no_match unless $_[0];
    return Pugs::Runtime::Match->new( { 
        bool  => 1,
        match => $1,
        tail  => $2,
        capture => { bareword => $1 },
    } )
        if $_[0] =~ /^ ([_\w\d-]+) ( (?: \(|\;|\s|$ ) .*)$/sx;
    return $class->no_match;
};

#~ sub sub_call_no_paren {
    #~ my $class = shift;
    #~ return $class->no_match unless $_[0];
    #~ return Pugs::Runtime::Match->new( { 
        #~ bool  => 1,
        #~ match => $1,
        #~ tail  => $2,
        #~ capture => { op => 'CALL_NO_PAREN', name => { single_quoted => $1 }, },
    #~ } )
        #~ if $_[0] =~ /^ ([_\w]+) (?:\s|$|;) (.*)$/sx;
    #~ return $class->no_match;
#~ };

sub single_quoted {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( "'" . $_[0], "'" );
    $extracted = substr( $extracted, 1, -1 ) if length($extracted) > 1;
    return Pugs::Runtime::Match->new( { 
        bool  => ( $extracted ne '' ),
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
        bool  => ( $extracted ne '' ),
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
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        capture => $extracted,
    } );
}

sub recompile {
    my $class = shift;
    %hash = (
        %Pugs::Grammar::Var::hash,
        %Pugs::Grammar::StatementControl::hash,
        '...' => Pugs::Compiler::Rule->compile( q(
            { 
                return { die => "not implemented" } 
            }
        ) ),
        Inf => Pugs::Compiler::Rule->compile( q(
            { return { num => 'Inf' ,} } 
        ) ),
        NaN => Pugs::Compiler::Rule->compile( q(
            { return { num => 'NaN' ,} } 
        ) ),
        q(') => Pugs::Compiler::Rule->compile( q(
            <Pugs::Grammar::Term.single_quoted>
            { return { single_quoted => $/{'Pugs::Grammar::Term.single_quoted'}->() ,} }
        ) ),
        q(") => Pugs::Compiler::Rule->compile( q(
            <Pugs::Grammar::Term.double_quoted>
            { return { double_quoted => $/{'Pugs::Grammar::Term.double_quoted'}->() ,} }
        ) ),
        q(<) => Pugs::Compiler::Rule->compile( q(
            <Pugs::Grammar::Term.angle_quoted>
            { return { angle_quoted => $/{'Pugs::Grammar::Term.angle_quoted'}->() ,} }
        ) ),
        q(.) => Pugs::Compiler::Rule->compile( q(
            <Pugs::Grammar::Term.bareword>
            { return { method => $/{'Pugs::Grammar::Term.bareword'}->() ,} }
        ) ),
        q() => Pugs::Compiler::Rule->compile( q!
                ### number
                \d+ { return { num => $() ,} } 
            |
                ### long:<name> 
                <Pugs::Grammar::Term.pair>
                { return $/{'Pugs::Grammar::Term.pair'}->() }
            #~ |
                #~ ### func(... func.(...
                #~ <Pugs::Grammar::Term.sub_call> 
                #~ { return $/{'Pugs::Grammar::Term.sub_call'}->() }
            |
                ### v6
                <Pugs::Grammar::Term.bareword> 
                { return $/{'Pugs::Grammar::Term.bareword'}->() }
        ! ),
    );
    $class->SUPER::recompile;
}

BEGIN {
    __PACKAGE__->recompile;
}

1;
