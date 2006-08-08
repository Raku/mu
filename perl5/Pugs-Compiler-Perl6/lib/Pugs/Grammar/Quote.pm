package Pugs::Grammar::Quote;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Runtime::Match;
use Pugs::Compiler::Token;
use Text::Balanced;

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

BEGIN {

    __PACKAGE__->add_rule(
        q(') =>  q( 
            <Pugs::Grammar::Quote.single_quoted>
            { return { single_quoted => $/{'Pugs::Grammar::Quote.single_quoted'}->() ,} }
        ) );
    __PACKAGE__->add_rule(
        q(") =>  q( 
            <Pugs::Grammar::Quote.double_quoted>
            { return { double_quoted => $/{'Pugs::Grammar::Quote.double_quoted'}->() ,} }
        ) );
    __PACKAGE__->add_rule(
        q(<) => q(
            <Pugs::Grammar::Quote.angle_quoted>
            { return { 
                    angle_quoted => $/{'Pugs::Grammar::Quote.angle_quoted'}->(),
                } 
            }
        ) );

    __PACKAGE__->recompile;
}


1;
