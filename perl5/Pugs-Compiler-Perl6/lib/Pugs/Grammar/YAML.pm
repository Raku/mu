package Pugs::Grammar::YAML;

use strict;
use warnings;

use Pugs::Runtime::Match;

use base 'Pugs::Grammar::Base';
use Carp;
use YAML::Syck;

sub parse {
    my $pos = $_[2]{p} || 0;
    my ( $ast, $to );
    $YAML::Syck::ImplicitTyping = 1;
    no warnings 'once';
    eval { $ast = YAML::Syck::Load( $_[1] ) };
        if ( $@ ) {
            carp "Error parsing YAML: $@";
            return;
        }
    $to = length( $_[1] ) + 1;  # XXX
    my $match = Pugs::Runtime::Match->new( { 
        bool    => \( $ast ? 1 : 0 ),
        str     => \$_[1],
        match   => [],
        from    => \$pos,
        to      => \$to,
        capture => \$ast,
    } );
    #print "YAML Expression: ",Dumper( $match->() );
    return $match;
};

1;
