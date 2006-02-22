# pX/Common/iterator_engine_p6regex.pl - fglock
#
# experimental implementation of p6-regex parser
#
# see also: ../../Grammars/rx_grammar.pm

use strict;
use warnings;

require 'iterator_engine.pl';

{
  package grammar1;

sub any { 
    return unless $_[0];
    return ( undef, { '.'=> substr($_[0],0,1) }, substr($_[0],1) )
}

sub ws {
    return unless $_[0];
    return ( undef, { 'ws'=> $1 }, substr($_[0], 1) )
        if $_[0] =~ /^(\s)/;
    return;
};
sub escaped_char {
    return unless $_[0];
    return ( undef, { 'escaped_char'=> $1 }, substr($_[0], 2) )
        if $_[0] =~ /^(\\.)/;
    return;
};
sub word { 
    return unless $_[0];
    return ( undef, { 'word'=> $1 }, $2 )
        if $_[0] =~ /^([_[:alnum:]]+)(.*)/;
    return;
};

# rule compiler

sub closure {
        # p5 code is called using: "rule { xyz { v5; ... } }" (audreyt on #perl6)
        # or: "rule { ... [:perl5:: this is p5 ] ... }"
        # or: "[:perl5(1) this is perl5 ]" (putter on #perl6)

        my ( $code, $tail ) = $_[0] =~ /^\{(.*?)\}(.*)/;
        return unless defined $code;
        #print "parsing $code - $tail\n";
        my $result = eval $code;
        return ( undef, { code => $result }, $tail );
}

sub subrule {
        my ( $code, $tail ) = $_[0] =~ /^\<(.*?)\>(.*)/;
        return unless defined $code;
        #print "parsing subrule $code\n";
        return ( undef, { rule => $code }, $tail );
}

# TODO - simplify this by using a ruleop::label - see implementation of 'star'
*non_capturing_group =
do {
    my $r = ruleop::concat(
        ruleop::constant( '[' ),
        ruleop::concat(
            \&rule,
            ruleop::constant( ']' )
        )
    );
    sub { 
        my ( $state, $match, $tail ) = $r->( @_ ); 
        return unless $match;
        $match = $match->[1];  # remove '['
        pop @$match;   # remove ']'
        #print Dumper( $match );
        ( $state, $match, $tail );
    }        
};

# TODO - simplify this by using a ruleop::label - see implementation of 'star'
*capturing_group = 
do {
    my $r = ruleop::concat(
        ruleop::constant( '(' ),
        ruleop::concat(
            \&rule,
            ruleop::constant( ')' )
        )
    );
    sub { 
        my ( $state, $match, $tail ) = $r->( @_ ); 
        return unless $match;
        $match = $match->[1];  # remove '('
        pop @$match;   # remove ')'
        #print Dumper( $match );
        ( $state, { capture => $match }, $tail );
    }        
};

sub ruleop::capture { 
    my $node = shift;
    sub {
        print "# *** ruleop::capture not implemented\n";
        $node->( @_ );
    }
}

*dot = 
        ruleop::label( 'dot', 
            ruleop::constant( '.' ),
        );

# <ws>* [ <closure> | <subrule> | ... ]
*term = 
    ruleop::concat(
        ruleop::greedy_star( \&ws ),
        ruleop::alternation(
            \&closure,
            \&subrule,
            \&capturing_group,
            \&non_capturing_group,
            \&word,
            \&dot,
        ),
    );

# XXX - allow whitespace everywhere

# [ <term>\* | <term> 
# note: <term>\* creates a term named 'star'
*quantifier = 
    ruleop::alternation( 
        ruleop::label( 'star', 
            ruleop::concat(
                \&term,
                ruleop::constant( '*' ),
            ),
        ),
        \&term,
    );

# [ <term> [ \| <term> ]+ | <term> ]* 
# note: <term>|<term> creates a term named 'alt'
# XXX - 'alt' position is wrong
*rule = 
    ruleop::greedy_star (
        ruleop::alternation( 
            ruleop::label( 'alt', 
                ruleop::concat(
                    \&quantifier,
                    ruleop::greedy_plus(
                        ruleop::concat(
                            ruleop::constant( '|' ),
                            \&quantifier,
                        ),
                    ),
                ),
            ),                
            \&quantifier,
        ),
    );

}

#------ rule emitter

my $namespace = 'grammar1::';
sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1]; $tab .= '  ';
    local $Data::Dumper::Indent = 0;
    #print "emit_rule: ", ref($n)," ",Dumper( $n ), "\n";
    if ( ref( $n ) eq 'ARRAY' ) {
        my @s;
        for ( @$n ) {
            push @s, emit_rule( $_, $tab );
        }
        return $s[0] unless $s[1];
        return $s[1] unless $s[0];

        return $s[0].$s[1] unless $s[0];
        return $s[0].$s[1] unless $s[1];

        return "$tab ruleop::concat(\n" . 
               $s[0] . "$tab ,\n" . $s[1] . "$tab )\n";
    }
    elsif ( ref( $n ) eq 'HASH' ) 
    {
        my ( $k, $v ) = each %$n;
        #print "$tab $k => $v \n";
        if ( $k eq 'capture' ) {
            # return "$tab # XXX capture ?\n";
            return "$tab ruleop::capture(\n" .
                   emit_rule( $v, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'star' ) {
            pop @$v;  # remove '*'
            return "$tab ruleop::greedy_star(\n" .
                   emit_rule( $v, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'alt' ) {
            local $Data::Dumper::Indent = 1;
            my @alt = ( $v->[0] );
            # print "*** \$v:\n",Dumper $v;
            while(1) {
                $v = $v->[1];
                last unless defined $v->[0][1];
                push @alt, $v->[0][1];
            }
            #print "*** \@alt:\n",Dumper @alt;

            my @emit = map { 
                   emit_rule( $_, $tab ) .
                   "$tab ,\n" 
                 } @alt;

            return "$tab ruleop::alternation(\n" . 
                   join( '', @emit ) .
                   "$tab )\n";
        }        
        elsif ( $k eq 'code' ) {
            # return "$tab # XXX code - compile '$v' ?\n";
            return "$tab $v  # XXX - code\n";  
        }        
        elsif ( $k eq 'ws' ) {
            return;
        }
        elsif ( $k eq 'dot' ) {
            return "$tab \\&{'${namespace}any'}\n";
        }
        elsif ( $k eq 'rule' ) {
            return "$tab \\&{'$namespace$v'}\n";
        }
        elsif ( $k eq 'constant' ) {
            return "$tab ruleop::constant( '$v' )\n";
        }
        elsif ( $k eq 'word' ) {
            return "$tab ruleop::constant( '$v' )\n";
        }
        else {
            die "unknown node: ", Dumper( $n );
        }
    }
    else 
    {
        die "unknown node: ", Dumper( $n );
    }
}

1;
