# pX/Common/iterator_engine_p6regex.pl - fglock
#
# experimental implementation of p6-regex parser
#
# see also: ../../Grammars/rx_grammar.pm

use strict;
use warnings;

# implemented:
# . ? * + *? +? 
# \char <ws> <word> literal
# [] 
# {} (with perl5 code)
# () (but doesn't capture yet)
# <subrule>
# |

# implemented but untested:
# <'literal'>
# <other::rule>

# not implemented:
# $var $1
# <"literal">
# ^ ^^ $ $$
# <!term>
# <unicode-class> <+unicode-class> <+unicode-class+unicode-class>
# <?var> <@var> <&var> <%var>
# {n..m} 
# : :: :::   (commit)
# :=         (alias)
# <(closure-assertion)> <{code-returns-rule}>
# <'literal'>
# <<character-class>> <[character-class]>
# :flag :flag() :flag[]
# lookahead lookbehind
# #comment\n
# \x0a \0123 ...
# <?ws>  -- optional whitespace ???

# not sure if specified:
# &
# 'literal' "literal"
# <!n,m>  -- <!{n..m}> ???

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
        if $_[0] =~ /^(\s)/s;
    return;
};
sub escaped_char {
    return unless $_[0];
    return ( undef, { 'escaped_char'=> $1 }, substr($_[0], 2) )
        if $_[0] =~ /^(\\.)/s;
    return;
};
sub word { 
    return unless $_[0];
    return ( undef, { 'word'=> $1 }, $2 )
        if $_[0] =~ /^([_[:alnum:]]+)(.*)/s;
    return;
};

sub closure {
        # p5 code is called using: "rule { xyz { v5; ... } }" (audreyt on #perl6)
        # or: "rule { ... [:perl5:: this is p5 ] ... }"
        # or: "[:perl5(1) this is perl5 ]" (putter on #perl6)

        my ( $code, $tail ) = $_[0] =~ /^\{(.*?)\}(.*)/s;
        return unless defined $code;
        #print "parsing $code - $tail\n";
        my $result = eval $code;
        return ( undef, { code => $result }, $tail );
}

sub subrule {
        my ( $code, $tail ) = $_[0] =~ /^\<(.*?)\>(.*)/s;
        return unless defined $code;
        #print "parsing subrule $code\n";
        return ( undef, { rule => $code }, $tail );
}

*non_capturing_group =
    ruleop::label( 'non_capturing_group',
      ruleop::concat(
        ruleop::constant( '[' ),
        \&rule,
        ruleop::constant( ']' )
      ),
    );

*capturing_group = 
    ruleop::label( 'capturing_group',
      ruleop::concat(
        ruleop::constant( '(' ),
        \&rule,
        ruleop::constant( ')' )
      ),
    );

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

# <'literal'>
*literal = 
    ruleop::label( 'literal',
        ruleop::concat(    
            ruleop::constant( "<\'" ),
            ruleop::non_greedy_star( \&any ),
            ruleop::constant( "\'>" ),
        ),
    );

use vars qw( @rule_terms );
@rule_terms = (
            \&closure,
            \&subrule,
            \&capturing_group,
            \&non_capturing_group,
            \&word,
            \&escaped_char,
            \&literal,
            \&dot,
);

# <ws>* [ <closure> | <subrule> | ... ]
*term = 
    ruleop::concat(
        ruleop::greedy_star( \&ws ),
        ruleop::alternation( \@rule_terms ),
    );

# XXX - allow whitespace everywhere

# [ <term>[\*|\+] | <term> 
# note: <term>\* creates a term named 'star'
*quantifier = 
    ruleop::alternation( 
      [
        ruleop::label( 'star', 
            ruleop::concat(
                \&term,
                ruleop::alternation( [
                    ruleop::constant( '?' ),
                    ruleop::constant( '*?' ),
                    ruleop::constant( '+?' ),
                    ruleop::constant( '*' ),
                    ruleop::constant( '+' ),
                ] ),
            ),
        ),
        \&term,
      ]
    );

# [ <term> [ \| <term> ]+ | <term> ]* 
# note: <term>|<term> creates a term named 'alt'
# XXX - 'alt' position is wrong
*rule = 
    ruleop::greedy_star (
        ruleop::alternation( 
          [
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
          ]
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
        if ( $k eq 'capturing_group' ) {
            $v = $v->[1][0];  # remove '( )'
            return "$tab ruleop::capture(\n" .
                   emit_rule( $v, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'non_capturing_group' ) {
            local $Data::Dumper::Indent = 1;
            # print "*** \$v:\n",Dumper $v;
            $v = $v->[1][0];  # remove '[ ]'
            # print "*** \$v:\n",Dumper $v;
            return emit_rule( $v, $tab );
        }        
        elsif ( $k eq 'star' ) {
            my $quantifier = pop @$v;  # '*' or '+'
            $quantifier = $quantifier->{'constant'};
            my $sub = { 
                    '*' =>'greedy_star',     
                    '+' =>'greedy_plus',
                    '*?'=>'non_greedy_star', 
                    '+?'=>'non_greedy_plus',
                    '?' =>'optional',
                }->{$quantifier};
            # print "*** \$quantifier:\n",Dumper $quantifier;
            die "quantifier not implemented: $quantifier" 
                unless $sub;
            return "$tab ruleop::$sub(\n" .
                   emit_rule( $v, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'alt' ) {
            # local $Data::Dumper::Indent = 1;
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

            return "$tab ruleop::alternation( [\n" . 
                   join( '', @emit ) .
                   "$tab ] )\n";
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
        elsif ( $k eq 'escaped_char' ) {
            return "$tab ruleop::constant( '". substr( $v, 1 ) ."' )\n";
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
