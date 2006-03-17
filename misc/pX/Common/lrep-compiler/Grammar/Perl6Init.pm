# This package pre-fill the Grammar::Perl6 package
package Grammar::Perl6;

require Runtime::Perl5::RuleOps;

use Text::Balanced;
use Data::Dumper;
use warnings;
no warnings qw(once);

use vars qw( @rule_terms );

# ----- the following were included only for performance reasons,
# because they are too frequent and they are too slow using the basic
# rule parser
# UPDATE - move these to prelude using rx:perl

sub subrule {
    my ( $code, $tail ) = $_[0] =~ /^\<(.*?)\>(.*)$/s;
    return unless defined $code;
    #print "parsing subrule $code\n";
    return { 
        bool  => 1,
        match => { code => $code },
        tail  => $tail,
        capture => [ { subrule => $code } ],
    }
}

# XXX - set non-capture flag
sub non_capturing_subrule {
    my ( $code, $tail ) = $_[0] =~ /^\<\?(.*?)\>(.*)$/s;
    return unless defined $code;
    # print "non_capturing_subrule $code - $1\n";
    return { 
        bool  => 1,
        match => { code => $code },
        tail  => $tail,
        capture => [ { non_capturing_subrule => $code } ],
    }
}

sub negated_subrule {
    my ( $code, $tail ) = $_[0] =~ /^\<\!(.*?)\>(.*)$/s;
    return unless defined $code;
    # print "negated_subrule $code - $1\n";
    return { 
        bool  => 1,
        match => { code => $code },
        tail  => $tail,
        capture => [ { negated_subrule => $code } ],
    }
}

@rule_terms = (
    # <'literal'> literal \*
    Runtime::Perl5::RuleOps::concat(    
        Runtime::Perl5::RuleOps::constant( "<\'" ),
        Runtime::Perl5::RuleOps::capture( 'constant',
            Runtime::Perl5::RuleOps::non_greedy_star( \&any ),
        ),
        Runtime::Perl5::RuleOps::constant( "\'>" ),
    ),

    \&negated_subrule,
    \&non_capturing_subrule,
    \&subrule,
);

# XXX - allow whitespace everywhere
# [ <term>[\*|\+] | <term> 
# note: <term>\* creates a term named 'star'
*quantifier = 
    Runtime::Perl5::RuleOps::alternation( [
        Runtime::Perl5::RuleOps::capture( 'star', 
            Runtime::Perl5::RuleOps::concat(
                Runtime::Perl5::RuleOps::capture( 'term', \&term ),
                Runtime::Perl5::RuleOps::capture( 'literal',
                    Runtime::Perl5::RuleOps::alternation( [
                        Runtime::Perl5::RuleOps::constant( '??' ),
                        Runtime::Perl5::RuleOps::constant( '?' ),
                        Runtime::Perl5::RuleOps::constant( '*?' ),
                        Runtime::Perl5::RuleOps::constant( '+?' ),
                        Runtime::Perl5::RuleOps::constant( '*' ),
                        Runtime::Perl5::RuleOps::constant( '+' ),
                    ] ),
                ),
                \&ws_star,
            ),
        ),
        \&term,
    ] );

*alt = 
    Runtime::Perl5::RuleOps::capture( 'alt', 
        Runtime::Perl5::RuleOps::concat(
            Runtime::Perl5::RuleOps::capture( 'term', \&quantifier ),
            Runtime::Perl5::RuleOps::greedy_plus(
                Runtime::Perl5::RuleOps::concat(
                    Runtime::Perl5::RuleOps::constant( '|' ),
                    Runtime::Perl5::RuleOps::capture( 'term', \&quantifier ),
                ),
            ),
        ),
    ),
;



sub any { 
    return unless $_[0];
    return { 
        bool  => 1,
        match => { '.'=> substr($_[0],0,1) },
        tail  => substr($_[0],1),
        ( $_[2]->{capture} ? ( capture => [ substr($_[0],0,1) ] ) : () ),
    };
}
sub ws {
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^(\s+)(.*)$/s;
    return;
};
sub p6ws {
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'p6ws'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^((?:\s|\#(?-s:.)*)+)(.*)$/s;
    return;
};
sub newline {
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'newline'=> $1 },
        tail  => substr($_[0],1),
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^(\n)/s;
    return;
};
sub escaped_char {
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'escaped_char'=> $1 },
        tail  => substr($_[0],2),
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^\\(.)/s;
    return;
};

# ----- the following were included only for performance reasons,
# because they are too frequent and they are too slow using the basic 
# rule parser

sub code {
    return unless $_[0];
    ($extracted,$remainder) = Text::Balanced::extract_codeblock( $_[0] );
    return { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        ( $_[2]->{capture} ? ( capture => [ $extracted ] ) : () ),
    };
}

sub literal {
    return unless $_[0];
    ($extracted,$remainder) = Text::Balanced::extract_delimited( $_[0], "'" );
    $extracted = substr( $extracted, 1, -1 ) if $extracted;
    return { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        ( $_[2]->{capture} ? ( capture => [ { literal => $extracted } ] ) : () ),
    };
}

sub ws_star {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws*'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^(\s*)(.*)$/s;
    return;
};
sub p6ws_star {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'p6ws*'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^((?:\s|\#(?-s:.)*)*)(.*)$/s;
    return;
};

sub varscalar {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws*'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => $1 ) : () ),
    }
        if $_[0] =~ / ^  
            (   \$
                (?: 
                    (?:\:\:)? 
                    [_[:alnum:]]+ 
                )+
            )  
            (.*) $ /xs;
    return;
};

sub varhash {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws*'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => $1 ) : () ),
    }
        if $_[0] =~ / ^  
            (   \%
                (?: 
                    (?:\:\:)? 
                    [_[:alnum:]]+ 
                )+
            )  
            (.*) $ /xs;
    return;
};


sub variable {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws*'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => $1 ) : () ),
    }
        if $_[0] =~ / ^  
            (   [ $ % @ % ]
                (?: 
                    (?:\:\:)? 
                    [_[:alnum:]]+ 
                )+
            )  
            (.*) $ /xs;
    return;
};

sub ident {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ident'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ { ident => $1 } ] ) : () ),
    }
        if $_[0] =~ / ^  
            ( 
                (?: 
                    (?:\:\:)? 
                    [_[:alnum:]]+ 
                )+
            )  
            (.*) $ /xs;
    return;
};

1;
