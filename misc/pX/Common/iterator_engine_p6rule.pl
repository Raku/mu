# pX/Common/iterator_engine_p6rule.pl - fglock
#
# experimental implementation of p6-regex parser
#
# see: iterator_engine_README

# TODO $var := (capture)

use strict;
use warnings;

use Data::Dumper;

require 'iterator_engine.pl';
require 'iterator_engine_p6rule_lib.pl';

my $namespace = 'grammar1::';

{
  package grammar1;

  use Data::Dumper;
  no warnings 'once';

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

# XXX - compile non_capturing_subrule using a rule
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

# XXX - compile negated_subrule using a rule
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

*capturing_group = 
    ruleop::concat(
        ruleop::constant( '(' ),
        ruleop::capture( 'capturing_group',
            \&rule,
        ),
        ruleop::constant( ')' )
    );

*dot = 
    ruleop::capture( 'dot', 
        ruleop::constant( '.' ),
    );

# <'literal'> literal \*
my @literals = (
    ruleop::concat(    
        ruleop::constant( "<\'" ),
        ruleop::capture( 'literal',
            ruleop::non_greedy_star( \&any ),
        ),
        ruleop::constant( "\'>" ),
    ),
    ruleop::capture( 'literal', \&word ),
    ruleop::capture( 'literal', \&escaped_char )
);

use vars qw( @rule_terms );
@rule_terms = (
    \&capturing_group,
    @literals,
    \&negated_subrule,
    \&non_capturing_subrule,
    \&subrule,
    \&dot,
    # more items are pushed later - see below 
);

# <ws>* [ <closure> | <subrule> | ... ]
*term = 
    ruleop::concat(
        \&ws_star,
        ruleop::alternation( \@rule_terms ),
        \&ws_star,
    );

# XXX - allow whitespace everywhere

# [ <term>[\*|\+] | <term> 
# note: <term>\* creates a term named 'star'
*quantifier = 
    ruleop::alternation( [
        ruleop::capture( 'star', 
            ruleop::concat(
                ruleop::capture( 'term', \&term ),
                ruleop::capture( 'literal',
                    ruleop::alternation( [
                        ruleop::constant( '??' ),
                        ruleop::constant( '?' ),
                        ruleop::constant( '*?' ),
                        ruleop::constant( '+?' ),
                        ruleop::constant( '*' ),
                        ruleop::constant( '+' ),
                    ] ),
                ),
                \&ws_star,
            ),
        ),
        \&term,
    ] );

# [ <term> [ \| <term> ]+ | <term> ]* 
# note: <term>|<term> creates a term named 'alt'
# XXX - 'alt' position is wrong
*rule = 
    ruleop::greedy_star (
        ruleop::alternation( 
          [
            ruleop::capture( 'alt', 
                ruleop::concat(
                    ruleop::capture( 'term', \&quantifier ),
                    ruleop::greedy_plus(
                        ruleop::concat(
                            ruleop::constant( '|' ),
                            ruleop::capture( 'term', \&quantifier ),
                        ),
                    ),
                ),
            ),                
            \&quantifier,
          ]
        ),
    );

# [<rule>]
*non_capturing_group = ::compile_rule( ' \[ <rule> \] ', {print_program=>0} );
push @rule_terms, \&non_capturing_group;

# $var
*ident =    ::compile_rule( '[[\:\:]?<word>]+', {print_ast=>0} );
*variable = ::compile_rule( '[\$|\%|\@]<ident>' );
push @rule_terms, ruleop::capture( 'variable',\&variable );

# <@var> is a run-time alternation (TimToady on #perl6)
*runtime_alternation = ::compile_rule( 
    '\< <variable> \>' );
unshift @rule_terms, ruleop::capture( 
    'runtime_alternation',\&runtime_alternation );

# $xxx := (capture)
*named_capture = ::compile_rule( 
    '<variable> <ws>? \:\= <ws>? \( <rule> \)' );
unshift @rule_terms, ruleop::capture( 
    'named_capture',\&named_capture );

# { code }
    # p5 code is called using: "rule { xyz { v5; ... } }" (audreyt on #perl6)
    #   (but TimToady said it's not)
    # or: "rule { ... [:perl5:: this is p5 ] ... }"
    # or: "[:perl5(1) this is perl5 ]" (putter on #perl6)
    
# XXX - this is veeery slow - the actual implementation is in file
#       p6rule_lib.pl and uses Text::Balanced
# *code = ::compile_rule( '\{[<code>|.]*?\}' );
unshift @rule_terms, ruleop::capture( 
    'closure', \&code );
    
}

#------ match functions

sub match::get {
    my $match =   $_[0];
    my $name =    $_[1];
    
    return $match->{capture}  if $name eq '$/<>' || $name eq '$<>';
    return $match->{match}    if $name eq '$/';
    
    # $/<0>, $/<1>
    # $<0>, $<1>
    if ( $name =~ /^ \$ \/? <(\d+)> $/x ) {
        my $num = $1;
        my $n = 0;
        for ( @{ match::get( $match, '$/<>' ) } ) {
            next unless ref($_) eq 'HASH';
            if ( $num == $n ) {
                my (undef, $capture) = each %$_;
                # print "cap\n", Dumper( $capture );
                return $capture;
            }
            $n++;
        }
        return;
    }
    
    # $/<name>
    # $<name>
    if ( $name =~ /^ \$ \/? <(.+)> $/x ) {
        my $n = $1;
        for ( @{ match::get( $match, '$/<>' ) } ) {
            next unless ref($_) eq 'HASH';
            if ( exists $_->{$n} ) {
                # print "cap\n", Dumper( $capture );
                return $_->{$n};
            }
        }
        return;
    }

    die "match variable $name is not implemented";
    # die "no submatch like $name in " . Dumper( $match->{match} );
}

sub match::str {
    my $match = $_[0];
    #print "STR: ", ref( $match ), " ", Dumper( $match ), "\n";
    return join( '', map { match::str( $_ ) } @$match )
        if ref( $match ) eq 'ARRAY';
    return join( '', map { match::str( $_ ) } values %$match )
        if ref( $match ) eq 'HASH';
    return $match;
}

#------ rule emitter

# compile_rule( $source, {flag=>value} );
#
# flags:
#   print_program=>1 - prints the generated program
#
sub compile_rule {
    local $Data::Dumper::Indent = 1;
    my $match = grammar1::rule->( $_[0] );
    my $flags = $_[1];
    print "ast:\n", Dumper( $match->{capture} ) if $flags->{print_ast};
    die "syntax error in rule '$_[0]' at '" . $match->{tail} . "'\n"
        if $match->{tail};
    die "syntax error in rule '$_[0]'\n"
        unless $match->{bool};
    my $program = main::emit_rule( $match->{capture} );
    print "generated rule:\n$program" if $flags->{print_program};
    my $code = eval($program); die $@ if $@;
    return $code;
}

sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1] || '    '; 
    $tab .= '  ';
    local $Data::Dumper::Indent = 0;
    #print "emit_rule: ", ref($n)," ",Dumper( $n ), "\n";

    # XXX - not all nodes are actually used

    if ( ref($n) eq '' ) {
        # XXX - this should not happen, but it does
        return '';
    }
    if ( ref( $n ) eq 'ARRAY' ) {
        my @s;
        for ( @$n ) {
            #print "emitting array item\n";
            my $tmp = emit_rule( $_, $tab );
            push @s, $tmp . "$tab ,\n" if $tmp;
        }
        
        # XXX XXX XXX - temporary hacks to translate p6 to p5 -- see 'closure' node
        if ($s[-1] =~ /^#return-block#(.*)/s ) {
            #print "return block\n";
            my $code = $1;
            #print "Code: $code\n";
            pop @s;
            my $program;
            if ( @s == 1 ) {
                $program = $s[0];
            }
            else {
                $program = "$tab ruleop::concat(\n" . 
                            ( join '', @s ) . 
                            "$tab )\n";
            }
            #print "program $program\n";
            my $return;
            $return = "
    sub { 
        my \$rule = \n$program    ;
        my \$match = \$rule->( \@_ );
        return unless \$match;
        my \$c = sub " . $code . "; 
        #use Data::Dumper;
        #print \"capture was: \", Dumper( \$match->{capture} );
        return { 
            \%\$match,
            capture => [ \$c->( \$match ) ],
        }; 
    }\n";
            return $return;
        }
        return $s[0] if @s == 1;
        return "$tab ruleop::concat(\n" . 
               ( join '', @s ) . 
               "$tab )\n";
    }
    elsif ( ref( $n ) eq 'HASH' ) 
    {
        my ( $k, $v ) = each %$n;
        #print "$tab $k => $v \n";
        if ( $k eq 'rule' ) {
            return emit_rule( $v, $tab );
            
            #return "$tab ruleop::capture( '$k',\n" .
            #       emit_rule( $v, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'capturing_group' ) {
            return "$tab ruleop::capture( '$k',\n" .
                   emit_rule( $v, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'non_capturing_group' ) {
            return emit_rule( $v, $tab );
        }        
        elsif ( $k eq 'star' ) {
            local $Data::Dumper::Indent = 1;
            my $term = $v->[0]{'term'};
            #print "*** \$term:\n",Dumper $term;
            my $quantifier = $v->[1]{'literal'}[0];
            my $sub = { 
                    '*' =>'greedy_star',     
                    '+' =>'greedy_plus',
                    '*?'=>'non_greedy_star', 
                    '+?'=>'non_greedy_plus',
                    '?' =>'optional',
                    '??' =>'null_or_optional',
                }->{$quantifier};
            # print "*** \$quantifier:\n",Dumper $quantifier;
            die "quantifier not implemented: $quantifier" 
                unless $sub;
            return "$tab ruleop::$sub(\n" .
                   emit_rule( $term, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'alt' ) {
            # local $Data::Dumper::Indent = 1;
            # print "*** \$v:\n",Dumper $v;
            my @s;
            for ( @$v ) { 
                my $tmp = emit_rule( $_, $tab );
                push @s, $tmp if $tmp;   
            }
            return "$tab ruleop::alternation( [\n" . 
                   join( '', @s ) .
                   "$tab ] )\n";
        }        
        elsif ( $k eq 'term' ) {
            return emit_rule( $v, $tab );
        }        
        elsif ( $k eq 'code' ) {
            # return "$tab # XXX code - compile '$v' ?\n";
            return "$tab $v  # XXX - code\n";  
        }        
        elsif ( $k eq 'dot' ) {
            return "$tab \\&{'${namespace}any'}\n";
        }
        elsif ( $k eq 'subrule' ) {
            #print Dumper $v;
            return "$tab ruleop::capture( '$v', \\&{'$namespace$v'} )\n";
        }
        elsif ( $k eq 'non_capturing_subrule' ) {
            #print Dumper $v;
            return "$tab \\&{'$namespace$v'}\n";
        }
        elsif ( $k eq 'negated_subrule' ) {
            #print Dumper $v;
            return "$tab ruleop::negate( \\&{'$namespace$v'} )\n";
        }
        elsif ( $k eq 'literal' ) {
            #print "literal:", Dumper($v);
            my $name = quotemeta(join('',@$v));
            #print "literal: $name\n";
            return "$tab ruleop::constant( \"$name\" )\n";
        }
        elsif ( $k eq 'variable' ) {
            #print "variable:", Dumper($v);
            my $name = match::str( $v );
            # print "var name: ", match::str( $v ), "\n";
            my $value = "sub { die 'not implemented: $name' }\n";
            $value = eval $name if $name =~ /^\$/;
            $value = join('', eval $name) if $name =~ /^\@/;

            # XXX - what hash/code interpolate to?
            # $value = join('', eval $name) if $name =~ /^\%/;

            return "$tab ruleop::constant( '" . $value . "' )\n";
        }
        elsif ( $k eq 'closure' ) {
            #print "closure: ", Dumper( $v );
            my $code = match::str( $v ); 
            
            # XXX XXX XXX - temporary hacks to translate p6 to p5
            $code =~ s/ \$<(.*?)> / match::get( \$_[0], '\$<$1>' ) /sgx;
            #print "Code: $code\n";
            
            return "$tab sub {\n" . 
                   "$tab     $code;\n" . 
                   "$tab     return { bool => 1, tail => \$_[0] } }\n"
                unless $code =~ /return/;
            return "#return-block#" . $code;
        }
        elsif ( $k eq 'runtime_alternation' ) {
            my $code = match::str( match::get( 
                { capture => $v }, 
                '$<variable>'
            ) );
            return "$tab ruleop::alternation( \\$code )\n";
        }
        elsif ( $k eq 'named_capture' ) {
            my $name = match::str( match::get( 
                { capture => $v }, 
                '$<variable>'
            ) );
            my $program = emit_rule(
                    match::get( 
                        { capture => $v }, 
                        '$<rule>'
                    ), $tab );
            return "$tab ruleop::capture( '$name', \n" . $program . "$tab )\n";
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


__END__

random notes...

<fglock> audreyt: what is the relationship between AST and Match? (I'm compiling 
the match captures)
<audreyt> fglock: no relationship whatsoever :)
<audreyt> fglock: the Match object may carry an captured object in $/<>
<audreyt> aka $()
<audreyt> and if you are writing a Perl 6 parser, then that capture object may be 
an AST object
<audreyt> you can set the capture object by
<audreyt> rule { $() := ... }
<audreyt> or
<audreyt> rule { ... { return $capture_object } }
<audreyt> or
<audreyt> rule { ... { $() := ... } }
<audreyt> if the capture object is not set explicitly
<audreyt> then it's set to the entire match as matched by the rule.
<spinclad> so in q:code:{ say $x; {{{$a}}} } the $x is literal but the $a is 
unquoted (interpolated)? therefore the {{{ }}}'s?
<audreyt> +$/ and ~$/ resolves to +$() and ~$() respectively.
