package Pugs::Grammar::Expression;

use utf8;
use strict;
use warnings;

#use Pugs::Compiler::Rule;
use Pugs::Runtime::Match;
use Pugs::Grammar::Precedence;

#use Pugs::Grammar::P6Term;   # pure Perl6 version
use Pugs::Grammar::Term;    # Perl5+Perl6 version

use Pugs::Grammar::Quote;
use Pugs::Grammar::Operator;
use Pugs::Grammar::StatementControl;
use base 'Pugs::Grammar::Base';
use Carp;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

my $rx_end_with_blocks = qr/
                ^ \s* (?: 
                            [})\]] 
                          | $
                        )
            /xs;
my $rx_end_no_blocks = qr/
                ^
                (?: 
                    \s+ {
                  | \s* (?: 
                            [})\]] 
                          | -> 
                          | $
                        )  
                )
            /xs;

# this is not thread-safe, but it saves time in Parse::Yapp 
# XXX - this optimization is no longer needed, as the optimization in Grammar::Operator worked best
our ( $p, $match, $pos, $rx_end, $allow_modifier, $statement_modifier,
    $allow_semicolon );
# our ( $reentrant, $last_reentrant ) = (0,0);

sub parse {
    #print "perl6_expression param: ", Dumper @_;
    my $pos = $_[2]{p} || 0;
    my ( $ast, $to ) = ast( $_[1], $_[2] );
    my $match = Pugs::Runtime::Match->new( { 
        bool    => \( $ast ? 1 : 0 ),
        str     => \$_[1],
        match   => [],
        from    => \$pos,
        to      => \$to,
        capture => \$ast,
    } );
    #print "Expression: ",Dumper( $match->() );
    return $match;
};

sub ast {
    local ( $p, $match, $pos, $rx_end, $allow_modifier, $statement_modifier,
        $allow_semicolon );
    #    if $reentrant && $reentrant >= $last_reentrant;
    # $last_reentrant = $reentrant;
    # $reentrant++;
    #print " $reentrant ";

    $match = shift;
    my $param = shift;
    $pos = $param->{p} || 0;
    #my $s = substr( $_[0], $pos );
    #print "pos: $pos\n";

    my $no_blocks    = exists $param->{args}{no_blocks}       ? 1 : 0;
    $allow_modifier  = exists $param->{args}{allow_modifier}  ? 1 : 0;
    $allow_semicolon = exists $param->{args}{allow_semicolon} ? 1 : 0;
    #print "don't parse blocks: $no_blocks ";
    #print "allow modifier: $allow_modifier \n";
    $rx_end = $no_blocks 
                ? $rx_end_no_blocks
                : $rx_end_with_blocks;
    $statement_modifier = undef;
    
    $match .= '';
    if  (  substr( $match, $pos ) =~ /$rx_end/ 
        || (  !$allow_semicolon
           && substr( $match, $pos ) =~ /^\s* ; /xs
           )
        ) {
        # end of parse
        # $reentrant--;
        return (undef, $match);
    }
    #print "Grammar::Expression::ast '$match' \n";

    $p = Pugs::Grammar::Operator->new(
        yylex => sub {
            my ( $label, $node );
            ( $label, $node, $pos ) = lexer( $match, $pos, $rx_end );
            #print "Expression: at $pos\n";
            ( $label, $node );
        },
        yyerror => sub { 
            local $Carp::CarpLevel = 2;
            croak "parsing error in Expression: ..." . substr($match,$pos,30) . "... "; 
        },
    ); 
    
    my $out=$p->YYParse(yydebug => 0);

    if ( $statement_modifier ) {
        $pos = $statement_modifier->to;
        $out = {
            statement => $statement_modifier->()->{'statement'},
            exp1 => $statement_modifier->()->{'exp1'},
            exp2 => $out,
        }; 
    }

    #print "Expression: ", Dumper( $out );
    # $reentrant--;
    return ( $out, $pos );
}

sub lexer {
    my ( $match, $pos, $rx_end ) = @_;

        #print "Lexer: start\n";
        #print "Grammar::Expression::ast::lex '$match' \n";
        if ( substr( $match, $pos ) =~ /$rx_end/  
           || (  !$allow_semicolon
              && substr( $match, $pos ) =~ /^\s* ; /xs
              )
           ) {
            #warn "end of expression at: [",substr($match,0,10),"]";
            return ('', '', $pos);
        }

        my @expect = $p->YYExpect;  # XXX is this expensive?
        #print "Expect: @expect \n";
        my $expect_term = grep { $_ eq 'NUM' || $_ eq 'BAREWORD' } @expect;

        # -- this only optimizes about 2%
        #my $expect = $p->{STATES}[$p->{STACK}[-1][0]]{ACTIONS};
        #print "Expect: ", Dumper( keys %{$expect} );
        #my $expect_term =  exists $expect->{'NUM'} 
        #                || exists $expect->{'BAREWORD'};
        
        my $expect_end =
            (  
                # $reentrant == 1 &&     # XXX - doesn't work inside sub{}
                $allow_modifier &&       #     - but this should work
                grep { $_ eq 'postfix:<++>' } @expect
            );
            
        # a new-line after a block may terminate a statement
        if ( $expect_end ) {
            #print "Expecting end-of-expression at $pos \n";
            #print "  $match\n";
            #print "  ", (" "x$pos), "^", "\n";
            
            if ( substr( $match, $pos-1, 1 ) eq '}' ) {
                #print "  It was a block\n";
                # TODO - "unspace"
                my $spaces = Pugs::Grammar::BaseCategory->ws( $match, { p => $pos } );
                # does the spaces contain 'newline' ?
                return ('', '', $pos)
                    if $spaces =~ /\n/s;
            }
        }

        my $m = Pugs::Grammar::BaseCategory->ws( $match, { p => $pos } );
        # print "match is ",Dumper($m),"\n";
        if ( $m ) {
            $pos = $m->to;
            #print "pos after <ws>: $pos\n";
        }
        
        # a statement modifier can also terminate a statement
        #print "test modifier at $pos \n";
        if (  # $expect_end  && --- XXX not working?
              $allow_modifier 
           ) {
            $statement_modifier = Pugs::Grammar::StatementModifier->parse( $match, { p => $pos } );
            return ('', '', $pos)
                if $statement_modifier;
        }

        #print "Lexer: try <Operator|Term|Quote>\n";

        my $m1 = Pugs::Grammar::Operator->parse( $match, { p => $pos } );
        #print "Lexer: Operator done\n";
        my $m2;
        if ( $expect_term ) {
            $m2 = Pugs::Grammar::Term->parse( $match, { p => $pos } );
            #print "Lexer: Term done\n";
            $m2 = Pugs::Grammar::Quote->parse( $match, { p => $pos } )
                unless $m2;
        }
        #print "Lexer: m1 = " . Dumper($m1) . "m2 = " . Dumper($m2);

        my $pos2;
        while(1) {
            $pos2 = $m2->to if $m2;
            # term.>>meth() 
            if ( $m2 && $m2->tail && $m2->tail =~ /^(\.(?:>>|»)\.?|(?:>>|»)\.)/ ) {
                my $meth = Pugs::Grammar::Term->parse( $match, { p => $pos2 + length($1) } );
                $meth->data->{capture} = \{ 
                    op1  => 'method_call_hyper', 
                    hyper => 1,
                    self => $m2->(), 
                    method => $meth->(),
                    param => undef,
                };
                $m2 = $meth;
                next;
            }
            # term.meth() 
            if ( $m2 && $m2->tail && $m2->tail =~ /^\.[^.([{<«]/ ) {
                my $meth = Pugs::Grammar::Term->parse( $match, { p => $pos2 } );
                $meth->data->{capture} = \{ 
                    op1  => 'method_call', 
                    self => $m2->(), 
                    method => $meth->(),
                    param => undef,
                };
                $m2 = $meth;
                next;
            }
            # term.() 
            if ( $m2 && $m2->tail && $m2->tail =~ /^\.[([{<«]/ ) {
                $pos2++;
            }
            # term() 
            if ( $m2 && $m2->tail && $m2->tail =~ /^\.?\(/ ) {
                my $paren = Pugs::Grammar::Term->parse( $match, { p => $pos2 } );
                #print "paren: ",Dumper($paren);
                if ( exists $m2->()->{dot_bareword} ) {
                    $paren->data->{capture} = \{ 
                        op1 => 'method_call', 
                        self => { 'scalar' => '$_' }, 
                        method => $m2->(), 
                        param => $paren->(), 
                    };
                }
                elsif ( exists $m2->()->{op1} 
                     && $m2->()->{op1} eq 'method_call'
                     && ! defined $m2->()->{param} 
                ) {
                    $paren->data->{capture} = \{ 
                        %{$m2->()}, 
                        param => $paren->(), 
                    };
                }
                else {
                    $paren->data->{capture} = \{ 
                        op1 => 'call', 
                        sub => $m2->(), 
                        param => $paren->(), 
                    };
                }
                $m2 = $paren;
                next;
            }
            # term[] 
            if ( $m2 && $m2->tail && $m2->tail =~ /^\.?\[/ ) {
                my $paren = Pugs::Grammar::Term->parse( $match, { p => $pos2 } );
                if ( exists $m2->()->{dot_bareword} ) {
                    $paren->data->{capture} = \{ 
                        op1 => 'method_call', 
                        self => { 'scalar' => '$_' }, 
                        method => { bareword => '[]' },
                        param => $paren->()->{exp1}, 
                    };
                }
                elsif ( exists $m2->()->{op1} 
                     && $m2->()->{op1} eq 'method_call'
                     && ! defined $m2->()->{param} 
                ) {
                    $paren->data->{capture} = \{ 
                        %{$m2->()},
                        method => { bareword => '[]' },
                        param => $paren->()->{exp1}, 
                    };
                }
                else {
                    $paren->data->{capture} = \{ 
                        fixity => 'postcircumfix', 
                        op1 => "[", 
                        op2 => "]", 
                        exp1 => $m2->(), 
                        exp2 => $paren->()->{exp1}, 
                    };
                }
                $m2 = $paren;
                next;
            }
            # term{} 
            if ( $m2 && $m2->tail && $m2->tail =~ /^\.?\{/ ) {
                my $paren = Pugs::Grammar::Term->parse( $match, { p => $pos2 } );
                if ( exists $m2->()->{dot_bareword} ) {
                    $paren->data->{capture} = \{ 
                        op1 => 'method_call', 
                        self => { 'scalar' => '$_' }, 
                        method => { bareword => '{}' },
                        param => $paren->()->{'bare_block'}, 
                    };
                }
                elsif ( exists $m2->()->{op1} 
                     && $m2->()->{op1} eq 'method_call'
                     && ! defined $m2->()->{param} 
                ) {
                    $paren->data->{capture} = \{ 
                        %{$m2->()},
                        method => { bareword => '{}' },
                        param => $paren->()->{'bare_block'}, 
                    };
                }
                else {
                    my $block = $paren->()->{'bare_block'};
                    if ( @{ $block->{'statements'} } < 2 ) {
                        $paren->data->{capture} = \{ 
                            fixity => 'postcircumfix', 
                            op1 => "{", 
                            op2 => "}", 
                            exp1 => $m2->(), 
                            exp2 => $block->{'statements'}[0], 
                        };
                    }
                    else {
                        # multidim
                        $paren->data->{capture} = \{ 
                            fixity => 'postcircumfix', 
                            op1 => "{", 
                            op2 => "}", 
                            exp1 => $m2->(), 
                            exp2 => $block, 
                        };
                    }
                }
                $m2 = $paren;
                next;
            }
            # term<> 
            if ( $m2 && $m2->tail 
                && $m2->tail =~ /^\.?[<«]/ ) {
                # XXX - is '<' a quote?
                my $paren = Pugs::Grammar::Quote->parse( $match, { p => $pos2 } );
                if ( exists $m2->()->{dot_bareword} ) {
                    $paren->data->{capture} = \{ 
                        op1 => 'method_call', 
                        self => { 'scalar' => '$_' }, 
                        method => { bareword => '<>' }, 
                        param => $paren->(), 
                    };
                }
                elsif ( exists $m2->()->{op1} 
                     && $m2->()->{op1} eq 'method_call'
                     && ! defined $m2->()->{param} 
                ) {
                    $paren->data->{capture} = \{ 
                        %{$m2->()}, 
                        method => { bareword => '<>' },
                        param => $paren->(), 
                    };
                }
                else {
                    $paren->data->{capture} = \{ 
                        fixity => 'postcircumfix', 
                        op1 => "{", 
                        op2 => "}", 
                        exp1 => $m2->(), 
                        exp2 => $paren->(), 
                    };
                }
                $m2 = $paren;
                next;
            }
            last;
        } # /while

        # longest token
        $m = undef;
        if ( $m1 && $m2 ) {
            if ( $m1->to < $m2->to ) {
                $m = $m2
            }
            else {
                $m = $m1
            }
        }
        else {
            $m = $m1 if $m1;
            $m = $m2 if $m2;
        }
        return ('','', $pos) unless ref $m;
        #print "Lexer: Term or expression: ",Dumper( $m->() );

# <fglock> like: ( name 1, 2 or 3 ) - is it parsed as name(1,2 or 3) or (name(1,2) or 3)
# <TimToady> it will be taken provisionally as a listop, with listop precedence
# <TimToady> so name(1,2) or 3
# <TimToady> but it will fail compilation if name is not supplied by CHECK time.
# <TimToady> it will also fail if name is declared as a unary or 0-ary func.

        my $ast = $m->();
        $ast->{pos} = $pos;
        #print "pos after op: $pos\n";
        $pos = $m->to if $m;
        my $t;
        if ( exists $ast->{stmt} ) {
            $t = [ $ast->{stmt} => $ast ]
        }
        elsif ( exists $ast->{op} ) {
            if ( exists $ast->{reduce} ) {
                $t = [ 'REDUCE' => $ast ]
            }
            else {
                $t = [ $ast->{op} => $ast->{op} ];
            }
        }
        elsif ( exists $ast->{bareword} ) {
            $t = [ 'BAREWORD' => $ast ]
        }
        elsif ( exists $ast->{dot_bareword} ) {
            $t = [ 'DOT_BAREWORD' => $ast ]
        }
        else {
            $t = [ 'NUM' => $ast ]
        }
        #print "Term: ",Dumper($t); #, "MATCH $match\n";
        $t=['',''] unless $ast;  #$match; # defined($t);

        #print "expect NUM \n" if grep { $_ eq 'NUM' } @expect;
        #print "expect '/' \n" if grep { $_ eq '/' }   @expect;

        #print "token: $$t[0] ", Dumper( $$t[1] ); #, $match;
        #print "expect: ", Dumper( @expect );

        return($$t[0],$$t[1], $pos );
}

1;
