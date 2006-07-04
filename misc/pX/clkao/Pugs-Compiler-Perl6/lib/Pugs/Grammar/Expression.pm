package Pugs::Grammar::Expression;

use strict;
use warnings;

#use Pugs::Compiler::Rule;
use Pugs::Grammar::Precedence;
use Pugs::Grammar::Term;
use Pugs::Grammar::Operator;
use Pugs::Grammar::StatementControl;
use base 'Pugs::Grammar::Base';
use Carp;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

# XXX - PCR is not calling this
*ws = &Pugs::Grammar::BaseCategory::ws;

my $rx_end_with_blocks = qr/
                ^ \s* (?: 
                            [});] 
                          | if \s 
                          | unless \s
                          | for \s 
                          | $
                        )
            /xs;
my $rx_end_no_blocks = qr/
                ^
                (?: 
                    \s+ {
                  | \s* (?: 
                            [});] 
                          | if \s 
                          | unless \s
                          | for \s 
                          | -> 
                          | $
                        )  
                )
            /xs;

sub ast {
    my $match = shift;
    my $param = shift;

    my $no_blocks = exists $param->{args}{no_blocks} ? 1 : 0;
    # warn "don't parse blocks: $no_blocks ";
    my $rx_end = $no_blocks 
                ? $rx_end_no_blocks
                : $rx_end_with_blocks;

    $match .= '';
    if ( $match =~ /$rx_end/ ) {
        # end of parse
        return (undef, $match);
    }
    #print "Grammar::Expression::ast '$match' \n";
    my $p;
    my $last = length( $match );
    
    my $lex = sub {
        #print "Grammar::Expression::ast::lex '$match' \n";
        if ( $match =~ /$rx_end/ ) {
            #warn "end of expression at: [",substr($match,0,10),"]";
            return ('', '');
        }
        #print "still here\n";
        my $m;
        my $whitespace_before = 0;

        my @expect = $p->YYExpect;  # XXX is this expensive?
        
        for ( 1 ) {
            $m = Pugs::Grammar::BaseCategory->ws( $match );
            # <ws> is nonstandard in that it returns a hashref instead of a Match
            # print "match is ",Dumper($m),"\n";
            if ( $m->{bool} ) {
                $match = $m->{tail};
                $whitespace_before = 1;
            }
            
            # print "tail $match \n"; 
            
            # XXX @expect should use symbolic names; better use TABLE instead of 'literal'
            #print " @{[ sort @expect ]} \n";
            # if ( grep { $_ eq '++' || $_ eq '{' } @expect ) {
            
            # XXX temporary hack - matching options in 'expected' order should fix this
            if ( $match =~ /^</ ) {   # && ! $whitespace_before ) {
                # XXX - angle quotes are always tried even if it were expecting a simple '<'
            
                # after whitespace means '<' (default)
                # without whitespace means '<str>'
                #print "checking angle quote ... [$whitespace_before]\n";
                $m = Pugs::Grammar::Term->angle_quoted( substr($match, 1), { p => 1 } );
                if ( $m ) {
                    #print "Match: ",Dumper $m->();
                    if ( grep { $_ eq 'NUM' } @expect ) {
                        # expects a term
                        $m = Pugs::Runtime::Match->new( { 
                            bool  => 1,
                            match => $m,
                            tail  => $$m->{tail},
                            capture => { angle_quoted => $m->() },
                        } );
                        #print "Match: ",Dumper $m->();
                        last;
                    }
                    # expects an op
                    # x < 1  --- less than
                    # x<1    --- starts angle-quote
                    unless ( $whitespace_before ) {
                        $m = Pugs::Runtime::Match->new( { 
                            bool  => 1,
                            match => $m,
                            tail  => $$m->{tail},
                            capture => { op => "ANGLE", angle_quoted => $m->() },
                        } );
                        #print "Match: ",Dumper $m->();
                        last;
                    }
                }
            }

            my $m1 = Pugs::Grammar::Operator->parse( $match, { p => 1 } );
            my $m2 = Pugs::Grammar::Term->parse( $match, { p => 1 } );
            #warn "m1 = " . Dumper($m1->()) . "m2 = " . Dumper($m2->());

        while(1) {
            # term.meth() is high-precedence
            if ( $m2 && $$m2->{tail} && $$m2->{tail} =~ /^\.[^.]/ ) {
                my $meth = Pugs::Grammar::Term->parse( $$m2->{tail}, { p => 1 } );
                $$meth->{capture} = { 
                    op1  => 'method_call', 
                    self => $m2->(), 
                    method => $meth->(),
                    param => undef,
                };
                $m2 = $meth;
                next;
            }
            # term() is high-precedence
            if ( $m2 && $$m2->{tail} && $$m2->{tail} =~ /^\(/ ) {
                my $paren = Pugs::Grammar::Term->parse( $$m2->{tail}, { p => 1 } );
                if ( exists $m2->()->{dot_bareword} ) {
                    $$paren->{capture} = { 
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
                    $$paren->{capture} = { 
                        %{$m2->()}, 
                        param => $paren->(), 
                    };
                }
                else {
                    $$paren->{capture} = { 
                        op1 => 'call', 
                        sub => $m2->(), 
                        param => $paren->(), 
                    };
                }
                $m2 = $paren;
                next;
            }
            last;
        }

            # longest token
            if ( $m1 && $m2 ) {
                if ( length($$m1->{tail}) > length($$m2->{tail}) ) {
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
            last if $m;
            
            local $Carp::CarpLevel = 2;
            carp "unrecognized token '",substr($match,0,10),"'\n"
                if $match;            
        } # /for
            
        my $tail = $$m->{tail};

        # method call
 #       if ( defined $tail && $tail =~ /^\./ ) {
 #               # TODO - long dot
 #               my $meth = Pugs::Grammar::Term->parse( $tail, { p => 1 } );
 #               $meth->()->{self} = $m->();
 #               $m = $meth;
 #               $tail = $$m->{tail};
 #               #print "Method: ",Dumper $m->();

# TODO -
# <fglock> like: ( name 1, 2 or 3 ) - is it parsed as name(1,2 or 3) or (name(1,2) or 3)
# <TimToady> it will be taken provisionally as a listop, with listop precedence
# <TimToady> so name(1,2) or 3
# <TimToady> but it will fail compilation if name is not supplied by CHECK time.
# <TimToady> it will also fail if name is declared as a unary or 0-ary func.

  #      }


        {
            # trim tail
            my $tmp = $tail;
            $match = $tmp if defined $tmp;  # match failure doesn't kill $match (PCR "bug")
        }

        #print Dumper $m;
        #print $match;
        my $ast = $m->();

        $ast->{pos} = $last - length( $match );
        my $t;
        if ( exists $ast->{stmt} ) {
            # unused!
            if ( $ast->{stmt} eq '{' ) {
                $t = [ 'BLOCK_START' => $ast ]
            }
            elsif ( $ast->{stmt} eq '}' ) {
                $t = [ 'BLOCK_END' => $ast ]
            }
            else {
                $t = [ $ast->{stmt} => $ast ]
            }
        }
        elsif ( exists $ast->{op} ) {
            if (  $ast->{op} eq 'my' 
               || $ast->{op} eq 'our' 
               || $ast->{op} eq 'has' ) {
                $t = [ 'MY' => $ast ]
            }
            else {
                $t = [ $ast->{op} => $ast ];
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
        #warn "T ",Dumper($t), "MATCH $match\n";
        $t=['',''] unless $ast;  #$match; # defined($t);

        #print "expect NUM \n" if grep { $_ eq 'NUM' } @expect;
        #print "expect '/' \n" if grep { $_ eq '/' }   @expect;

        #print "token: $$t[0] ", Dumper( $$t[1] ); #, $match;
        #print "expect: ", Dumper( @expect );

        return($$t[0],$$t[1]);
    };

    $p = Pugs::Grammar::Operator->new(
        yylex => $lex, 
        yyerror => sub { 
            local $Carp::CarpLevel = 2;
            carp "parsing error in Expression: ..." . substr($match,0,30) . "... "; 
        },
    );

    my $out=$p->YYParse(yydebug => 0);
    #print Dumper $out;
    return ( $out, $match );
}

1;
