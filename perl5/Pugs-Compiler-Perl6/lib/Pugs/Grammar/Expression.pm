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

sub ast {
    my $match = shift;
    $match .= '';
    # print "Grammar::Expression::AST '$match' \n";
    my $p;
    my $last = length( $match );
    my $lex = sub {
        my $m;
        my $whitespace_before = 0;

        for ( 1 ) {
            $m = Pugs::Grammar::BaseCategory->ws( $match );
            # <ws> is nonstandard in that it returns a hashref instead of a Match
            # print "match is ",Dumper($m),"\n";
            if ( $m->{bool} ) {
                $match = $m->{tail};
                $whitespace_before = 1;
            }
            my @expect = $p->YYExpect;  # XXX is this expensive?
            
            # print "tail $match \n"; 
            
            # XXX @expect should use symbolic names; better use TABLE instead of 'literal'
            #print " @{[ sort @expect ]} \n";
            # if ( grep { $_ eq '++' || $_ eq '{' } @expect ) {
            $m = Pugs::Grammar::StatementControl->parse( $match, { p => 1 } );
            last if ( $m );
            #}
            
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
            
            # XXX temporary hack - matching options in 'expected' order should fix this
            if ( $match =~ /^{/ ) {
                # after whitespace means block-start
                #print "checking { ... [$whitespace_before]\n";
                if ( $whitespace_before ) {
                    $m = Pugs::Runtime::Match->new( { 
                        bool  => 1,
                        match => '{',
                        tail  => substr( $match, 1 ),
                        capture => { stmt => '{' },
                    } );
                    #print "Match: ",Dumper $m->();
                    last;
                }
            }

            $m = Pugs::Grammar::Operator->parse( $match, { p => 1 } );
            last if ( $m );
            $m = Pugs::Grammar::Term->parse( $match, { p => 1 } );
            last if ( $m );
            
            local $Carp::CarpLevel = 2;
            carp "unrecognized token '",substr($match,0,10),"'\n"
                if $match;
            
        } # /for
            
        my $ast = $m->();

        {
            # XXX temporary hack - check if an alphanumeric-ending token is actually 
            #     a longer-token bareword
            #     'ne' vs. ':negate'
            my $name = $ast->{op};
            if (   defined $name 
                && $name =~ /[[:alnum:]]$/ 
                && defined $$m->{tail}
                && $$m->{tail} =~ /^[_[:alnum:]]/ 
            ) {
                #print "mismatched name: $name\n";
                $m = Pugs::Grammar::Term->parse( $match, { p => 1 } );
                $ast = $m->();
            }
        }        

        {
            # trim tail
            my $tmp = $$m->{tail};
            $match = $tmp if defined $tmp;  # match failure doesn't kill $match (PCR "bug")
        }

        $ast->{pos} = $last - length( $match );
        my $t;
        if ( exists $ast->{stmt} ) {

            if ( $ast->{stmt} eq 'if' or $ast->{stmt} eq 'unless' ) {
                $t = [ 'IF' => $ast ]
            }
            elsif ( $ast->{stmt} eq 'sub' 
                || $ast->{stmt} eq 'multi' 
                || $ast->{stmt} eq 'submethod' 
                || $ast->{stmt} eq 'method') {
                $t = [ 'SUB' => $ast ]
            }
            elsif ( $ast->{stmt} eq 'my' 
                || $ast->{stmt} eq 'our' 
                || $ast->{stmt} eq 'has' ) {
                $t = [ 'MY' => $ast ]
            }
            elsif ( $ast->{stmt} eq '{' ) {
                $t = [ 'BLOCK_START' => $ast ]
            }
            else {
                $t = [ $ast->{stmt} => $ast ]
            }
        }
        elsif ( exists $ast->{op} ) {
            $t = [ $ast->{op} => $ast ]
        }
        elsif ( exists $ast->{bareword} ) {
            $t = [ 'BAREWORD' => $ast ]
        }
        else {
            $t = [ 'NUM' => $ast ]
        }
        $t=['',''] unless $match; # defined($t);

        #print "expect NUM \n" if grep { $_ eq 'NUM' } @expect;
        #print "expect '/' \n" if grep { $_ eq '/' }   @expect;

        # print "token: $$t[0] ", Dumper( $$t[1] );
        # print "expect: ", Dumper( @expect );

        return($$t[0],$$t[1]);
    };

    # TODO - check for remaining whitespace!

    $p = Pugs::Grammar::Operator->new(
        yylex => $lex, 
        yyerror => sub { 
            local $Carp::CarpLevel = 2;
            carp "parsing error in Expression: ..." . substr($match,0,30) . "... "; },
    );

    my $out=$p->YYParse;
    #print Dumper $out;
    return ( $out, $match );
}

1;
