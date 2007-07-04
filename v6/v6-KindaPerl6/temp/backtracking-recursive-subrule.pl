
=pod

This Perl 5 program demonstrates a prototype for:

- a Perl6 Match class
- backtracking into subrules
- OO rules with Grammar inheritance
- captures
- 'Perl' subrules (non-regex)

Tested with perl 5.8.8

TODO:

- convert the intermediate data structure to a Match
- 'return' blocks
- regex parameters

Caveats:

- the compiler must detect which subs are 'regex' or 'Perl'
  or, compile all subs as both regex and Perl

=cut

use strict;

# This is the "Runtime" library

{
    package Match;
    use Data::Dumper;
    sub new {
        bless { 
            array  => [], 
            hash   => {}, 
            bool   => 0, 
            result => undef,
            from   => undef, 
            to     => undef, 
            match_str => undef,
        }, $_[0];
    }
    sub clone {
        bless { 
            array  => [ @{$_[0]->{array}} ], 
            hash   => { %{$_[0]->{hash}} }, 
            bool   => $_[0]->{bool}, 
            result => $_[0]->{result},
            from   => $_[0]->{from}, 
            to     => $_[0]->{to}, 
            match_str => $_[0]->{match_str}, 
        }, ref $_[0];
    }
    sub array  :lvalue { $_[0]->{array} }
    sub hash   :lvalue { $_[0]->{hash} }
    sub bool   :lvalue { $_[0]->{bool} }
    sub result :lvalue { $_[0]->{result} }
    sub from   :lvalue { $_[0]->{from} }
    sub to     :lvalue { $_[0]->{to} }
    sub match_str :lvalue { $_[0]->{match_str} }
    
    sub str {
          $_[0]->bool 
        ? substr( ${$_[0]->match_str}, $_[0]->from, $_[0]->to - $_[0]->from )
        : undef;
    }
    sub perl {
        Dumper( $_[0] );
    }

    our @Matches;
    sub from_global_data {
        unless ( defined $_[0] ) {
            # no match
            push @Matches, Match->new();
            return;
        }

        my ( $previous, $action, @data ) = @{+shift};
        if ( defined $previous ) {
            from_global_data( $previous );
        }
        
        # XXX - use a dispatch table
        if ( $action eq 'create' ) {
            push @Matches, Match->new();
            $Matches[-1]->bool = 1;
            $Matches[-1]->from = $data[0];
            $Matches[-1]->match_str = $data[1];
        }
        elsif ( $action eq 'to' ) {
            $Matches[-1]->to = $data[0];
        }
        elsif ( $action eq 'capture' ) {
            # XXX - named captures, pre-numbered captures
            my $match = pop @Matches;
            push @{ $Matches[-1]->array }, $match;
        }
        else {
            die "no action like '$action'"
        }
        
        # print "action: $action [ @data ]\n";

    }

}

# This is the base Grammar for tests

{
    package MyGrammar;
      
    #  Perl 6:  regex rule0 { a* }
    our $rule0_qr = qr/
          (?{ 
            local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ];
            $GLOBAL::_M2 = $GLOBAL::_M;
          })         

            a*
            
          (?{ 
            local $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ];
            $GLOBAL::_M2 = $GLOBAL::_M;
          })   
        /x;

    #our $rule0_sub = sub {
    sub rule0_sub {
            local $GLOBAL::_Class = shift;
            /$rule0_qr/;
        };

    #  Perl 6:  regex rule1 { <rule0> aaaa }
    our $rule1_qr = qr/
          (?{ 
            local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ];
            $GLOBAL::_M2 = $GLOBAL::_M;
          })                   
          
            # Call Subrule
            (?:
                (??{ 
                    eval '$'.$GLOBAL::_Class.'::rule0_qr' 
                })
                (?{ 
                    local $GLOBAL::_M = [ $GLOBAL::_M, 'capture' ];
                })
            )
          
          aaaa
          
          (?{ 
            local $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ];
            $GLOBAL::_M2 = $GLOBAL::_M;
          })   
        /x;

    #our $rule1_sub = sub {
    sub rule1_sub {
            local $GLOBAL::_Class = shift;
            /$rule1_qr/;
        };

}

# This Grammar inherits the base Grammar, and redefine a subrule
      
{
    package MyGrammar2;
    use base 'MyGrammar';
      
    #  Perl 6:  regex rule0 { b* }
    our $rule0_qr = qr/
          (?{ 
            local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ];
            $GLOBAL::_M2 = $GLOBAL::_M;
          })         

            b*
            
          (?{ 
            local $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ];
            $GLOBAL::_M2 = $GLOBAL::_M;
          })   
        /x;

    #our $rule0_sub = sub {
    sub rule0_sub {
            local $GLOBAL::_Class = shift;
            /$rule0_qr/;
        };
}

# This Grammar inherits the base Grammar,  and redefine a subrule with Perl code

{
    package MyGrammar3;
    use base 'MyGrammar';
      
    #  Perl 6: 
    # 
    #  method rule0 { 
    #        $/.from = pos();
    #        $/.to = pos() + 3;
    #        $/.bool = 1;
    #        $/.match_str := $_;
    #  }
    #
    our $rule0_qr = qr/
          (?{ 
            local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ];
            $GLOBAL::_M2 = $GLOBAL::_M;
          })         
          (?{ 
            $GLOBAL::MATCH = Match->new();
            $GLOBAL::MATCH->from = pos;
            $GLOBAL::MATCH->match_str = \$_;

            $GLOBAL::_Class->rule0_sub();
          })   
          
          (??{ 
            # (pos - to) ???
            '.{' . ($GLOBAL::MATCH->to - $GLOBAL::MATCH->from) . '}'
          })

          (?{ 
            # TODO - unpack $GLOBAL::MATCH
            local $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ];
            $GLOBAL::_M2 = $GLOBAL::_M;
          })   

        /x;

    #our $rule0_sub = sub {
    sub rule0_sub {
            local $GLOBAL::_Class = shift;
            # ... some code ...

            # set the Match object
            $GLOBAL::MATCH->from = pos();
            $GLOBAL::MATCH->to = pos() + 3;
            $GLOBAL::MATCH->bool = 1;
            $GLOBAL::MATCH->match_str = \$_;
        };
}

# This Grammar inherits the base Grammar, and redefine a recursive subrule
      
{
    package MyGrammar4;
    use base 'MyGrammar';
      
    #  Perl 6:  regex rule0 { b <rule0> | <null> }
    our $rule0_qr = qr/
          (?{ 
            local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ];
            $GLOBAL::_M2 = $GLOBAL::_M;
          })         

          (?:
                # 'b' and recurse
                b
                # recurse (Perl 5.10)
                ## (?R)
                # recurse (Perl 5.8)
                (??{ 
                    eval '$'.$GLOBAL::_Class.'::rule0_qr' 
                })
                # optional capture
                (?{ 
                    local $GLOBAL::_M = [ $GLOBAL::_M, 'capture' ];
                })
          |
                # or nothing
          )

          (?{ 
            local $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ];
            $GLOBAL::_M2 = $GLOBAL::_M;
          })   
        /x;

    #our $rule0_sub = sub {
    sub rule0_sub {
            local $GLOBAL::_Class = shift;
            /$rule0_qr/;
        };
}

# Tests

use Test::More qw(no_plan);
use Data::Dumper;

{
    local $_ = '    aaazzzz    ';
    local $GLOBAL::_M2;
    MyGrammar->rule1_sub();
    #print " global: ", Dumper( $GLOBAL::_M2 );
    Match::from_global_data( $GLOBAL::_M2 );
    $GLOBAL::MATCH = shift @Match::Matches;
    #print Dumper( \@Match::Matches );
    print " result1 ", $GLOBAL::MATCH->perl, "\n";
    is( $GLOBAL::MATCH->str, undef, 'no match' );
}

{
    local $_ = '    aaaaaaaaaa    ';
    local $GLOBAL::_M2;
    MyGrammar->rule1_sub();
    #print " global: ", Dumper( $GLOBAL::_M2 );
    Match::from_global_data( $GLOBAL::_M2 );
    $GLOBAL::MATCH = shift @Match::Matches;
    #print Dumper( \@Match::Matches );
    print " result1 ", $GLOBAL::MATCH->perl, "\n";
    is( $GLOBAL::MATCH->str, 'aaaaaaaaaa', 'backtracks into subrule' );
}

{
    local $_ = '    aabbaaaaaaa    ';
    local $GLOBAL::_M2;
    MyGrammar2->rule1_sub();
    #print " global: ", Dumper( $GLOBAL::_M2 );
    Match::from_global_data( $GLOBAL::_M2 );
    $GLOBAL::MATCH = shift @Match::Matches;
    #print Dumper( \@Match::Matches );
    print " result2 ", $GLOBAL::MATCH->perl, "\n";
    is( $GLOBAL::MATCH->str, 'bbaaaa', 'redefined subrule' );
}

{
    local $_ = '    a123aaaaaaa    ';
    MyGrammar3->rule1_sub();
    #print " global: ", Dumper( $GLOBAL::_M2 );
    Match::from_global_data( $GLOBAL::_M2 );
    $GLOBAL::MATCH = shift @Match::Matches;
    #print Dumper( \@Match::Matches );
    print " result3 ", $GLOBAL::MATCH->perl, "\n";
    is( $GLOBAL::MATCH->str, '123aaaa', 'subrule is Perl code' );
}

{
    local $_ = '    aabbaaaaaaa    ';
    MyGrammar4->rule1_sub();
    #print " global: ", Dumper( $GLOBAL::_M2 );
    Match::from_global_data( $GLOBAL::_M2 );
    $GLOBAL::MATCH = shift @Match::Matches;
    #print Dumper( \@Match::Matches );
    print " result4 ", $GLOBAL::MATCH->perl, "\n";
    is( $GLOBAL::MATCH->str, 'bbaaaa', 'recursive subrule' );
}

