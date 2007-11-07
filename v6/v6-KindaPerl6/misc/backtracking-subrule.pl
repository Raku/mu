
=pod

This Perl 5 program demonstrates a prototype for:

- a Perl6 Match class
- backtracking into subrules
- OO rules with Grammar inheritance
- captures
- 'Perl' subrules (non-regex)

Tested with perl 5.8.8

TODO:

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
        substr( ${$_[0]->match_str}, $_[0]->from, $_[0]->to - $_[0]->from );
    }
    sub perl {
        Dumper( $_[0] );
    }
}

# This is the base Grammar for tests

{
    package MyGrammar;
      
    our $rule0_qr = qr/
          (?{ 
            local $GLOBAL::_M = Match->new(); 
            $GLOBAL::MATCH = $GLOBAL::_M;
            $GLOBAL::_M->from = pos;
            $GLOBAL::_M->match_str = \$_;
          })         

            a*
            
          (?{ 
            $GLOBAL::_M->bool = 1;
            $GLOBAL::_M->to = pos;
            $GLOBAL::MATCH = $GLOBAL::_M;
          })   
        /x;

    #our $rule0_sub = sub {
    sub rule0_sub {
            local $GLOBAL::_Class = shift;
            /$rule0_qr/;
        };

    our $rule1_qr = qr/
          (?{ 
            local $GLOBAL::_M = Match->new(); 
            $GLOBAL::MATCH = $GLOBAL::_M;
            $GLOBAL::_M->from = pos;
            $GLOBAL::_M->match_str = \$_;
            #print " ",$GLOBAL::_M->perl,"\n";
          })         
          
          (?:
            (?{ 
                $GLOBAL::MATCH = $GLOBAL::_M;
            })
            
            (??{ eval '$'.$GLOBAL::_Class.'::rule0_qr' })
            
            (?{
                local $GLOBAL::_M = $GLOBAL::_M->clone();  
                push @{ $GLOBAL::_M->array }, $GLOBAL::MATCH;
                #print " ",$GLOBAL::_M->perl(),"\n";
            })
          )
          
          aaaa
          
          (?{ 
            $GLOBAL::_M->bool = 1;
            $GLOBAL::_M->to = pos;
            $GLOBAL::MATCH = $GLOBAL::_M;
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
      
    our $rule0_qr = qr/
          (?{ 
            local $GLOBAL::_M = Match->new(); 
            $GLOBAL::MATCH = $GLOBAL::_M;
            $GLOBAL::_M->from = pos;
            $GLOBAL::_M->match_str = \$_;
            #print "looking for 'b'\n";
          })         

            b*
            
          (?{ 
            $GLOBAL::_M->bool = 1;
            $GLOBAL::_M->to = pos;
            $GLOBAL::MATCH = $GLOBAL::_M;
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
      
    our $rule0_qr = qr/
          (?{ 
            local $GLOBAL::_M = Match->new(); 
            $GLOBAL::MATCH = $GLOBAL::_M;
            $GLOBAL::_M->from = pos;
            $GLOBAL::_M->match_str = \$_;

            $GLOBAL::_Class->rule0_sub();
          })   
          
          (??{ 
            '.{' . ($GLOBAL::MATCH->to - $GLOBAL::MATCH->from) . '}'
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
      
    our $rule0_qr = qr/
          (?{ 
            local $GLOBAL::_M = Match->new(); 
            $GLOBAL::MATCH = $GLOBAL::_M;
            $GLOBAL::_M->from = pos;
            $GLOBAL::_M->match_str = \$_;
            #print "looking for 'b'\n";
          })         

          (?:
              (?:
                # 'b' and recurse
                b

                (?{ 
                    $GLOBAL::MATCH = $GLOBAL::_M;
                })
                
                (??{ eval '$'.$GLOBAL::_Class.'::rule0_qr' })
                
                (?{
                    local $GLOBAL::_M = $GLOBAL::_M->clone();  
                    push @{ $GLOBAL::_M->array }, $GLOBAL::MATCH;
                    #print " ",$GLOBAL::_M->perl(),"\n";
                })
              )
          |
              # or nothing
          )

          (?{ 
            $GLOBAL::_M->bool = 1;
            $GLOBAL::_M->to = pos;
            $GLOBAL::MATCH = $GLOBAL::_M;
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

{
    local $_ = '    aaaaaaaaaa    ';
    MyGrammar->rule1_sub();
    print " result1 ", $GLOBAL::MATCH->perl, "\n";
    is( $GLOBAL::MATCH->str, 'aaaaaaaaaa', 'backtracks into subrule' );
}

{
    local $_ = '    aabbaaaaaaa    ';
    MyGrammar2->rule1_sub();
    print " result2 ", $GLOBAL::MATCH->perl, "\n";
    is( $GLOBAL::MATCH->str, 'bbaaaa', 'redefined subrule' );
}

{
    local $_ = '    a123aaaaaaa    ';
    MyGrammar3->rule1_sub();
    print " result3 ", $GLOBAL::MATCH->perl, "\n";
    is( $GLOBAL::MATCH->str, '123aaaa', 'subrule is Perl code' );
}

{
    local $_ = '    aabbaaaaaaa    ';
    MyGrammar4->rule1_sub();
    print " result4 ", $GLOBAL::MATCH->perl, "\n";
    is( $GLOBAL::MATCH->str, 'bbaaaa', 'recursive subrule' );
}

