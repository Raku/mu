use lib 
    '../Pugs-Compiler-Rule/lib',
    '../Pugs-Compiler-Precedence/lib',
;

use strict;
use warnings;

use Test::More tests => 17;

use Pugs::Compiler::Rule;
use Pugs::Grammar::Precedence;
use Pugs::Grammar::Term;
use Pugs::Grammar::Operator;

use Pugs::Grammar::Expression;
use Pugs::Grammar::StatementControl;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

{
    my $match = Pugs::Grammar::Expression->parse( q(10) );
    #print Dumper $match->();
    is_deeply(
        $match->(), { 'num' => '10' ,}, 
        'the expression compiler looks ok' );
}

{
    my $match = Pugs::Grammar::Expression->parse( q($a) );
    #print Dumper $match->();
    is_deeply(
        $match->(), { 'scalar' => '$a' ,}, 
        'the expression compiler looks ok - var without spaces' );
}

{
    my $match = Pugs::Grammar::Expression->parse( q( $a ) );
    #print Dumper $match->();
    is_deeply(
        $match->(), { 'scalar' => '$a' ,}, 
        'the expression compiler looks ok - var with spaces' );
}

{
    my $match = Pugs::Grammar::Expression->parse( q($a {) );
    #print Dumper $match->();
    is_deeply(
        $match->(), { 'scalar' => '$a' ,}, 
        'the expression compiler looks ok - var before {' );
}

{
    my $match = Pugs::Grammar::Expression->parse( q($a )."\n" );
    #print Dumper $match->();
    is_deeply(
        $match->(), { 'scalar' => '$a' ,}, 
        'the expression compiler looks ok - var before newline' );
}

{
    my $match = Pugs::Grammar::StatementControl->statement_list( q(10) );
    #print Dumper $match->();
    is_deeply(
        $match->(), { 
          #'statements' => [
          #  {
              'num' => '10'
          #  }
          #]
        }, 
        'a simple statement' );
}

{
    my $match = Pugs::Grammar::StatementControl->statement_list( q(10;20) );
    #print Dumper $match->();
    is_deeply(
        $match->(), {
          #'statements' => [
          #  {
              'list' => [
                {
                  'num' => '10'
                },
                {
                  'num' => '20'
                }
              ],
              'op1' => ';'
          #  }
          #]
        }, 
        'statements' );
}

{
    my $match = Pugs::Grammar::StatementControl->statement_list( q(10+10;) );
    #print Dumper $match->();
    is_deeply(
        $match->(), 
        { 
          #'statements' => [
          #  {
              'exp1' => {
                'num' => '10'
              },
              'exp2' => {
                'num' => '10'
              },
              'op1' => '+'
          #  }
          #]
        }, 
        'a simple statement 2' );
}

{
    my $match = Pugs::Grammar::StatementControl->parse( q({ 10 }) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        {             
          'bare_block' => {
            'num' => '10'
          }
        },
        'a bare block'
    );
}

{
    my $match = Pugs::Grammar::StatementControl->parse( q({}) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        {             
          'bare_block' => {
            #'statements' => [
            #  {}
            #]
          }
        },
        'an empty block'
    );
}

{
    my $match = Pugs::Grammar::StatementControl->parse( q( if 10 { 20 }) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
         {     
          'if' => {
            'block' => {
              #'statements' => [
              #  {
                  'num' => '20'
              #  }
              #]
            },
            'exp' => {
              'num' => '10'
            }
          }
        },
        'if'
    );
}

{
    my $match = Pugs::Grammar::StatementControl->parse( q(if 10 { 20; 30 }) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        {

          'if' => {
            'block' => {
              #'statements' => [
              #  {
                  'list' => [
                    {
                      'num' => '20'
                    },
                    {
                      'num' => '30'
                    }
                  ],
                  'op1' => ';'
              #  }
              #]
            },
            'exp' => {
              'num' => '10'
            }
          }

        },
        'if with 2-statement block'
    );
}

{
    my $match = Pugs::Grammar::StatementControl->parse( q(if $i { if $a { $b } }) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        {
          'if' => {
            'block' => {
              'if' => {
                'block' => {
                  'scalar' => '$b'
                },
                'exp' => {
                  'scalar' => '$a'
                }
              }
            },
            'exp' => {
              'scalar' => '$i'
            }
          }
        },
        'if inside if'
    );
}

{
    my $match = Pugs::Grammar::StatementControl->parse( q({{}}) );
    # print Dumper $match->();
    is_deeply(
        $match->(),
        {             
          'bare_block' => {
            'bare_block' => {}
          }
        },
        'a block with an empty block'
    );
}

{
    my $match = Pugs::Grammar::StatementControl->statement_list( q(
        if 11 {  
            if $a { 
                $b;
            }  
        }
     ) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        {
          'if' => {
            'block' => {
              'if' => {
                'block' => {
                  'scalar' => '$b'
                },
                'exp' => {
                  'scalar' => '$a'
                }
              }
            },
            'exp' => {
              'num' => '11'
            }
          }
        },
        'if inside if 2 with newlines'
    );
}

{
    my $match = Pugs::Grammar::StatementControl->statement_list( q(
        { 3 }  
        { 4 }
     ) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        [
          {
            'bare_block' => [
              {
                'num' => '3'
              }
            ]
          },
          {
            'bare_block' => [
              {
                'num' => '4'
              }
            ]
          }
        ],
        '2 blocks'
    );
}

{
    my $match = Pugs::Grammar::StatementControl->statement_list( q(
        { 3 }  7;
        { 4+5 }  8+9;
     ) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        [
          {
            'bare_block' => [
              {
                'num' => '3'
              }
            ]
          },
          {
            'num' => '7'
          },
          {
            'bare_block' => [
              {
                'exp1' => {
                  'num' => '4'
                },
                'exp2' => {
                  'num' => '5'
                },
                'op1' => '+'
              }
            ]
          },
          {
            'exp1' => {
              'num' => '8'
            },
            'exp2' => {
              'num' => '9'
            },
            'op1' => '+'
          }
        ],
        '2 blocks, 2 expressions'
    );
}
