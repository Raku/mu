use lib 
    '../Pugs-Compiler-Rule/lib',
    '../Pugs-Compiler-Precedence/lib',
;

use Pugs::Grammar::Perl6;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

use Test::More 'no_plan';

{
    my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
    if 1 {
        10 + $a / "abc"
    }
    1;
PERL6
    #print Dumper $match->();
    is_deeply( $match->(), 
        [
          {
            'if' => {
              'block' => [
                {
                  'exp1' => {
                    'num' => '10'
                  },
                  'exp2' => {
                    'exp1' => {
                      'scalar' => '$a'
                    },
                    'exp2' => {
                      'double_quoted' => '"abc"'
                    },
                    'op1' => '/'
                  },
                  'op1' => '+'
                }
              ],
              'exp' => {
                'num' => '1'
              }
            }
          },
          {
            'num' => '1'
          }
        ],
        'statement: if 1 {10 + $a / "abc"} '
    );
}

{
    #print $Pugs::Grammar::StatementControl::hash{sub}->perl5;
    my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
    sub prefix:<xx> { $a }
PERL6
    #print Dumper $match->();
    is_deeply( $match->(), 
        [
          {
            'sub' => {
              'block' => [
                {
                  'scalar' => '$a'
                }
              ],
              'name' => {
                'pair' => {
                  'key' => {
                    'single_quoted' => 'prefix'
                  },
                  'value' => {
                    'single_quoted' => 'xx'
                  }
                }
              }
            }
          }
        ],
        'statement: sub declaration '
    );
}

{
    #print $Pugs::Grammar::StatementControl::hash{sub}->perl5;
    my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
    xx( $a )
PERL6
    #print Dumper $match->();
    is_deeply( $match->(), 
        [
          {
            'exp1' => {
              'scalar' => '$a'
            },
            'op1' => {
              'name' => {
                'single_quoted' => 'xx'
              },
              'op' => 'CALL'
            },
            'op2' => 'CLOSE_PAREN'
          }
        ],
        'statement: sub call '
    );
}

{
    #print $Pugs::Grammar::StatementControl::hash{sub}->perl5;
    my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
    xx()
PERL6
    #print Dumper $match->();
    is_deeply( $match->(), 
        [
          {
            'op1' => {
              'name' => {
                'single_quoted' => 'xx'
              },
              'op' => 'CALL'
            },
            'op2' => 'CLOSE_PAREN'
          }
        ],
        'statement: sub call, no param '
    );
}

{
    #print $Pugs::Grammar::StatementControl::hash{sub}->perl5;
    my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
    say $a;
PERL6
    #print Dumper $match->();
    is_deeply( $match->(), 
        [
          {
            'exp1' => {
              'scalar' => '$a'
            },
            'op1' => {
              'name' => {
                'single_quoted' => 'say'
              },
              'op' => 'CALL_NO_PAREN'
            }
          }
        ],
        'statement: sub call, no parenthesis '
    );
}

{
    #print $Pugs::Grammar::StatementControl::hash{sub}->perl5;
    my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
    say $a, $b;
PERL6
    #print Dumper $match->();
    is_deeply( $match->(), 
        [
          {
            'exp1' => {
              'list' => [
                {
                  'scalar' => '$a'
                },
                {
                  'scalar' => '$b'
                }
              ],
              'op1' => ','
            },
            'op1' => {
              'name' => {
                'single_quoted' => 'say'
              },
              'op' => 'CALL_NO_PAREN'
            }
          }
        ],
        'statement: sub call, no parenthesis, commas '
    );
}

{
    #print $Pugs::Grammar::StatementControl::hash{sub}->perl5;
    my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
#comment
    $a
PERL6
    #print Dumper $match->();
    is_deeply( $match->(), 
        [
          {
            'scalar' => '$a'
          }
        ],
        'statement: comment '
    );
}

__END__

# fails!
my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
if 1 {
    10 + $a / "abc"
}
"expression";
PERL6
use YAML;
print Dump $match->();
