use lib 
    '../Pugs-Compiler-Rule/lib',
    '../Pugs-Compiler-Tokenizer/lib',
    '../Pugs-Compiler-Precedence/lib',
;

use Test::More tests => 2;

use Pugs::Compiler::Rule;
use Pugs::Grammar::Precedence;
use Pugs::Grammar::Term;
use Pugs::Grammar::Operator;

use Pugs::Grammar::Expression;
use Pugs::Grammar::Perl6;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

{
    my $match = Pugs::Grammar::Perl6->parse( q(10;) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        { 
          int => 10,
          pos => 2, 
        },
        'term 1'
    );
}

TODO:
{
    local $TODO = "expression without trailing ';'";
    my $match;
    eval {
        $match = Pugs::Grammar::Perl6->parse( q(10) );
    };
    #print Dumper $match->();
    is_deeply(
        $match->(),
        { 
          int => 10,
          pos => 2, 
        },
        'term 1'
    );
}

__END__

# TODO - update

{
    my $match = Pugs::Grammar::Expression->ast( q(10+20) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        { 
          'exp1' => {
            'num' => '10'
          },
          'exp2' => {
            'num' => '20'
          },
          'op1' => '+'
        },
        'term 2'
    );
}

{
    my $match = Pugs::Grammar::Expression->ast( q(10,20) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        { 
          'list' => [
            {
              'num' => '10'
            },
            {
              'num' => '20'
            }
          ],
          'op1' => ','
        },
        'list'
    );
}

{
    my $match = Pugs::Grammar::Expression->ast( q(10 + $a / "abc") );
    #print Dumper $match->();
    
    is_deeply(
        $match->(),
    
        {
      'exp1' => {
        'num' => '10'
      },
      'exp2' => {
        'exp1' => {
          'scalar' => '$a'
        },
        'exp2' => {
          'double_quoted' => 'abc'
        },
        'op1' => '/'
      },
          'op1' => '+'
        },
    
        'expression q(10 + $a / "abc")'
    );
}

{
    my $match = Pugs::Grammar::Expression->ast( q(1,2,3) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        {
          'list' => [
            {
              'num' => '1'
            },
            {
              'num' => '2'
            },
            {
              'num' => '3'
            }
          ],
          'op1' => ','
        },
        'list expression'
    );
}

{
    my $match = Pugs::Grammar::Expression->ast( q(1,2 Y 3) );
    #print Dumper $match->();
    is_deeply(
        $match->(),
        {
          'list' => [
            {
              'list' => [
                {
                  'num' => '1'
                },
                {
                  'num' => '2'
                }
              ],
              'op1' => ','
            },
            {
              'num' => '3'
            }
          ],
          'op1' => 'Y'
        },
        'list+list expression'
    );
}
