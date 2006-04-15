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
