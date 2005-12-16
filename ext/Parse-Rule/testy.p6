use lib::Parse::Rule::AST;
use lib::Parse::Rule::Match;

my $ast = 
    Union.new(
        left => Concat.new(
            left => Literal.new(string => 'a'),
            right => undef,
        ),
        right => Literal.new(string => ''),
    );

my $x = $ast.left;
$x.right = $ast;

my $mark = Mark.new(exp => $ast);

my $astprime = 
    Concat.new(
        left => Concat.new(
            left => $mark,
            right => Cut.new(
                mark => $mark,
            ),
        ),
        right => Literal.new(string => 'a'),
    );
                

my $match = 
    Match.new(
        backtrack => { die "Match failed" },
        input => "aaaaaaa",
        pos => 0,
    );

my $m = Parse::Rule::Match::match($astprime: $match, {$^x});
say $m.pos;
my $m2 = $m.backtrack()();
say $m2.pos;
