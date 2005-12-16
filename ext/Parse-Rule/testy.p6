use lib::Parse::Rule::AST;
use lib::Parse::Rule::Match;

my $ast = 
    Union.new(
        either => Concat.new(
            first => Literal.new(string => 'a'),
            then  => undef,
        ),
        or     => Literal.new(string => ''),
    );

my $x = $ast.either;
$x.then = $ast;

my $mark = Mark.new(exp => $ast);

my $astprime = 
    Concat.new(
        first => Concat.new(
            first => $mark,
            then => Cut.new(
                mark => $mark,
            ),
        ),
        then => Literal.new(string => 'a'),
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
