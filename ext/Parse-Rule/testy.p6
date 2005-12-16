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

my $match = 
    Match.new(
        backtrack => { die "Match failed" },
        input => "aaaaaab",
        pos => 0,
    );

my $m = Parse::Rule::Match::match($ast: $match, {$^x});
say $m.pos;
my $m2 = $m.backtrack()();
say $m2.pos;
