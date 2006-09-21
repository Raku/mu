use v6-alpha;
use Test;

# L<S29/"Type Declarations">

=kwid

Test for some type declarations for built-in functions. 

=cut

plan 11;

my sub eval_ok1($code, :$todo = 'feature') { &Test::eval_ok.goto($code,$code,:$todo) }

eval_ok1('AnyChar.isa(Str)');
eval_ok1('Char.isa(Str)');
eval_ok1('Codepoint =:= Uni');
eval_ok1('CharLingua.isa(AnyChar)');
eval_ok1('Grapheme.isa(AnyChar)');
eval_ok1('Codepoint.isa(AnyChar)');
eval_ok1('Byte.isa(AnyChar)');
eval_ok1('Byte.isa(Num)');
{
  eval_ok1('subset MatchTest of Item | Junction;');
}
{
  eval_ok1('use Math::Basic :constants; 3 < pi < 4;');
}
{
  eval_ok1('use Math::Basic; 3 < Math::Basic::pi < 4;');
}
