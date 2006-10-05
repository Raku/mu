use v6-alpha;
use Test;

# L<S29/"Type Declarations">

=kwid

Test for some type declarations for built-in functions. 

=cut

plan 11;

my sub ok_eval1($code, :$todo = 'feature') { &Test::ok.goto(eval($code),$code,:$todo) }

ok_eval1('AnyChar.isa(Str)');
ok_eval1('Char.isa(Str)');
ok_eval1('Codepoint =:= Uni');
ok_eval1('CharLingua.isa(AnyChar)');
ok_eval1('Grapheme.isa(AnyChar)');
ok_eval1('Codepoint.isa(AnyChar)');
ok_eval1('Byte.isa(AnyChar)');
ok_eval1('Byte.isa(Num)');
{
  ok_eval1('subset MatchTest of Item | Junction;');
}
{
  ok_eval1('use Math::Basic :constants; 3 < pi < 4;');
}
{
  ok_eval1('use Math::Basic; 3 < Math::Basic::pi < 4;');
}
