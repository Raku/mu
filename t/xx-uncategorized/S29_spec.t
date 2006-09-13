use v6-alpha;

use Test;

=kwid

Some failing tests from the top of S29.

=cut

plan 13;

my sub eval_ok1($code, $msg = "") { eval_ok($code,$code,$msg) }

eval_ok1('AnyChar.isa(Str)');
eval_ok1('Char.isa(Str)');
eval_ok1('LinguaChar =:= Ling');
eval_ok1('GraphemeChar =:= Graph');
eval_ok1('CodePoint =:= Uni');
eval_ok1('LinguaChar.isa(AnyChar)');
eval_ok1('GraphemeChar.isa(AnyChar)');
eval_ok1('CodePoint.isa(AnyChar)');
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
