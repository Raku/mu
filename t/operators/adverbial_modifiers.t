#!/usr/bin/pugs

use v6;
use Test;

plan 63;

eval_is 'infix:<..>(1, 10, by => 2)', <1 3 5 7 9>, 'range operator, :by parameter, long name', :todo<feature>;
eval_is '1..10 :by(2)', <1 3 5 7 9>, 'range operator, :by adverb, space', :todo<feature>;
eval_is '1..10:by(2)', <1 3 5 7 9>, 'range operator, :by adverb, without space', :todo<feature>;

eval_is 'infix:<...>(1, by => 2)[0..4]', <1 3 5 7 9>, 'infinite range operator, long name', :todo<feature>;
eval_is '1... :by(2)[0..4]', <1 3 5 7 9>, 'infinite range operator, :by adverb, space', :todo<feature>;
eval_is '1...:by(2)[0..4]', <1 3 5 7 9>, 'infinite range operator, :by adverb, without space', :todo<feature>;

# XXX need to test prefix:<=> on $handle with :prompt adverb

sub prefix:<blub> (Str $foo, Int +$times = 1) {
	("BLUB" x $times) ~ $foo;
}

is prefix:<blub>("bar"), 'BLUBbar', 'user-defined prefix operator, long name';
is prefix:<blub>("bar", times => 2), 'BLUBBLUBbar', 'user-defined prefix operator, long name, optional parameter';
is prefix:<blub>(:times(2) "bar"), 'BLUBBLUBbar', 'user-defined prefix operator, long name, :times adverb, leading';
is prefix:<blub>("bar" :times(2)), 'BLUBBLUBbar', 'user-defined prefix operator, long name, :times adverb, trailing';
is eval('blub "bar"'), 'BLUBbar', 'user-defined prefix operator, basic call', :todo<feature>;
is eval('blub "bar" :times(2)'), 'BLUBBLUBbar', 'user-defined prefix operator, :times adverb, space', :todo<feature>;
is eval('blub "bar":times(2)'), 'BLUBBLUBbar', 'user-defined prefix operator, :times adverb, no space', :todo<feature>;

{
  # These basic adverb tests are copied from a table in A12.
  my $bar = 123;
  my @many = (4,5);
  sub dostuff(){"stuff"}
  my($v,$e);
  $e = (foo => $bar);
  $v = :foo($bar);
  is \$v, \$e, ':foo($bar)';

  $e = (foo => [1,2,3,@many]);
  $v = :foo[1,2,3,@many];
  is \$v, \$e, ':foo[1,2,3,@many]';

  $e = (foo => «alice bob charles»);
  $v = :foo«alice bob charles»;
  is \$v, \$e, ':foo«alice bob charles»';

  $e = (foo => 'alice');
  $v = :foo«alice»;
  is \$v, \$e, ':foo«alice»';

  fail("FIXME parsefail", :todo<bug>);
  #$e = (foo => { a => 1, b => 2 });
  $v = eval ':foo{ a => 1, b => 2 }';
  #is \$v, \$e, ':foo{ a => 1, b => 2 }', :todo;

  fail("FIXME parsefail", :todo<bug>);
  #$e = (foo => { dostuff() });
  $v = eval ':foo{ dostuff() }';
  #is \$v, \$e, ':foo{ dostuff() }', :todo;

  $e = (foo => 0);
  $v = :foo(0);
  is \$v, \$e, ':foo(0)';

  $e = (foo => 1);
  $v = :foo;
  is \$v, \$e, ':foo';
}

{
  # Exercise various mixes of "f", parens "()",
  # and adverbs with "X' and without "x" an argument.

  sub f(+$x,+$y){$x~$y}
  my $v;

  # f(XY) f(YX) f(xY) f(Xy)

  $v = f(:x("a"):y("b"));
  is $v, "ab", 'f(:x("a"):y("b"))';

  $v = f(:y("b"):x("a"));
  is $v, "ab", 'f(:y("b"):x("a"))';

  $v = f(:x:y("b"));
  is $v, "1b", 'f(:x:y("b"))';

  $v = f(:x("a"):y);
  is $v, "a1", 'f(:x("a"):y)';

  # fXY() fxY() fXy()

  $v = f:x("a"):y("b")();
  is $v, "ab", 'f:x("a"):y("b")()';

  $v = f:x:y("b")();
  is $v, "1b", 'f:x:y("b")()';

  $v = f:x("a"):y ();
  is $v, "a1", 'f:x("a"):y ()';

  # fX(Y) fY(X) fx(Y) fX(y)

  $v = f:x("a")(:y("b"));
  is $v, "ab", 'f:x("a")(:y("b"))';

  $v = f:y("b")(:x("a"));
  is $v, "ab", 'f:y("b")(:x("a"))';

  $v = f:x (:y("b"));
  is $v, "1b", 'f:x (:y("b"))';

  $v = f:x("a")(:y);
  is $v, "a1", 'f:x("a")(:y)';

  # fXY fxY fXy

  $v = f:x("a"):y("b");
  is $v, "ab", 'f:x("a"):y("b")';

  $v = f:x:y("b");
  is $v, "1b", 'f:x:y("b")';

  $v = f:x("a"):y;
  is $v, "a1", 'f:x("a"):y';

  # f(X)Y f(Y)X f(x)Y f(X)y f(x)y

  $v = f(:x("a")):y("b");
  is $v, "ab", 'f(:x("a")):y("b")';

  $v = f(:y("b")):x("a");
  is $v, "ab", 'f(:y("b")):x("a")';

  $v = f(:x):y("b");
  is $v, "1b", 'f(:x("a")):y("b")';

  $v = f(:x("a")):y;
  is $v, "a1", 'f(:x("a")):y';

  $v = f(:x):y;
  is $v, "11", 'f(:x):y';

  # f()XY f()YX f()xY f()Xy  f()xy

  $v = f():x("a"):y("b");
  is $v, "ab", 'f():x("a"):y("b")';

  $v = f():y("b"):x("a");
  is $v, "ab", 'f():y("b"):x("a")';

  $v = f():x:y("b");
  is $v, "1b", 'f():x:y("b")';

  $v = f():x("a"):y;
  is $v, "a1", 'f():x("a"):y';

  $v = f():x:y;
  is $v, "11", 'f():x:y';

  # fX()Y fY()X fx()y

  $v = f:x("a")():y("b");
  is $v, "ab", 'f:x("a")():y("b")';

  $v = f:y("b")():x("a");
  is $v, "ab", 'f:y("b")():x("a")';

  $v = f:x ():y;
  is $v, "11", 'f:x ():y';

}

{
  # Exercise mixes of adverbs and positional arguments.

  my $v;
  sub f(+$x,$s){$x~$s}
  sub g($s1,+$x,$s2){$s1~$x~$s2}
  sub h(*@a){@a.perl}
  sub i(*%h){%h.perl}
  sub j($s1,*%h,$s2){$s1~%h.perl~$s2}

  # f(X s) f(Xs) f(s X) f(sX) f(xs) f(sx)

  $v = f(:x("a") "b");
  is $v, "ab", 'f(:x("a") "b")';

  $v = f(:x("a")"b");
  is $v, "ab", 'f(:x("a")"b")';

  $v = f("b" :x("a"));
  is $v, "ab", 'f("b" :x("a"))';

  $v = f("b":x("a"));
  is $v, "ab", 'f("b":x("a"))';

  $v = f(:x "b");
  is $v, "1b", 'f(:x "b")';

  $v = f("b" :x);
  is $v, "1b", 'f("b" :x)';

  # fX(s) f(s)X

  $v = f:x("a")("b");
  is $v, "ab", 'f:x("a")("b")';

  $v = f("b"):x("a");
  is $v, "ab", 'f("b"):x("a")';

  # fX s  fXs  fx s

  $v = "eval failed";
  eval '$v = f:x("a") "b"';
  is $v, "ab", 'f:x("a") "b"', :todo<bug>;

  $v = "eval failed";
  eval '$v = f:x("a")"b"';
  is $v, "ab", 'f:x("a")"b"', :todo<bug>;

  $v = "eval failed";
  eval '$v = f:x "b"';
  is $v, "ab", 'f:x "b"', :todo<bug>;

  # fs X  fsX  fs x  fsx

  $v = f "b" :x("a");
  is $v, "ab", 'f "b" :x("a")';

  $v = f "b":x("a");
  is $v, "ab", 'f "b":x("a")';

  $v = f "b" :x;
  is $v, "1b", 'f "b" :x';

  $v = f "b":x;
  is $v, "1b", 'f "b":x';

  # add more tests...

}
