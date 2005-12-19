#!/usr/bin/pugs

use v6;
use Test::Pil;

pil_is_eq(q:to/CODE/
use Bootstrap.pil
CODE,
q:to/RESULT/
RESULT);


pil_is_eq(q:to/CODE/
loop
{
  
}
CODE,
q:to/RESULT/
RESULT);

pil_is_eq(q:to/CODE/
( -> $x { $x.sub(1) } ).do_for([3, 4, 5, 6])
CODE,
q:to/RESULT/
[2, 3, 4, 5]
RESULT);

pil_is_eq(q:to/CODE/
$x := 10;
do
{
  $x;
  $x := $x.sub(1);
}
while $x
CODE,
q:to/RESULT/
10 9 8 7 6 5 4 3 2 1
RESULT);
