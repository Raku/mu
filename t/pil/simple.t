#!./pugs

use v6;
use Test::Pil;

pil_is_eq(q:to/CODE/
[]`push(3, 4, 5)
CODE,
q:to/RESULT/
[3, 4, 5]
RESULT);


pil_is_eq(q:to/CODE/
(-> $x {-> $y {$x`add($y)}})`(3)`(4)
CODE,
q:to/RESULT/
7
RESULT);

pil_is_eq(q:to/CODE/
(-> $n {-> &f { $n`eq(0)`if_else( ->{1}, ->{$n`multiply(&f`($n`subtract(1)))}) }`(&?SUB)})`(10)
CODE,
q:to/RESULT/
3628800
RESULT, "Audrey's Factorial");

pil_is_eq(q:to/CODE/
(-> $n {
    (-> &fact {
        &fact`(&fact, $n)
    })`(-> &f, $x {
            $x`eq(0)`if_else(
                -> { 1 },
                -> { $x`multiply( &f`(&f, $x`subtract(1)) ) }
            )
       });
})`(10);
CODE,
q:to/RESULT/
3628800
RESULT, "Stevan's Factorial" );

pil_is_eq(q:to/CODE/
1`add(1);
2`add(2)
CODE,
q:to/RESULT/
4
RESULT);

