
use v6;

role Math::Trig;

our @trig = q:x:/ sin cos tan asin acos atan sec cosec cotan asec
                  acosec acotan sinh cosh tanh asinh acosh atanh sech
                  cosech cotanh asech acosech acotanh /;

for @trig -> $func {
    &$func = multi sub (: Num ?$x = $CALLER::_, +$base)
            returns Num { ... };
}

multi sub atan (Num $x, Num $y : Num +$base) returns Num
    { ... };

multi sub pi is constant () returns Num
    { 3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37510_58209_74944_5923 };

1;
