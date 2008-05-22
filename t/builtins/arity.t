use v6;

use Test;

plan 17;

# L<S06/Required parameters/method:>
{
    my sub foo () {}
    is &foo.arity, 0, '0 arity &foo';
}

{
    my sub foo ($a) {}
    is &foo.arity, 1, '1 arity &foo';
}

{
    my sub foo ($a, $b) {}
    is &foo.arity, 2, '2 arity &foo';
}

{
    my sub foo ($a, $b, @c) {}
    is &foo.arity, 3, '3 arity &foo';
}

{
    my sub foo ($a, $b, @c, %d) {}
    is &foo.arity, 4, '4 arity &foo';
}

# It's not really specced in what way (*@slurpy_params) should influence
# .arity. Also it's unclear what the result of &multisub.arity is.
# See the thread "&multisub.arity?" on p6l started by Ingo Blechschmidt for
# details:
# L<http://thread.gmane.org/gmane.comp.lang.perl.perl6.language/4915>

{
    is ({ $^a         }.arity), 1,
        "block with one placeholder var has .arity == 1";
    is (-> $a { $a         }.arity), 1,
        "pointy block with one placeholder var has .arity == 1";
    is arity({ $^a,$^b     }:), 2,
        "block with two placeholder vars has .arity == 2";
    is arity(-> $a, $b { $a,$b     }:), 2,
        "pointy block with two placeholder vars has .arity == 2";
    is arity({ $^a,$^b,$^c }:), 3,
        "block with three placeholder vars has .arity == 3";
    is arity(-> $a, $b, $c { $a,$b,$c }:), 3,
        "pointy block with three placeholder vars has .arity == 3";
}

{
    is arity({ my $k; $^a         }:), 1,
        "additional my() vars don't influence .arity calculation (1-1)";
    is arity({ my $k; $^a,$^b     }:), 2,
        "additional my() vars don't influence .arity calculation (1-2)";
    is arity({ my $k; $^a,$^b,$^c }:), 3,
        "additional my() vars don't influence .arity calculation (1-3)";
}

{
    is arity({ $^a;         my $k }:), 1,
        "additional my() vars don't influence .arity calculation (2-1)";
    is arity({ $^a,$^b;     my $k }:), 2,
        "additional my() vars don't influence .arity calculation (2-2)";
    is arity({ $^a,$^b,$^c; my $k }:), 3,
        "additional my() vars don't influence .arity calculation (2-3)";
}
