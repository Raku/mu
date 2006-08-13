use v6-alpha;

use Test;

plan 2;

# TODO, based on synopsis 4:
#
# * KEEP, UNDO, PRE, POST, CONTROL
#   CATCH is tested in t/base/try.t
#
# * $var will undo, etc
#
# * LEAVE type blocks in the context of CATCH
#
# * PRE/POST in classes is not the same as LEAVE/ENTER

# L<S04/"Closure traits">

{
    my $str;

    for 1..10 -> $i {
        last if $i > 3;
        $str ~= "($i a)";
        next if $i % 2 == 1;
        $str ~= "($i b)";
        LAST  { $str ~= "($i Last)" }
        LEAVE { $str ~= "($i Leave)" }
        NEXT  { $str ~= "($i N)" }
        FIRST { $str ~= "($i F)" }
        ENTER { $str ~= "($i E)" }
    }

    is $str, "(1 F)(1 E)(1 a)" ~ "(1 N)(1 Leave)" ~
                  "(2 E)(2 a)(2 b)(2 N)(2 Leave)" ~
                  "(3 E)(3 a)" ~ "(3 N)(3 Leave)" ~
                  "(4 E)"  ~  "(4 Last)(4 Leave)",
       'trait blocks work properly in for loop';
}

{
    my $str;

    for 1..10 -> $i {
        last if $i > 3;
        $str ~= "($i a)";

        ENTER { $str ~= "($i E1)" }
        LAST  { $str ~= "($i Last1)" }
        FIRST { $str ~= "($i F1)" }
        LEAVE { $str ~= "($i Leave1)" }

        next if $i % 2 == 1;
        $str ~= "($i b)";

        LAST  { $str ~= "($i Last2)" }
        NEXT  { $str ~= "($i N1)" }
        FIRST { $str ~= "($i F2)" }
        LEAVE { $str ~= "($i Leave2)" }
        ENTER { $str ~= "($i E2)" }
        NEXT  { $str ~= "($i N2)" }
    }

    is $str, "(1 F1)(1 F2)(1 E1)(1 E2)(1 a)" ~ "(1 N1)(1 N2)" ~  "(1 Leave1)(1 Leave2)" ~
                         "(2 E1)(2 E2)(2 a)(2 b)(2 N1)(2 N2)" ~  "(2 Leave1)(2 Leave2)" ~
                         "(3 E1)(3 E2)(3 a)" ~ "(3 N1)(3 N2)" ~  "(3 Leave1)(3 Leave2)" ~
                         "(4 E1)(4 E2)"  ~     "(4 Last1)(4 Last2)(4 Leave1)(4 Leave2)",
       'trait blocks work properly in for loop';
}
