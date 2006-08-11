use v6-alpha;

use Test;

=pod

Control block tests

Most tests are still TODO. 
These are based on L<S04/"Closure traits">

=cut

plan 11;

# don't read this linearly, some tests are not in the order they're written it
# (or at least, should not be ;-)

# TODO, based on synopsis 4:
#
# * KEEP, UNDO, PRE, POST, CONTROL
#   CATCH is tested in t/base/try.t
#
# * $var will undo, etc
#
# * blocks appearing multiple times
#
# * semantics of FIRST vs INIT, in terms of closures 
#
# * LEAVE type blocks in the context of CATCH
#
# * PRE/POST in classes is not the same as LEAVE/ENTER

my (@first, @enter, @leave, @last, @next) = ();

for (1 .. 3) -> $i {
    # FIXME: these don't parse yet 
    LAST  { push @last, $i }
    LEAVE { push @leve, [ $i, +@enter ] }
    ENTER { push @enter, [ $i, +@leave ] }
    FIRST { push @first, $i }
    NEXT { push @next, $i }
    next if $i % 2 == 1;
}

# L<S04/"Closure traits" /FIRST/>
is(+@first, 1, "FIRST ran once");
is(@first[0], 1, "only on 1");

# L<S04/"Closure traits" /LAST/>
is(+@last, 1, "LAST ran once", :todo);
is(@last[0], 1, "only on 3", :todo);

# L<S04/"Closure traits" /ENTER/>
is(+@enter, 3, "ENTER ran thrice", :todo);

# L<S04/"Closure traits" /LEAVE/>
is(+@leave, 3, "ENTER ran thrice", :todo);

is(@enter[0][1], 0, "enter and leave are in proper order", :todo);
is(@enter[2][1], 2, "...", :todo);
is(@leave[0][1], 1, "...", :todo);
is(@leave[2][1], 3, "...", :todo);

# L<S04/"Closure traits" /NEXT/>
is(+@next, 2, "NEXT ran twice, for each odd number in loop", :todo);

