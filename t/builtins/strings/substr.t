use v6-alpha;

use Test;

plan 'skip_all';

#?pugs: todo('This test is not working as-is', 1);
{ 
# L<S09/"Junctions"/Each of the resulting set of calls is then recursively autothreaded>
# See also t/junctions/s09eg.t
# This test is not working as-is
#    is(eval('substr("camel", 0|1, 2&3)'), (("ca"|"am") & ("cam"|"ame")), "junctive substr");
}

