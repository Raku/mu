use v6;
require Test;

plan(7);

ok ('a' x 3 eq 'aaa');
ok ('ab' x 4 eq 'abababab');
ok (1 x 5 eq '11111');
ok ('' x 6 eq '');

my @foo = 'x' xx 10;
ok (@foo[0] eq 'x');
ok (@foo[9] eq 'x');
ok (+@foo == 10);
