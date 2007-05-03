use Test;
use Ook::Grammar;

plan 7;

# Valid

'1.0' ~~ /<fleytitala>/;
is($/<fleytitala>, '1.0');

'1.E-10' ~~ /<fleytitala>/;
is($/<fleytitala>, '1.E-10');

'12.34' ~~ /<fleytitala>/;
is($/<fleytitala>, '12.34');

'-1.' ~~ /<fleytitala>/;
is($/<fleytitala>, '-1.');

'--12.34E123' ~~ /<fleytitala>/;
is($/<fleytitala>, '--12.34E123');

# Invalid

'.0' ~~ /<fleytitala>/;
is($/<fleytitala>, undef);

'1.2E' ~~ /<fleytitala>/;
is($/<fleytitala>, '1.2E');
