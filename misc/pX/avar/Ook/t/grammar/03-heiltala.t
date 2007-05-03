use Test;
use Ook::Grammar;

plan 16;

1 ~~ /<heiltala>/;
is($/<heiltala><sigil>, undef);
is($/<heiltala><fjöldatala><tala>, 1);

'-1' ~~ /<heiltala>/;
is($/<heiltala><sigil>, '-');
is($/<heiltala><fjöldatala><tala>, '1');

1234 ~~ /<heiltala>/;
is($/<heiltala><sigil>, undef);
is($/<heiltala><fjöldatala><tala>, 1234);

'-1234' ~~ /<heiltala>/;
is($/<heiltala><sigil>, '-');
is($/<heiltala><fjöldatala><tala>, '1234');

'$AB12' ~~ /<heiltala>/;
is($/<heiltala><sigil>, undef);
is($/<heiltala><fjöldatala><hextala>, 'AB12');

'-$AB12' ~~ /<heiltala>/;
is($/<heiltala><sigil>, '-');
is($/<heiltala><fjöldatala><hextala>, 'AB12');

'$ab12' ~~ /<heiltala>/;
is($/<heiltala><sigil>, undef);
is($/<heiltala><fjöldatala><hextala>, 'ab12');

'-$ab12' ~~ /<heiltala>/;
is($/<heiltala><sigil>, '-');
is($/<heiltala><fjöldatala><hextala>, 'ab12');
