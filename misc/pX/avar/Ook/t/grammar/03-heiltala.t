use Test;
use Ook::Grammar;

plan 8;

1 ~~ /<fjöldatala>/;
is($/<fjöldatala><tala>, 1);

#-1 ~~ /<fjöldatala>/;
#is($/<fjöldatala><tala>, -1);

1234 ~~ /<fjöldatala>/;
is($/<fjöldatala><tala>, 1234);

'-1234' ~~ /<fjöldatala>/;
is($/<fjöldatala><tala>, '-1234');

'$AB12' ~~ /<fjöldatala>/;
is($/<fjöldatala><hextala>, 'AB12');

'-$AB12' ~~ /<fjöldatala>/;
is($/<fjöldatala><hextala>, '-AB12');

'$ab12' ~~ /<fjöldatala>/;
is($/<fjöldatala><hextala>, 'ab12');

'-$ab12' ~~ /<fjöldatala>/;
is($/<fjöldatala><hextala>, '-ab12');
