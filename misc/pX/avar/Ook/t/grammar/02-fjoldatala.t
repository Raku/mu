use Test;
use Ook::Grammar;

plan 4;

1 ~~ /<fjöldatala>/;
is($/<fjöldatala><tala>, 1);

1234 ~~ /<fjöldatala>/;
is($/<fjöldatala><tala>, 1234);

'$AB12' ~~ /<fjöldatala>/;
is($/<fjöldatala><hextala>, 'AB12');

'$ab12' ~~ /<fjöldatala>/;
is($/<fjöldatala><hextala>, 'ab12');
