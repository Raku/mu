#!/usr/bin/pugs
use v6;

use Test;

plan 26;

# check the subroutine with the closest matching signature is called

multi earth (:$me)               {"me $me"};
multi earth (:$him)              {"him $him"};
multi earth (:$me, :$him)        {"me $me him $him"};
multi earth (:$me, :$him, :$her) {"me $me him $him her $her"};
multi earth ($me)                {"pos $me"};
multi earth ($me, :$you)         {"pos $me you $you"};
multi earth ($me, :$her)         {"pos $me her $her"};
multi earth ($me, $you)          {"pos $me pos $you"};
multi earth ($me, $you, :$her)   {"pos $me pos $you her $her"};

is( try { earth(me => 1) },                     'me 1',             'named me', :todo<feature>);
is( try { earth(him => 2) },                    'him 2',            'named you', :todo<feature>);
is( try { earth(me => 1, him => 2) },           'me 1 him 2',       'named me, named him', :todo<feature>);
is( try { earth(him => 2, me => 1) },           'me 1 him 2',       'named him, named me', :todo<feature>);
is( try { earth(me => 1, him => 2, her => 3) }, 'me 1 him 2 her 3', 'named me named him named her', :todo<feature>);
is( try { earth(him => 2, me => 1, her => 3) }, 'me 1 him 2 her 3', 'named him named me named her', :todo<feature>);
is( try { earth(her => 3, me => 1, him => 2) }, 'me 1 him 2 her 3', 'named her named me named him', :todo<feature>);
is( try { earth(her => 3, him => 2, me => 1) }, 'me 1 him 2 her 3', 'named her named him named me', :todo<feature>);

is( try { earth('a') },                'pos a',             'pos');
is( try { earth('b', you => 4) },      'pos b you 4',       'pos, named you', :todo<feature>);
is( try { earth('c', her => 3) },      'pos c her 3',       'pos, named her', :todo<feature>);
is( try { earth('d', 'e') },           'pos d pos e',       'pos, pos');
is( try { earth('f', 'g', her => 3) }, 'pos f pos g her 3', 'pos, pos, named');


# ensure we get the same results when the subroutines are 
# defined in reverse order
#

multi wind ($me, $you, :$her)   {"pos $me pos $you her $her"};
multi wind ($me, $you)          {"pos $me pos $you"};
multi wind ($me, :$her)         {"pos $me her $her"};
multi wind ($me, :$you)         {"pos $me you $you"};
multi wind ($me)                {"pos $me"};
multi wind (:$me, :$him, :$her) {"me $me him $him her $her"};
multi wind (:$me, :$him)        {"me $me him $him"};
multi wind (:$him)              {"him $him"};
multi wind (:$me)               {"me $me"};

is( try { wind(me => 1) },                     'me 1',             'named me', :todo<feature>);
is( try { wind(him => 2) },                    'him 2',            'named you', :todo<feature>);
is( try { wind(me => 1, him => 2) },           'me 1 him 2',       'named me, named him', :todo<feature>);
is( try { wind(him => 2, me => 1) },           'me 1 him 2',       'named him, named me', :todo<feature>);
is( try { wind(me => 1, him => 2, her => 3) }, 'me 1 him 2 her 3', 'named me named him named her', :todo<feature>);
is( try { wind(him => 2, me => 1, her => 3) }, 'me 1 him 2 her 3', 'named him named me named her', :todo<feature>);
is( try { wind(her => 3, me => 1, him => 2) }, 'me 1 him 2 her 3', 'named her named me named him', :todo<feature>);
is( try { wind(her => 3, him => 2, me => 1) }, 'me 1 him 2 her 3', 'named her named him named me', :todo<feature>);

is( try { wind('a') },                'pos a',             'pos');
is( try { wind('b', you => 4) },      'pos b you 4',       'pos, named you', :todo<feature>);
is( try { wind('c', her => 3) },      'pos c her 3',       'pos, named her', :todo<feature>);
is( try { wind('d', 'e') },           'pos d pos e',       'pos, pos');
is( try { wind('f', 'g', her => 3) }, 'pos f pos g her 3', 'pos, pos, named');

