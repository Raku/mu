#!/usr/bin/pugs
use v6;

use Test;

plan 26;

# check the subroutine with the closest matching signature is called

sub earth (+$me)               {"me $me"};
sub earth (+$him)              {"him $him"};
sub earth (+$me, +$him)        {"me $me him $him"};
sub earth (+$me, +$him, +$her) {"me $me him $him her $her"};
sub earth ($me)                {"pos $me"};
sub earth ($me, +$you)         {"pos $me you $you"};
sub earth ($me, +$her)         {"pos $me her $her"};
sub earth ($me, $you)          {"pos $me pos $you"};
sub earth ($me, $you, +$her)   {"pos $me pos $you her $her"};

is( earth(me => 1),                     'me 1',             'named me', :todo<feature>);
is( earth(him => 2),                    'him 2',            'named you', :todo<feature>);
is( earth(me => 1, him => 2),           'me 1 him 2',       'named me, named him', :todo<feature>);
is( earth(him => 2, me => 1),           'me 1 him 2',       'named him, named me', :todo<feature>);
is( earth(me => 1, him => 2, her => 3), 'me 1 him 2 her 3', 'named me named him named her', :todo<feature>);
is( earth(him => 2, me => 1, her => 3), 'me 1 him 2 her 3', 'named him named me named her', :todo<feature>);
is( earth(her => 3, me => 1, him => 2), 'me 1 him 2 her 3', 'named her named me named him', :todo<feature>);
is( earth(her => 3, him => 2, me => 1), 'me 1 him 2 her 3', 'named her named him named me', :todo<feature>);

is( earth('a'),                'pos a',             'pos', :todo<feature>);
is( earth('b', you => 4),      'pos b you 4',       'pos, named you', :todo<feature>);
is( earth('c', her => 3),      'pos c her 3',       'pos, named her', :todo<feature>);
is( earth('d', 'e'),           'pos d pos e',       'pos, pos', :todo<feature>);
is( earth('f', 'g', her => 3), 'pos f pos g her 3', 'pos, pos, named');


# ensure we get the same results when the subroutines are 
# defined in reverse order
#

sub wind ($me, $you, +$her)   {"pos $me pos $you her $her"};
sub wind ($me, $you)          {"pos $me pos $you"};
sub wind ($me, +$her)         {"pos $me her $her"};
sub wind ($me, +$you)         {"pos $me you $you"};
sub wind ($me)                {"pos $me"};
sub wind (+$me, +$him, +$her) {"me $me him $him her $her"};
sub wind (+$me, +$him)        {"me $me him $him"};
sub wind (+$him)              {"him $him"};
sub wind (+$me)               {"me $me"};

is( wind(me => 1),                     'me 1',             'named me');
is( wind(him => 2),                    'him 2',            'named you', :todo<feature>);
is( wind(me => 1, him => 2),           'me 1 him 2',       'named me, named him');
is( wind(him => 2, me => 1),           'me 1 him 2',       'named him, named me');
is( wind(me => 1, him => 2, her => 3), 'me 1 him 2 her 3', 'named me named him named her');
is( wind(him => 2, me => 1, her => 3), 'me 1 him 2 her 3', 'named him named me named her');
is( wind(her => 3, me => 1, him => 2), 'me 1 him 2 her 3', 'named her named me named him');
is( wind(her => 3, him => 2, me => 1), 'me 1 him 2 her 3', 'named her named him named me');

is( wind('a'),                'pos a',             'pos', :todo<feature>);
is( wind('b', you => 4),      'pos b you 4',       'pos, named you', :todo<feature>);
is( wind('c', her => 3),      'pos c her 3',       'pos, named her', :todo<feature>);
is( wind('d', 'e'),           'pos d pos e',       'pos, pos', :todo<feature>);
is( wind('f', 'g', her => 3), 'pos f pos g her 3', 'pos, pos, named', :todo<feature>);

