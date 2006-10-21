use v6-alpha;
use Test;
plan 4;

#first lets test lexical named subs
{
    my String sub namedStr() { return 'string' };
    is namedStr(), 'string', 'named sub() return String';
}
is eval('namedStr()'), '', 'Correct : lexical named sub namedStr() should not be available here';

{
    my Int sub namedInt() { return 55 };
    is namedInt(), 55, 'named sub() return Int';
}
is eval('namedInt()'), '', 'Correct : lexical named sub namedInt() should not be available here';

