use v6-alpha;

use Test;

plan 11;


my $str = 'hello';


is $str.subst(/h/,'f'),       'fello', 'We can use subst';
is $str,                      'hello', '.. withouth side effect';
is $str.subst(rx:g:i/L/,'p'), 'heppo', '.. with multiple adverbs';

my $i=0;
is $str.subst(/l/,{$i++}),    'he0lo', '.. with a closure as replacement';
is $str.subst(rx:g/l/,{$i++}),'he12o', '.. which act like closure and can be called more then once';
is $str.=subst(/l/,'i'),      'heilo', '.. and with the .= modifier';
is $str,                      'heilo', '.. it changes the receiver';

# not sure about this. Maybe '$1$0' should work.

is 'a'.subst(/(.)/,"$1$0"), '',       '.. and it can not access captures from strings';
is 'a'.subst(/(.)/,{$0~$0}),'aa',     '.. you must wrap it in a closure';
is '12'.subst(/(.)(.)/,{$()*2}),'24', '.. and do nifty things in closures'; 

# XXX: UNSPECCED, but imo it should work with Strings instead of Regexen

dies_ok {'a'.subst('a','b'), 'b'}, '.. it fails with Str as Patterns';

