use v6-alpha;

use Test;

plan 7;


my $str = 'hello';


is $str.subst(/h/,'f'),       'fello', 'We can use subst';
is $str,                      'hello', '.. withouth side effect';
is $str.subst(rx:g:i/L/,'p'), 'heppo', '.. with multiple adverbs';

my $i=0;
is $str.subst(/l/,{$i++}),    'he0lo', '.. with a closure as replacement';
is $str.subst(rx:g/l/,{$i++}),'he12o', '.. which act like closure and can be called more then once';
is $str.=subst(/l/,'i'),      'heilo', '.. and with the .= modifier';
is $str,                      'heilo', '.. it changes the receiver';


