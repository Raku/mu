use v6-alpha;

use Test;

plan 7;

# L<S29/Str/=item comb>

# comb Str
is "".comb, (), 'comb on empty string';
is "a bc d".comb, <a bc d>, 'default matcher and limit';
is "a bc d".comb(:limit(2)), <a bc>, 'default matcher with supplied limit',
    :todo<feature>;

is "a ab bc ad ba".comb(m:Perl5/\ba\S*/), <a ab ad>,
    'match for any a* words';
is "a ab bc ad ba".comb(m:Perl5/\S*a\S*/), <a ab ad ba>,
    'match for any *a* words';
is eval('"a ab bc ad ba".comb(m:Perl5/\S*a\S*/, 2)'), <a ab>,
    'matcher and limit', :todo<feature>;

# comb a list
is eval('(<a ab>, <bc ad ba>).comb(m:Perl5/\S*a\S*/)'), <a ab ad ba>,
     'comb a list', :todo<feature>;

# needed: comb a filehandle

# needed: captures in pattern return Match objects
