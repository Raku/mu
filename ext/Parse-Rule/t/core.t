use Test;
use Parse::Rule::Core;
use Parse::Rule::Media;

plan 8;

my $match = Match.new;
is_deeply($match.match_num,  [], 'match_num gets initialized');
is_deeply($match.match_name, {}, 'match_name gets initialized');

$match = Match.new(match_num => [1,2], match_name => { a => 3 });
is_deeply($match.match_num, [1,2], 'match_num can be overridden');
is_deeply($match.match_name, {a => 3}, 'match_name can be overridden');

my $text = "foo bar";
my $oo_start = Text.new(pos => 1, text => $text);
my $oo_end   = Text.new(pos => 3, text => $text);
my $oo = Match.new(start => $oo_start, end => $oo_end);
my $match_oo = Match.new(
    start => $oo_start,
    end   => $oo_end,
    match_num => [ $oo ],
    match_name => { mat => $oo },
);

my $ba_start = Text.new(pos => 4, text => $text);
my $ba_end = Text.new(pos => 6, text => $text);
my $ba = Match.new(start => $ba_start, end => $ba_end);
my $match_ba = Match.new(
    start => $ba_start,
    end   => $ba_end,
    match_num => [ $ba ],
    match_name => { mat => $ba },
);

$match = Match::combine([ $match_oo, $match_ba ], $oo_start);
ok($match.match_num[0][0] === $oo, 'combine match_num');
ok($match.match_num[0][1] === $ba, 'combine match_num');
ok($match.match_name<mat>[0] === $oo, 'combine match_num');
ok($match.match_name<mat>[1] === $ba, 'combine match_num');
    

# vim: ft=perl6 :
