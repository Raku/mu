use v6;

my @bowlers;
my $team_handicap;

for (1..2) -> $bowler {
  say "Please enter Bowler {$bowler}'s scores:";
  @bowlers[$bowler] = {average=>0, handicap => 0 };
  for (0..2) {
      print "Score $_:";
      my $score = $*IN.get;
      @bowlers[$bowler]<average> += $score;
  }
  @bowlers[$bowler]<average> = (@bowlers[$bowler]<average> / 3).Int;
  @bowlers[$bowler]<handicap> = ((200 - @bowlers[$bowler]<average> ) * .85).Int;
  $team_handicap += @bowlers[$bowler]<handicap>;
}
say;
for (1..2) -> $bowler {
  say "Bowler {$bowler}'s Average Score:", @bowlers[$bowler]<average>,
      "\t Handicap: ", @bowlers[$bowler]<handicap>;
}
      
say;
say "The team's handicap $team_handicap";
