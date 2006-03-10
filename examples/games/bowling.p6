#!/usr/bin/pugs
use v6;

my @bowlers;
my $team_handicap;

for (1..2) -> $bowler {
  say "Please enter Bowler $bowler's scores:";
  for (0..2) {
      print "Score $_:";
      my $score = =$*IN;
      @bowlers[$bowler]<average> += $score;
  }
  @bowlers[$bowler]<average> = int(@bowlers[$bowler]<average> / 3);
  @bowlers[$bowler]<handicap> = int((200 - @bowlers[$bowler]<average> ) * .85);
  $team_handicap += @bowlers[$bowler]<handicap>;
}
say;
for (1..2) -> $bowler {
  say "Bowler $bowler's Average Score:", @bowlers[$bowler]<average>,
      "\t Handicap: ", @bowlers[$bowler]<handicap>;
}
      
say;
say "The team's handicap $team_handicap";
