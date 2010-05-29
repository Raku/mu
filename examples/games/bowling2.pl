use v6;

print "How many bowlers are on your team?";
my $bowlers = $*IN.get;
print "How many games would you like to average?";
my $games   = $*IN.get;

my @bowlers;

for (1 .. $bowlers) {
    my $bowler;
    $bowler = { id => 0, total=>0, avg=>0, handicap=>0,games => [] };
    say "Enter scores for bowler $_:";
    for (1.. $games) {
      print "game $_ score:";
      my $score = $*IN.get;
      $bowler<games>.push( $score );      
    }
    $bowler<id>       = $_;
    $bowler<total>    = [+] @($bowler<games>);  
    $bowler<avg>      = $bowler<total> / $games;
    $bowler<handicap> = ( (200 - $bowler<avg>) * .8).Int;
    @bowlers.push($bowler);
}

for @bowlers -> $bowler {
  say "Bowler $bowler<id>'s average is $bowler<avg>\tHandicap: $bowler<handicap>"; 
}

my $team_handicap = [+] @bowlers.map: { $_.<handicap> };
say "Team handicap is: $team_handicap";

