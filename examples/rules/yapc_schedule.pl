grammar yapc_schedule;

rule schedule { <title> [ <talk> ]+ }

token title { \<title\> <speaker> \</title\> }

regex ws { .*? }

token talk { \<small\> <speaker> \</small\> }

token speaker { [\w|\s]+ }

### extract.pl
use v6-alpha;

module Main;

my $content = slurp @*ARGS[0];

my $match = $content ~~ m/<schedule>/;

say $match.perl;

