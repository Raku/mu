#!perl6
use v6;

say "*** Fetching from the Jerk It (tm) RSS feed...";
say "";

my $hdl = connect("www.phreeow.net", 80);
$hdl.say("GET /cgi-bin/jerkme.rss HTTP/1.0");
$hdl.say("Host: www.phreeow.net");
$hdl.say("");
$hdl.flush;

my $line = join('', =$hdl);
if ($line ~~ rx:perl5{<description>(.+)</description>\s*</item>}) {
    say $1;
}
else {
    say "Oops, connection failed."
}
