#!perl6
use v6;

say "*** Fetching from the Jerk It (tm) RSS feed...";

my $hdl = connect("www.phreeow.net", 80);
$hdl.say(
    "GET /cgi-bin/jerkme.rss HTTP/1.0\n",
    "Host: www.phreeow.net\n"
);
$hdl.flush;

if ($hdl.slurp ~~ rx:perl5{<description>(.+)</description>\s*</item>}) {
    say $1;
}
else {
    say "*** Oops, connection failed."
}
