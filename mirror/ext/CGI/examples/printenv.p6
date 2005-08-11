#!/usr/bin/pugs

use v6;
require CGI-0.0.1;

print header();

say "<TABLE CELLSPACING='0' CELLPADDING='2' BORDER='1'>";
for (keys(%*ENV)) -> $key {
    my $val = %*ENV{"$key"};
    say "<TR><TD ALIGN='right'>" ~ $key ~ "</TD><TD>" ~ $val ~ "</TD></TR>";
}
say "</TABLE>";
