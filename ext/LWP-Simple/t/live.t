!/usr/bin/pugs

use v6;
require Test;

my $tempfile = 'temp-ex-output';

# Change the URLs to something better/different
my @urls = <
  http://www.corion.net
  http://datenzoo.de
  http://datenzoo.de/
  http://datenzoo.de/index.html
>;
plan 1+@urls*8;

# require LWP::Simple;

use_ok('LWP::Simple');
for @urls -> $url {
  diag "Getting HEAD of $url";
  my $head = head($url);
  ok( $head ~~ rx:perl5/.../, "Got some headers as scalar");
  my @head = head($url);
  ok( @head > 3, "Got more than 1 line as list");
  my %head = head($url);
  ok( %head.keys() > 0, "Got some headers as hash");
  is( %head{'Content-Type'}, "text/html", "Got a content type of text/html");

  diag "Retrieving $url";
  my $res = get($url);
  ok(defined $res, "Got some result");
  ok( defined ($res ~~ rx:perl5/./), "and it's not empty");

  diag "Storing $url to $tempfile";
  my $f = getstore($url,$tempfile);
  my $buf = slurp $tempfile;
  is( $buf, $res, "... and contains $url");
  is( $f, $res, "... and getstore() returns $url");
};
