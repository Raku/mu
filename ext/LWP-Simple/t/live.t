#!/usr/bin/pugs

use v6;
require Test;

my $tempfile = 'temp-ex-output';

# Change the URLs to something better/different
my @urls = <
    %s/
    %s
    %s/index.html
>;
@urls = (); # unTODOme

if (%ENV{"PUGS_TESTS_ALLOW_NETWORK"}) {
  push @urls, <
    http://www.corion.net
    http://datenzoo.de
    http://datenzoo.de/
    http://datenzoo.de/index.html
  >;
};
plan 2+@urls*8;

use_ok('LWP::Simple');

my $expected = "Hello from Pugs";

# Spawn a local proxy server
sub spawn_server (Int $port) {
  my $port = $port || 8086;

  diag "Spawning proxy on port $port";
  my $sock = listen($port);

  my $url = "http://localhost:$port";
  ok(defined $sock, "Listening on $url");

  async {
    #diag "Spawned server";

    while (1) {
      my $hdl = $sock.accept;
      
      my $request = =$hdl;
      #diag $request;
      if ($request ~~ rx:perl5/^GET \/stop-server\//) {
        last();
      };
      
      while (readline($hdl) ~~ rx:perl5/\S/) { 1 };
      $hdl.say("HTTP/1.0 200 OK");
      $hdl.say("Content-Type: text/plain; charset=UTF-8");
      $hdl.say("Server: Fake local Pugs HTTPd");
      $hdl.say("X-Original-Request: $request");
      $hdl.say("");
      $hdl.say($expected);
      $hdl.flush();
      $hdl.close;
    };

    #diag "Shut down server";
  };
  
  $url;
};

my $base_url = spawn_server( 8086 );

for @urls -> $t_url {
  my $url = $t_url;
  $url ~~ s:perl5/%s/$base_url/;

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

get("$base_url/stop");
