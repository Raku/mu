#!/usr/bin/pugs

use v6;
require Test;

my $tempfile = "temp-pugs-download";

# Change the URLs to something better/different
my @urls = <
    %s/
>;

plan 2+@urls;

use_ok('LWP::Simple');

my $expected = "Hello from Pugs";

# Spawn a local proxy server
sub spawn_server (Int $port) {
  my $port = $port || 8086;

  diag "Spawning proxy on port $port";
  my $sock = listen($port);

  my $url = "http://localhost:$port";
  ok(defined($sock), "Listening on $url");

  async {
    #diag "Spawned server";

    while (1) {
      my $hdl = $sock.accept;

      my $request = =$hdl;
      $request ~~ s:Perl5/\s+$//;
      #diag $request;
      if ($request ~~ rx:Perl5{^GET /stop-server/}) {
        last();
      };

      while (readline($hdl) ~~ rx:Perl5/\S/) { 1 };
      $hdl.print( "HTTP/1.0 200 OK\r\n"
                ~ "Content-Type: text/plain; charset=UTF-8\r\n"
                ~ "Server: Fake local Pugs HTTPd\r\n"
                ~ "X-Original-Request: $request\r\n"
                ~ "\r\n"
                ~ $expected );
      $hdl.flush();
      $hdl.close;
    };

  };

  $url;
};

my $base_url = spawn_server( 8086 );

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", ">");

if($?OS eq any<MSWin32 mingw cygwin>) {
  $pugs = 'pugs.exe';
};

sub run_pugs ($c) {
  my $tempfile = "temp-ex-output";
  my $command = "$pugs $c $redir $tempfile";
  diag $command;
  system $command;
  my $res = slurp $tempfile;
  unlink $tempfile;
  return $res;
}

# Now talk back to ourselves via a child process:
# (which doesn't work, as Haskell suspends all (userspace-)
#  threads while running an external command)

for @urls -> $t_url {
  my $url = $t_url;
  $url ~~ s:perl5/%s/$base_url/;
  my $inc = map {qq! "-I$_"!}, @INC;

  # Will block forever
  # my $output = run_pugs(qq! $inc -MLWP::Simple -e "getprint('$url')" !);
  my $output = run_pugs(qq! -e "print 'Skipping until we get async really working'" !);
  is($output, $expected, "getprint() works", :todo);
};

get("$base_url/stop");
