#This is simple server which repeats what is said to it.
#You can connect to the server through telnet.
use v6-alpha;

my $port = @ARGS[0] // 1024;
my $sock = listen($port) or die "Could not open socket\n";
say "$?FILE, ready";
say "port: $port";
while(1) {
	my $client = $sock.accept();
	my $thr = async {
		$client.say("\t\t--Hello from perl6 driven echo server--\n\nAnything you say will be reapeated back to you. Type quit to exit\n\n");
		$client.flush;
	
		while(1) {
			$client.print('echo>');
			$client.flush;
			my $line = readline($client);
			$client.flush;
			next unless($line ~~ m:Perl5/\S/);
			$client.say("server: $line");
			$client.flush;
			last if $line ~~ m:Perl5:i/quit/;
		}
		$client.close;
	};
	say "Created thread: $thr";
}
#TODO: If there is way to get peer's address will be a good idea.
