#!perl6
use v6;

my $port = @ARGS[0] // 8080;

my $sock = listen($port);

say "Listening on port http://localhost:$port/";

while (1) {
    my $hdl = $sock.accept;
    my $thr = async {
        while (readline($hdl) ~~ rx:perl5/\S/) { 1 };
        $hdl.say("HTTP/1.0 200 OK");
        $hdl.say("Content-Type: text/html; charset=UTF-8");
	$hdl.say("Server: Pugs driven HTTP/1.0 server");
        $hdl.say("");
        $hdl.say("<html>");
	$hdl.say("<head><title>Hello from Pugs!</title></head>");
	$hdl.say("<body>");
        $hdl.say("<h1>Hello from <a href='http://pugscode.org/'>Pugs</a> on $*OS!</h1>");
        $hdl.say("<p>We are now { time() } seconds since Y2K epoch.</p>");
        $hdl.say("<p>Your random number of the day is: { int(rand(10)) }</p>");
	$hdl.say("</body></html>");
        $hdl.close;
    };
    say "Created thread: $thr";
}
