#!perl6
use v6;

my $port = @ARGS[0] // 8080;

my $sock = listen($port);

say "Listening on port http://localhost:$port/";

while (1) {
    my $hdl = $sock.accept;
    my $thr = async {
        $hdl.say("Hello, Pugs!");
        $hdl.say("Greetings from { time() } seconds after epoch.");
        $hdl.say("Your random number of the day is: { int(rand(10)) }");
        $hdl.close;
    };
    say "Created thread: $thr";
}
