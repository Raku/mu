use v6;
my $port = @ARGS[0] // 8080;
my $sock = listen($port);

say "Listening on port $port...";

while (1) {
    my $hdl = $sock.accept;
    my $thr = threads::create({
        $hdl.say("Hello, Pugs! Fnord { rand() }");
        $hdl.close;
    });
    say "Created thread: $thr";
}
