#!perl6
use v6;

# version 0.1, michael scherer, misc@zarb.org
# the first perl6 irc bot ( or at least, what i hope to be the first ).

say "A irc perl 6 bot";
my $server = "irc.freenode.net";
my $nick = "didie_p6";
my $chan = "#perl6";
my $debug;

my $hdl = connect($server, 6667);
$hdl.say("NICK $nick\n
USER $nick $nick $nick $nick\n");
$hdl.flush;

my $ligne = readline($hdl);

$hdl.say("JOIN $chan\n");
$hdl.flush;

while ($ligne) {
    chomp($ligne);
    say "Serveur said : $ligne" if $debug;
    if ( $ligne ~~ rx:perl5/^PING/ )
    {
        say "Reply to ping";
        $hdl.say("PONG $nick\n");
        $hdl.flush;
    }

    if ( $ligne ~~ rx:perl5/PRIVMSG $chan :$nick: !hello/ )
    {
        $hdl.say("PRIVMSG $chan :Hello from a perl 6 irc bot\n");
        $hdl.flush;
    }
    $ligne = readline($hdl);
}
