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
# no auto flush yet
#$hdl.autoflush(1);

$hdl.say("NICK $nick\nUSER $nick $nick $nick $nick\n");
$hdl.flush;

# first line is not so important, it can be discarded ( or i hope )
my $ligne = readline($hdl);

$hdl.say("JOIN $chan\n");
$hdl.flush;

while ($ligne = readline($hdl))
{
    chomp($ligne);
    say "Serveur said : $ligne" if $debug;

    given $ligne {

        when rx:perl5/^PING/ {
	    say "Reply to ping";
	    $hdl.say("PONG $nick\n");
	    $hdl.flush;
        }

        when rx:perl5/PRIVMSG $chan :$nick: !hello/  {
	    $hdl.say("PRIVMSG $chan :Hello from a perl 6 irc bot\n");
	    $hdl.flush;
        }
    };

}
