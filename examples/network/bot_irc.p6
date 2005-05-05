#!/usr/bin/pugs

use v6;

# version 0.1, michael scherer, misc@zarb.org
# some additions by:
#     stevan little <stevan@iinteractive.com>
#     Luke Palmer
# the first perl6 irc bot ( or at least, what i hope to be the first ).

say "A irc perl 6 bot";

my $nick = @*ARGS[0] // "didie_p6";
my $server = "irc.freenode.net";
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

say "Joined $chan";

while ($ligne = readline($hdl))
{
    chomp($ligne);
    say "Serveur said : $ligne"; # if $debug;

    given $ligne {

        when rx:perl5/^PING/ {
    	    say "Reply to ping";
	        $hdl.say("PONG $nick\n");
	        $hdl.flush;
        }

        when rx:perl5/$nick/ 
          && rx:perl5/^\:(.*?)\!.*?\sPRIVMSG $chan/ {
            my $writer = $1;
            given $ligne {   
                when rx:perl5/\b(?i:hello|hi)\b/ {
                    $hdl.say("PRIVMSG $chan :Hello $writer from a perl 6 irc bot\n");
                    $hdl.flush;
                }
            }
        }
                
    };

}
