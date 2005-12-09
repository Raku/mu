#!/usr/bin/pugs

# -----------------------------------------------
#  Copyright (C) 2005 by Andras Barthazi
#  This code is totally free, use it as you want
# -----------------------------------------------
#  !!WARNING!!  UGLY, NON-COMMENTED  !!WARNING!!
# -----------------------------------------------

use v6;
require CGI-0.0.1; set_url_encoding('utf-8');
require 'General.pl';
require 'Cookie.pl';
require 'Session.pl';

#--
my @whoswho = ('', 'A funny Perl 6 book cover page', 'A pug', 'Larry Wall', 'Leo Toetsch', 'Andras Barthazi', 
               'Dan Sugalski', 'Audrey Tang', 'Damian Conway', 'Randal Schwartz', 'A camel', 'A parrot', 'A Haskell symbol');
#--

my $page;
my @order;
my @flip;
my @up;
my $turns=+SessionGet('turns');

my $username=SessionGet('username');
if (param('name')) { $username = param('name'); }
if (param('action') eq 'logout') { $username=''; }

if (!$username or $username eq '') {
    $page = 'askname';
} else {
    $page = 'game';
    my $order=SessionGet('order');
    my $flip=SessionGet('flip');
    my $up=SessionGet('up');
    if (param('action') eq 'new') { $flip=''; $up=''; $order=''; $turns=0; }
    if ($order and $order ne '') {
        @order = split('/',$order);
        @flip = split('/',$flip);
        @up = split('/',$up);
     } else {
        for(1..12) {
            @order[$_*2-1] = $_; @up[$_*2-1]=0;
            @order[$_*2] = $_;   @up[$_*2]=0;
        }
        @flip=();
        for(1..100) {
            my $a = int(rand 24)+1;
            my $b = 24-int(rand 24);
            (@order[$a],@order[$b])=(@order[$b],@order[$a]);
        }
    }
    if (param('action') eq 'flip') {
        my($id)=param('id');
        if (@flip[0]>0 and @flip[1]>0) {
            @flip=();
            $turns++;
        }
        if (@flip[0]>0) {
            @flip[1]=$id;
            if ( @order[@flip[0]] eq @order[@flip[1]] ) {
                @up[@flip[0]]=@up[@flip[1]]=1;
            }
         } else {
            @flip[0]=$id;
        }
    }
    SessionSet('order',join('/',@order));
    SessionSet('flip',join('/',@flip));
    SessionSet('up',join('/',@up));
}
SessionSet('username',$username);
SessionSet('turns',$turns);

PageHeader(charset=>'utf-8').print;
say "<html><head><title>Perl 6</title><link type=\"text/css\" rel=\"stylesheet\" href=\"/game.css\" /></head><body>";

#--

given $page {
    when 'askname' {
        say '<form method="get">Please enter your name!<input type="text" name="name" value="' ~ param('name') ~ '"/><input type="submit" /></form>';
    }

    when 'game' {
        say 'Welcome '~$username~'! <a href="?action=logout">I\'m not '~$username~'</a> <a href="?action=new">Start new game</a>';
        say '<hr />';
        say '<div class="board">';
        my $img=1;
        my $flipped=0;
        for (1..4) {
            for (1..6) {
                if (@up[$img]==1 or @flip[0]==$img or @flip[1]==$img) {
                    print '<img src="/pics/card' ~ @order[$img] ~ '.jpg" title="' ~ @whoswho[@order[$img]]  ~ '"/>';
                    $flipped+=@up[$img];
                 } else {
                    print '<a href="?action=flip&id=' ~ $img ~ '"><img src="/pics/cardback.jpg" /></a>';
                }
                $img++;
            }
            say '<br />';
        }
        say '</div>';
        say '<hr />';
        if ($flipped == 24) {
            say 'Congratulations, you won the game! <a href="?action=new">Start new game</a>';
         } else {
            say 'Turns: ' ~ $turns ~ ' - Flipped: ' ~ $flipped/2 ~ ' pair' ~ ($flipped > 2 ?? 's' !! '' );
        }
    }
}

#--

say "</body></html>";

SessionDestroy();
