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

my $page;
my @order;
my @flip;
my @up;

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
    if (param('action') eq 'new') { $flip=''; $up=''; $order=''; }
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
            if ( @order[@flip[0]] eq @order[@flip[1]] ) {
                @up[@flip[0]]=@up[@flip[1]]=1;
            }
            @flip=(); 
        }
        if (@flip[0]>0) {
            @flip[1]=$id;
         } else {
            @flip[0]=$id;
        }
    }
    SessionSet('order',join('/',@order));
    SessionSet('flip',join('/',@flip));
    SessionSet('up',join('/',@up));
}
SessionSet('username',$username);

PageHeader(charset=>'utf-8').print;
say "<html><head><title>Perl 6</title><link type=\"text/css\" rel=\"stylesheet\" href=\"/game.css\" /></head><body>";

#--

if ($page eq 'askname') {
    say '<form method="get">Please enter your name!<input type="text" name="name" value="' ~ param('name') ~ '"/><input type="submit" /></form>';
}

if ($page eq 'game') {
    say 'Welcome '~$username~'! <a href="?action=logout">I\'m not '~$username~'</a> <a href="?action=new">Start new game</a>';
    say '<hr />';
    say '<div class="board">';
    my $img=1;
    for (1..4) {
        for (1..6) {
            print '<a href="?action=flip&id='~$img~'">';
            print '<img src="/pics/card' ~ ( (@up[$img]==1 or @flip[0]==$img or @flip[1]==$img) ?? @order[$img] :: 'back' ) ~ '.jpg" />';
            print '</a>';
            $img++;
        }
        say '<br />';
    }
    say '</div>';
}

#--
say "</body></html>";

SessionDestroy();
