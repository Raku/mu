#!/usr/bin/perl -T -w

#############################################################################
#                                                                           #
# This program is free software; you can redistribute it and/or             #
# modify it under the terms of the GNU General Public License               #
# as published by the Free Software Foundation; either version 2            #
# of the License, or (at your option) any later version.                    #
#                                                                           #
# This program is distributed in the hope that it will be useful,           #
# but WITHOUT ANY WARRANTY; without even the implied warranty of            #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
# GNU General Public License for more details.                              #
#                                                                           #
# You should have received a copy of the GNU General Public License         #
# along with this program; if not, write to the Free Software Foundation,   #
# Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.           #
#############################################################################

use strict;
use utf8;
use lib '../lib/';
use Web::Terminal::Settings;
use Web::Terminal::Dispatcher3;
#$Web::Terminal::Settings::port=2058;
#push (@INC, "$wwwpath$htmlpath$project");

my $MAX_SIZE_UPLOAD = 64;
use CGI qw(:standard);
if ($MAX_SIZE_UPLOAD) { $CGI::POST_MAX=1024 * $MAX_SIZE_UPLOAD; }
#use CGI::Carp qw(fatalsToBrowser);
use HTML::Entities;

#CGI::nph();   # Treat script as a non-parsed-header script
$ENV{PATH} = ""; # no PATH should be needed
$ENV{SAFE_MODE}=1;

my $query=new CGI;

my $sessionid=$query->param("sessionid");

if (not $sessionid) {
my $nid=crypt(rand(),'WV');
$nid=~tr/.\//WV/;
$nid=~s/^WV//;
my $now=time()-1159056000; # 36 year, 275 days offset
$sessionid=$nid.$now;
}

my $ip=$ENV{'REMOTE_ADDR'};
#my $ip="127.0.0.".int(rand(100));
#if ($ip eq '86.0.200.34') {
#$ip='127.'.int(rand(100)).'.'.int(rand(100)).'.'.int(rand(100));
#}
my $prompt=$Web::Terminal::Settings::prompt;
######### MAIN SITEMANAGER PROGRAM ###################

if ( $query->param()) {      # an action has been chosen
    my $cmd=$query->param("cmd");    
    my @cmdlines=split("\n",$cmd);
        for my $cmdline (reverse @cmdlines) {
            $cmdline=~/^\s*$/ && next;
            $cmdline=~/$Web::Terminal::Settings::prompt_pattern/
            && do {
                $cmd=$cmdline;
                $cmd=~s/$Web::Terminal::Settings::prompt_pattern//;
                chomp $cmd;
                last;
        };
        $cmdline=~/$Web::Terminal::Settings::quit_message/ 
        && do {
        $cmd='clear';
        };
        }
    my $action =  $query->param("action")||'runpugs';
    if ($action =~ /^(\w+)$/) {
    	$action = $1;
	        if ($action eq "runpugs") {
		        &runpugs($query,$cmd,$sessionid,$ip);
	        } 
    } else {            # no action has been taken, display login page
       my $warning_message="Action has illegal chars: $action";
       &runpugs($query,'init',$sessionid,$ip);
    }
} else {
	 &runpugs($query,'',$sessionid,$ip);
}
###################### END MAIN ##############################
=pod
runpugs receives a command and a session id and passes it on to the Dispatcher.
It returns the result
For the easy, simple version, the command is the last non-blank line of the form.
=cut

sub runpugs {
    my $query=shift;
    my $cmd=shift;
    my $sessionid=shift;
    my $ip=shift;
    my $dev=$query->param('reldev');#||0;
    if ($dev!=0){
    $dev=1;
    }
    my $devc='checked';
    my $relc='';
    if($dev==1) {
        $devc='checked';
        $relc='';
        } else {
        $devc='';
        $relc='checked';
        }
    my $html='';
    my $clear=0;
    my $prompt=$Web::Terminal::Settings::prompt;
    my $nprompt=$query->param('prompt')||$Web::Terminal::Settings::prompt;
    my $preply='';    
    if ($query->param('cmd')) {
    $preply=$query->param('cmd');
    }
    my $reply=$Web::Terminal::Settings::prompt;
    my @history=();
    my $prevcmd='';
        if(not $query->param('history') or ($query->param('history') eq '')) {
        } else {
            $cmd=$query->param('history');
        }
        if ($cmd=~//) {}
        if ($cmd=~/clear/) {
        $clear=1;
            $cmd='';
            $preply='';
        } elsif ($cmd!~/^\p{IsASCII}*$/) { #NO UNICODE!
            $cmd='';
            $reply = "Sorry, Unicode is not yet supported.\n".$Web::Terminal::Settings::prompt;
        } else {
            if ($cmd=~/>\s+(\:*help)\b/) {
                $cmd=~s/$1/:h/;
            } elsif ($cmd=~/>\s+(\:*(quit|bye))\b/) {
                $cmd=~s/$1/:q/;
            } 
            ($reply, $nprompt, my $histref) =
            &Web::Terminal::Dispatcher3::send($sessionid,$ip,$dev,1,$cmd);
            if (defined $histref) {
                @history=@{$histref};
                $prevcmd=$history[-1];
            }
            $prompt=$nprompt;
        }
    my $npromptw=HTML::Entities::encode_entities($nprompt);
#    my $replyw="$preply$prompt$prevcmd\n$reply";
    my $replyw="$preply\n$reply";
    if($clear==1) {
        $replyw='';
    }
   my @rows=split("\n",$replyw); 
   my $nrows=scalar @rows;#split("\n",$replyw); 
     ($replyw=~/^\s*$/) && ($nrows=1);
    if ($nrows>20) {$nrows=20;}
    my $historylist="\n";
    for my $entry (@history) {
        my $entryw=HTML::Entities::encode_entities($entry);
        $historylist.='<option value="'.$entryw.'">'.$entryw.'</option>'."\n";
    }
    if($replyw!~/Leaving\ pugs\.$/) {
        $replyw.=$nprompt;
        } 

    open(HTML,"<../data/runpugs_async3.html");
    while(<HTML>) {
        /_HIST_/ && do {
            $html.=$historylist;
            next;
        };
        s/_DEV_/$devc/;
        s/_REL_/$relc/;
    	/input.*name=\"sessionid\"/ && do {
            $html.='<input type="hidden" name="sessionid" value="'.$sessionid.'">'."\n";
            next; 
            };
            s/_PROMPTW_/$npromptw/;
        /_ALL_/ and do {
            chomp $html;
            $html.=$replyw;
            next;
        };
    	/([^\`\\]+$)/ && do {$html.=$1};
    }
	close HTML;

    #my $lang_charset = 'iso-8859-1';
    my $lang_charset = 'utf-8';
         print $query->header(-pragma=>'no-cache',
                      -charset=>$lang_charset,
			      );
	print $html;
}
################## END main_page ######################

