#!/usr/bin/perl
use strict;
use JSON;
use CGI qw(:standard);
use LWP::UserAgent;

my $cgi = new CGI;
my $json = new JSON;

my $ok = 1;
my $reason = '';

# get 'source' parameter...
my $source = param('source');
# for testing...
##$source = 'games/tic_tac_toe4.pl';

if(!$source) {
    $ok = 0;
    $reason = "The 'source' parameter is missing";
} else {
    #filter source from '..'
    $source =~ s/\.\.//g;
}

unless($source =~ /\.pl$/) {
    $ok = 0;
    $reason = "The 'source' parameter should end in .pl";
}

my $content = undef;
if($ok) {
    # load source file
    my $url = "http://svn.pugscode.org/pugs/examples/" . $source;
    my ($errtext,$text) = &loadFileFromUrl($url);

    if($errtext) {
        $ok = 0;
        $reason = $errtext;
    } else {
        $content = $text;
    }
}

# convert to json...
my $obj = {
    'ok'        => $ok,
    'reason'    => $reason,
    'source'    => $source,
    'content'   => $content
};
my $js  = objToJson($obj);


# write back response
print header( -TYPE => 'text/plain'),  $js;

# load file from Url
sub loadFileFromUrl($)
{
    my $url = shift;
    my $content = undef;
    my $errtext = undef;
    # $.get(url)    
    my $ua = LWP::UserAgent->new;
    $ua->agent("runpugs/0.1");
    my $req = HTTP::Request->new(GET => $url);
    my $res = $ua->request($req);
    if ($res->is_success) {
        $content = $res->content;
    } else {
        $errtext = $res->status_line;
    }
    # return error text and content
    return ($errtext,$content);
}