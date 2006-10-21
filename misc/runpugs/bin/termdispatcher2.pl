
use warnings;
use strict;
# 
# based on testmsg.pl from "Advanced Perl Programming"
#
use lib '../lib/';
use Web::Terminal::Settings;
use Web::Terminal::Dispatcher;
$Web::Terminal::Settings::port=2058;


my $id=$ARGV[0]||-1;
my $cmd=$ARGV[1] || 'my $a='.$id.';say "Hello, $a";';
#my $cmd='my $a='.$id.';say "Hello, $a";';
#my $prompt='pugs> ';
my $prompt='Prelude> ';
print "Sending msg $id: $cmd\n";
my $ip="127.0.0.1";
(my $reply,my $histref) = &Web::Terminal::Dispatcher::send($id,$ip,$prompt.$cmd);
print $reply;
print "\nHistory\n";
for my $entry (@{$histref}) {
print "\t$entry\n";
}

