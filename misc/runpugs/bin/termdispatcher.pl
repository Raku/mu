
use warnings;
use strict;
# 
# based on testmsg.pl from "Advanced Perl Programming"
#
#use lib '../lib/';
use Web::Terminal::Settings;
use Web::Terminal::Dispatcher;
#$Web::Terminal::Settings::port=2058;


my $id=$ARGV[0]||-1;
my $cmd=$ARGV[1] || 'print "hello\n";say "there";print 4';
print "Sending msg $id: $cmd\n";
my $ip="127.0.0.1";
(my $reply,my $prompt,my $histref) =
&Web::Terminal::Dispatcher::send($id,$ip,1,1,$cmd);
print $reply;
print "\nHistory\n";
for my $entry (@{$histref}) {
print "\t$entry\n";
}

