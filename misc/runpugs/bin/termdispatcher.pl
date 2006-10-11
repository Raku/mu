
use warnings;
use strict;
# 
# based on testmsg.pl from "Advanced Perl Programming"
#
use lib '../lib/';
use WebTerminal::Dispatcher;


my $id=$ARGV[0]||-1;
my $cmd=$ARGV[1] || 'my $a='.$id.';say "Hello, $a";';
#my $cmd='my $a='.$id.';say "Hello, $a";';
my $prompt='pugs> ';
print "Sending msg $id: $cmd\n";

my $reply = &WebTerminal::Dispatcher::send($id,$prompt.$cmd);
print $reply;

