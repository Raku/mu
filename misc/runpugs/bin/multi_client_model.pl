#!/usr/bin/perl
# run with perl -Mthreads 
 
use 5.008;             # 5.8 required for stable threading
use warnings;
use strict;

use threads;           # pull in threading routines

use lib '../lib/';
use Web::Terminal::Settings;
use Web::Terminal::Dispatcher3;
#$Web::Terminal::Settings::port=2058;
my $v=0;
my $hist=0; # don't display command history

my @threads=();
my $n_threads=10;
our $nclients_serial=100;

for my $i (0..$n_threads-1) {
	$threads[$i]=threads->create("client_session",$i);
}

# program exits when first thread exits
$threads[0]->join(); 


sub client_session {
my $i=shift;
my $w_max=60;
my $w_min=1;

# All probabilities are in %

# Application number, assuming 2 apps, 0=release, 1=devel
my $app_prob=30; 

# Number of commands
my $n_max=20;
my $n_min=1;

# Chance of abort (only meaningful in test mode)
my $abort_prob=10;

# Chance of proper quit
my $quit_prob=50;

# Time between commands
my $t_max=10;
my $t_min=1;

# Choice of commands
my @commands=(
'say "Hello, world!"',
'my $answer=42;',
'42',
'6*7',
'(1,2,3)>>*<<(4,5)',
'for (1..50) { say $^a xx $^a }',
'sub f($x) {$x*$x}',
'say &f(5)',
'[\+] [1,2,3,4,5]',
'[\+] (1,2,3,4,5)',
);
for my $j (1..$nclients_serial) {
# All probabilities are in %
# IP address
my $n1=int(rand(253))+1;
my $n2=int(rand(253))+1;
my $ip="192.168.$n1.$n2";
# Session id
my $nid=crypt(rand(),'WV');
$nid=~tr/.\//WV/;
$nid=~s/^WV//;
my $now=time()-1159056000; # 36 year, 275 days offset
my $id=$nid.$now;

# Application number, assuming 2 apps, 0=release, 1=devel
my $throw=rand(100);
my $app=($throw<$app_prob)?0:1;

# Number of commands
my $ncommands=int(rand($n_max))+$n_min;

# Chance of abort (only meaningful in test mode)
$throw=rand(100);
my $abort=($throw<$abort_prob)?1:0;

# Chance of proper quit
$throw=rand(100);
my $quit=($throw<$quit_prob)?1:0;

print $i*$nclients_serial+$j,": Client $ip/$id, app: $app, #commands: $ncommands, quit: $quit, abort: $abort\n",'-' x 80,"\n";# if $v;
my $cmd='1';
my $reply=&send_cmd($cmd,$id,$ip,$app,);
if ($cmd ne $reply) {
print "$i:WARNING: $cmd<>$reply\n" if $v;
}
for my $n (1..$ncommands) {
	my $ncmd=int(rand(20));
my $cmd=($ncmd<scalar @commands)?$commands[$ncmd]:&create_command();
print "$i:Cmd $n:\t",$cmd,"\n" if $v;
my $reply=&send_cmd($cmd,$id,$ip,$app,);
if ($cmd ne $reply) {
print "$i:WARNING: $cmd<>$reply\n";# if $v;
}
sleep int(rand($t_max))+$t_min 
}
if($quit) {
&send_cmd(':q',$id,$ip,$app);
} elsif ($abort) {
&send_cmd(':A',$id,$ip,$app);
} #Êotherwise do nothing, server should time the session out
my $wait= int(rand($w_max))+$w_min;
print "\n$i:Waiting for $wait seconds...\n" if $v;
sleep $wait; 
} #Êend of nclients_serial loop

}

sub create_command {
return '<'.scalar(localtime()).'>';
}

sub send_cmd {
my $cmd=shift;
	my $id = shift;
	my $ip = shift;
	my $app = shift;

(my $reply,my $prompt,my $histref) =
&Web::Terminal::Dispatcher3::send($id,$ip,$app,1,$cmd);
if($Web::Terminal::Settings::test) {
$reply=~s/^.*?called\ with\ //;
$reply=~s/\.\s*$//;
}
#print "Reply:",$reply,"\n";
if ($hist) {
print '-' x 40,"\nHistory\n";
for my $entry (@{$histref}) {
print "\t$entry\n";
}
print '-' x 40,"\n";
}
return $reply;
}

__END__

=head1 NAME

Web::Terminal client model

=head1 SYNOPSIS

    use Web::Terminal::Server;
    use  Web::Terminal::Settings;
    &Web::Terminal::Dispatcher::send($id,$ip,$app,$interactive,$cmd);

=head1 DESCRIPTION

This script models the behaviour of a user on a web terminal client.
It connects to the server, issues a number of commands.
The number and content of the commands are determined randomly.
This could be a lot smarter of course, including generating valid code randomly, sequences of lines, ...
=head1 SEE ALSO

L<Web::Terminal::Settings>,
L<Web::Terminal::Dispatcher>,
L<Web::Terminal::Server::Session>,
L<Web::Terminal::Msg>

=head1 AUTHOR

Wim Vanderbauwhede <wim.vanderbauwhede@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006. Wim Vanderbauwhede. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
