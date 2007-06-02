#!/usr/bin/perl
use strict;
use warnings;
use Repl;
use Web::Terminal::Server::Session;

my $app=(defined $ARGV[0])?$ARGV[0]:2;
my $id=(defined $ARGV[1])?$ARGV[1]:42;
$id='testsession'.$id;

my $session=create_session($app);

my $subref = sub {my $cmd=shift;my $res=$session->write($cmd);chomp $res; return ($res,$session->{'prompt'});};

my $prompt =$session->{'prompt'};
my $motd =$session->{'output'};

my $repl = new Repl {subref=>$subref,prompt=>$prompt,motd=>$motd};

$repl->run();


sub create_session {
	my $app = shift;
		# Create a new session
		my $session_number = $id;
		my $new_session = new Web::Terminal::Server::Session (
														  app => $app,
														  ia  => 1,
														  id => $session_number,
														  cmds => "42"
		);
		#$new_session->init();
		if ( $new_session->{'error'} == 1 ) { 		
			# Something went wrong, failed to create a new session
			print "Something went wrong, failed to create a new session:\n";
			print $new_session->{'error'};
			print $new_session->{'output'};			
		}

		
	return $new_session;
}    # end of create_session
