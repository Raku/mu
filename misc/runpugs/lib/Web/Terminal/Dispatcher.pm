package Web::Terminal::Dispatcher;
use vars qw( $VERSION );
$VERSION = '0.4.0';
use strict;
use Carp::Assert;
use utf8;
use YAML::Syck;

#
# Vaguely based on testmsg.pl from "Advanced Perl Programming"
#
use lib '.', '../..';    #to keep EPIC happy
use Web::Terminal::Settings;
use Web::Terminal::Msg;
use Exporter;
our @ISA       = qw( Exporter );
our @EXPORT    = qw(send );
our @EXPORT_OK = qw(send );
our %EXPORT_TAGS = (
					 ALL     => [qw( send )],
					 DEFAULT => [],
);
my $v =
  1; #(1 - $Web::Terminal::Settings::daemon)*(1-$Web::Terminal::Settings::test);

# send(id,ip,app,interactive,cmds)
# id is a unique identifier (string)
# ip is the client IP address (string)
# app is a small integer indicating the application to run
# interactive is a flag (1 or 0) indicating if the Server needs to run the app as an interactive session or single-shot
# cmds is the string of commands to be sent to the application
sub send {
	my $id          = shift;
	my $ip          = shift;
	my $app         = shift;
	my $interactive = shift;
	my $cmds        = shift;
	my $host        = $Web::Terminal::Settings::host;
	my $port        = $Web::Terminal::Settings::port;
	my $cmd         = $cmds;

	#WV:   We're using PUGS_SAFEMODE=1 instead
	# But for applications without safe mode, we need this
	if (     $Web::Terminal::Settings::filter
		 and $cmd =~ /$Web::Terminal::Settings::filter_pattern/ )
	{
		my $offending_command = $1 || $2;
		return "Sorry, \'$offending_command\' is not
	    allowed.\n$Web::Terminal::Settings::prompt";
	} else {
		my $conn;
		$conn =
		  Web::Terminal::Msg->connect( $host, $port, \&rcvd_msg_from_server );
		if ( not $conn ) {

			#WV: disabled, too dangerous
			#       system("/usr/bin/perl ../bin/termserv.pl");
			#       sleep 5;
			#    } else {last;}
			return "Sorry, the pugs server is not running.";
		} else { # Create the YAML-encoded message to be sent to the Server
			my $msg = YAML::Syck::Dump(
										{
										  id  => $id,
										  ip  => $ip,
										  app => $app,
										  ia  => $interactive,
										  cmd => $cmd
										}
			);
			print STDERR "Sending message to server: $msg\n", '#' x 70, "\n"
			  if $v;
			# Send it
			$conn->send_now($msg);
			print STDERR "done\n" if $v;
			# Wait for reply
			( my $rmesg, my $err ) = $conn->rcv_now();
			print STDERR
			  "Received reply from server: $rmesg (Error msg:$err)\n", '#' x 70,
			  "\n"
			  if $v;
			# Decode reply  
			my $rmesgref = YAML::Syck::Load($rmesg);
			my $rid      = $rmesgref->{id};
			my $reply    = $rmesgref->{msg};
			my $histref  = $rmesgref->{recent};
			my $prompt   = $rmesgref->{prompt};
			# Tear down TCP connection
			$conn->disconnect();

			if ( "$id" ne "$rid" ) {
				print "Terminal server returned wrong id: $rid, should be $id"
				  if $v;
				return "Sorry, the pugs session died.";
			}
			# The next bit is purely for testing
			if (     $Web::Terminal::Settings::test == 1
				 and $cmd ne ':A'
				 and $cmd ne ':q'
				 and $cmd !~ /Web::Terminal/ )
			{
				my $cmdreply = $reply;
				$cmdreply =~ s/^.*?called\ with\ //;
				$cmdreply =~ s/\.\s*$//;
				if ( $cmdreply ne $cmd and $cmd != 1 ) {
					print "D: Application returned '$reply'<>'$cmd':",
					  ( $reply eq '0' ), "<>", ( $reply == 0 ), ';', $rmesg,
					  "\n";
				}
			} elsif ( $Web::Terminal::Settings::test == 1 and $cmd eq ':A' ) {
				print "D: Simulated Abort. Application returned '$reply'\n"
				  if $v;
			} # end of testing stuff
			return ( $reply, $prompt, $histref );
		}
	}
}    # END of send()

sub rcvd_msg_from_server {
	my ( $conn, $msg, $err ) = @_;
	if ( defined $msg ) {
		die "Strange... shouldn't really be coming here\n";
	}
}
1;
__END__

=head1 NAME

Web::Terminal::Dispatcher -- Dispatches commands to a  terminal
session
Requires YAML::Syck.

=head1 SYNOPSIS

   use Web::Terminal::Dispatcher;
   use Web::Terminal::Settings;

    ($reply, $prompt, $histref) =
    &Web::Terminal::Dispatcher::send($sessionid,$ip,$version,$interactive,$cmd);

=head1 DESCRIPTION

This module exports a single subroutine C<send>. The arguments are:
    $sessionid: a string identifying the session
    $ip: the IP address of the client as a dotted-decimal string
    $version: an integer indicating the version of the terminal application to
    be used. The value is the index in the list of C<commands> (see L<Settings.pm>). 
    $interactive: if 1, the session is interactive, else it's batch mode
    $cmd: the actual command to be sent to the terminal application.

The call to C<send> returns a list containing:
    $reply: a string containing the reply from the terminal application
    $prompt: a string containing the prompt from the terminal application
    $histref: a reference to a list of the most recent commands (see
    Settings.pm)

=head1 SEE ALSO

L<Web::Terminal::Settings>,
L<Web::Terminal::Server>,
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
