package Web::Terminal::Server3;

use vars qw( $VERSION );
$VERSION = '0.3.0';
use utf8;
use strict;

use YAML::Syck;

#use lib '.';

use Proc::Daemon;
use Web::Terminal::Settings;
use Web::Terminal::Msg;
use Web::Terminal::Server::Session;

our @ISA         = qw( Exporter );
our @EXPORT      = qw( run );
our @EXPORT_OK   = qw( run );
our %EXPORT_TAGS = (
	ALL     => [qw( run )],
	DEFAULT => [],
);

#$|=1;
$SIG{CHLD} = 'IGNORE';

# The messages contain the session id.
# If the ID does not exist in %terminals, create a new session
# Otherwise, write to the terminal and send back the result, again
# with the session id as first line.

our %terminals        = ();
our %nsessions_per_ip = ();
our $session_counter  = 0;
our %sessions         = ();    # holds the session objects
our @sessions_stack     = 1 .. $Web::Terminal::Settings::nsessions;
our @sessions_stack_app = ();
my $v = 1 - $Web::Terminal::Settings::daemon;

my $pid;
sub run {
	my $host = $Web::Terminal::Settings::host;
	my $port = $Web::Terminal::Settings::port;

	$SIG{USR1} = \&timeout;
	$SIG{USR2} = \&init_create;

	if ($Web::Terminal::Settings::daemon) {
		Proc::Daemon::Init;
	}

	# fork/exec by the book:
	use Errno qw(EAGAIN);

  FORK: {
		if ( $pid = fork ) {

			#parent here
			my $log =
"$Web::Terminal::Settings::log_path/$Web::Terminal::Settings::appname.log";
			if ( -e $log ) {
				rename $log, $log . join( "", localtime );
			}
			open( LOG2, ">$log" );

			#select LOG2; $|=1; # to switch of buffering
			print "Parent: preload sessions ...\n " if $v;
			print @sessions_stack . join(','), "\n";
			for my $i ( 1 .. $Web::Terminal::Settings::npreloaded_sessions ) {
				for my $app ( 0 .. @Web::Terminal::Settings::commands - 1 ) {
					$app == 0 && next;
					my $ret = &create_session($app);
					print "OK? $ret\n";
				}
			}
			print "Parent: create new server..." if $v;
			Web::Terminal::Msg->new_server( $host, $port,\&login_proc );
			print "OK\n" if $v;
			Web::Terminal::Msg->event_loop();
		} elsif ( defined $pid ) {

			# child here
			while ( getppid() > 10 ) {    # a bit ad-hoc.
				sleep $Web::Terminal::Settings::check_interval;
				print "Child: ", getppid(), "\n" if $v;
				kill 'USR1', getppid();
			}
			print "Restarting server\n" if $v;
			chdir $Web::Terminal::Settings::cgi_path;
			exec(
"$Web::Terminal::Settings::perl ../bin/$Web::Terminal::Settings::server"
			);

		} elsif ( $! == EAGAIN ) {
			sleep 5;
			redo FORK;
		} else {
			print "Couldn't fork" if $v;
			die "Can't fork: $!\n";
		}
	}    # FORK
}

sub login_proc {
        # Unconditionally accept
        \&rcvd_msg_from_client;
}


sub rcvd_msg_from_client {
	my ( $conn, $msg, $err ) = @_;
	if ( defined $msg ) {
#		print "CONN: ",$conn,";MSG:",$msg,";ERR: ",$err,"\n" if $v;
		my $len = length($msg);
		if ( $len > 0 ) {
			my $mesgref = YAML::Syck::Load($msg);
			my $id      = $mesgref->{id};
			my $ip      = $mesgref->{ip};
			my $cmd     = $mesgref->{cmd};
			my $app     = $mesgref->{app};
			my $ia      = $mesgref->{ia};

			#            $cmd=pack("U0C*", unpack("C*",$cmd));
			my $tpid   = 0;
			my $nsess = scalar keys %terminals;
			if ( exists $terminals{$id} ) {
				my $term = $sessions{ $terminals{$id} };
				$tpid = $term->{pid};
				print LOG2 scalar(localtime), " : $nsess : $ip : $id : $tpid > ",
				  $cmd, "\n";
				print scalar(localtime), " : $nsess : $ip : $id : $tpid >
            ", $cmd, "\n" if $v;
			} 
			my $replyref;
			if ($cmd eq 'Web::Terminal::Server::Session.create' and $ip eq '127.0.0.1') {
				print "Received session create request\n" if $v;
#				my $app=0;
#				my $prev=$Web::Terminal::Settings::nsessions;
#				for my $i (0..@Web::Terminal::Settings::commands-1) {
#					if (@sessions_stack_app[$i]<$prev) {$app=$i}
#					$prev=@sessions_stack_app[$i];
#				}
				my $ret=&create_session($app);		
				$replyref = YAML::Syck::Dump(
				{
					id     => 0,
					msg    => $ret,
					recent => [],
					prompt => ''
				}
			);
			} else {
				print "Received ordinary request\n" if $v;
			my $lines = &termhandler( $id, $ip, $app, $ia, $cmd );
			my @history = ('');
			my $prompt = $Web::Terminal::Settings::prompt;
			if ( exists $terminals{$id} ) {
				my $term = $sessions{ $terminals{$id} };
				$prompt = $term->{prompt};
				if ( defined $term->{recent} ) {
					@history = @{ $term->{recent} };
				}
			}			
				$replyref = YAML::Syck::Dump(
				{
					id     => $id,
					msg    => $lines,
					recent => \@history,
					prompt => $prompt
				}
			);			
			}
			$conn->send_now($replyref);

		}
	}
}

sub termhandler {
	my $id  = shift;
	my $ip  = shift;
	my $app = shift;
	my $ia  = shift;
	my $cmd = shift;
	if (   scalar( keys %terminals ) > $Web::Terminal::Settings::nsessions
		or scalar(@sessions_stack) == 0 )
	{    # each pugs takes 1% of feather's MEM!
		print "Sorry, I can't run any more sessions.\nPlease try again later.\n"
		  if $v;
		return "Sorry, I can't run any more sessions.\nPlease try again later.";
	} else {
		if ( exists $terminals{$id} ) {
			print "Connecting to session $id: $terminals{$id}\n" if $v;
			my $term = $sessions{ $terminals{$id} };
			if ( $term->{pid} ) {
				$term->{called} = time;

				#if swap to other app #This will result in a new create()
				if ( $app != $term->{'app'} ) {
					&disconnect_from_session($id);

				 # then we should check if there is a free session for this $app
					if ( @{ $sessions_stack_app[$app] } ) {
						my $counter = &connect_to_session( $app, $id, $ip );
						my $term = $sessions{$counter};
						return $term->{output};
					} else {    # if not, create one
						my $counter = &create_session($app);
						if ( $counter != -1 ) {
							my $counter = &connect_to_session( $app, $id, $ip );
							my $term = $sessions{$counter};
							return $term->{output};
						} else {
							$nsessions_per_ip{$ip}--;
							return "Failed to create a new terminal.";
						}
					}
				}
				push @{ $term->{recent} }, $cmd unless $cmd =~ /^\s*$/;
				if (
					scalar @{ $term->{recent} } >
					$Web::Terminal::Settings::nrecent )
				{
					shift @{ $term->{recent} };
				}
				my $ncmd = $cmd;
				if ( $cmd eq $Web::Terminal::Settings::quit_command ) {
					$ncmd = $Web::Terminal::Settings::reset_command;
				}
				my $lines = $term->write($ncmd);
				if ( $cmd eq $Web::Terminal::Settings::quit_command ) {
					&disconnect_from_session($id);
					$nsessions_per_ip{$ip}--;
					$lines = $Web::Terminal::Settings::quit_message;
				} elsif ( $term->{error} == 1 ) {
					&kill_term($id)
					  ; # Well, really. If the session returns an error, we should kill it.
					$nsessions_per_ip{$ip}--;

					# but maybe we should create a new session right away
					&create_session($app);
				}
				return $lines;
			} else {
				return $Web::Terminal::Settings::prompt;
			}
		} else {
			if ( $nsessions_per_ip{$ip} >
				$Web::Terminal::Settings::nsessions_ip )
			{
				print LOG2 "MAX nsessions for $ip reached\n";
				print "MAX nsessions for $ip reached\n" if $v;
				return
"Sorry, you can't run more than ${Web::Terminal::Settings::nsessions_ip} sessions from one IP address.\n";
			} else {
				print "New session $id\n" if $v;
				$nsessions_per_ip{$ip}++;
				print "$app $ia $id $cmd\n" if $v;

				# then we should check if there is a free session for this $app
				if ( @{ $sessions_stack_app[$app] } ) {
					my $counter = &connect_to_session( $app, $id, $ip );
					print "Connected $id to free session $counter\n" if $v;
					my $term = $sessions{$counter};
					# Every time a free session is taken, create a new session
					# we use a SIGUSR2 to the child for this
					kill 'USR2', $pid or die $!;
					print "Initiated create_session() call, now returning\n" if $v;
					return $term->{output};
				} else {    # if not, create one
					my $counter = &create_session($app);
					if ( $counter != -1 ) {
						my $counter = &connect_to_session( $app, $id, $ip );
						my $term = $sessions{$counter};
						return $term->{output};
					} else {
						$nsessions_per_ip{$ip}--;
						return "Failed to create a new terminal.";
					}
				}
			}
		}
	}
}    # of termhandler

sub create_session {
	my $app = shift;
	if (@sessions_stack) {
		my $session_counter = shift @sessions_stack;
		my $new_session     = new Web::Terminal::Server::Session(
			app  => $app,
			ia   => 1,
			id   => $session_counter,
			cmds => "42"
		);
		if ( $new_session->{'error'} != 1 ) {
			$sessions{$session_counter} = $new_session;
			push @{ $sessions_stack_app[$app] }, $session_counter;
			return $session_counter;
		} else {
			$session_counter--;
			return -1;
		}
	} else {
		return -2;
	}
}    # end of create_session

sub connect_to_session {
	my $app                    = shift;
	my $id                     = shift;
	my $ip                     = shift;
	my $active_session_counter = shift @{ $sessions_stack_app[$app] };
	print "Connecting $id to new session $active_session_counter\n" if $v;
	$terminals{$id} = $active_session_counter;
	my $term = $sessions{ $terminals{$id} };
	$term->{called} = time;
	$term->{ip}     = $ip;
	return $active_session_counter;
}    # end of connect_to_session

sub disconnect_from_session {
	my $id   = shift;
	my $term = $sessions{ $terminals{$id} };
	print "Disconnecting $id from session $terminals{$id}\n" if $v;
	$term->{recent} = [];
	$term->{called} = 0;
	$term->{ip}     = '127.0.0.1';
	my $app              = $term->{app};
	my $inactive_session = $terminals{$id};
	delete $terminals{$id};
	push @{ $sessions_stack_app[$app] }, $inactive_session;
}    # end of connect_to_session


sub timeout() {
	my $now = time();
	for my $id ( keys %terminals ) {
		if ( exists $terminals{$id} ) {
			my $term = $sessions{ $terminals{$id} };
			my $then = $term->{called};
			if ( $now - $then > $Web::Terminal::Settings::timeout_idle ) {
				my $tpid = $term->{pid};
				my $ip  = $term->{ip};
				$nsessions_per_ip{$ip}--;
				&disconnect_from_session($id);

				#				&kill_term($id);
				print LOG2 "Cleaned up $ip : $id : $tpid\n";
				print "Cleaned up $ip : $id : $tpid\n" if $v;
			}
		}
	}
}

sub init_create {
	print "Initiating a create_session() call...\n" if $v;
	use Web::Terminal::Dispatcher3;
	(my $ret,my $p,my $h)=Web::Terminal::Dispatcher3::send(0,'127.0.0.1',1,1,'Web::Terminal::Server::Session.create');
	print "create_session() call returned $ret:",, ($ret<1)?"ERROR":"OK","\n" if $v;
}

sub kill_term {
	my $id = shift;

	# WHY?!
	$sessions{ $terminals{$id} }->DESTROY();
	push @sessions_stack, $terminals{$id};
	delete $sessions{ $terminals{$id} };
	delete $terminals{$id};
}

1;

__END__

=head1 NAME

Web::Terminal::Server -- Server for Web::Terminal
Requires YAML::Syck, Proc::Daemon.

=head1 SYNOPSIS

    use Web::Terminal::Server;
    use  Web::Terminal::Settings;
    &Web::Terminal::Server::run();

=head1 DESCRIPTION

This module exports a single subroutine C<run>, which runs the Web::Terminal
server. See L<Settings> for configuration options.

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
