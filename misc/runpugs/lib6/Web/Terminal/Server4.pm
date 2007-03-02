package Web::Terminal::Server4;

=pod
The inactive sessions queue starts with n_inactive_max sessions.
Ideally, once it drops to n_inactive_min, it should gradually create 
n_inactive_max-n_inactive_min sessions
To do this reallu asynchronously, we need to let the child handle this
The problem is that the child can't access the counters of the parent.
So I'd just have the child create n_inactive_max-n_inactive_min sessions, gradually e.g. one every 5 minutes
But that might interfere with the cleanup:
The child sleeps for some time, then cleans up
So if we use the same time constant (makes sense),
the we need a counter which is set by the signal, that's all


So child gets a SIGUSR
=> it sets n_new_sessions to max-min if n_new_sessions was 0
=cut

use vars qw( $VERSION );
$VERSION = '0.4.0';
# use utf8; # No UTF, sorry
use strict;
use Carp::Assert;
use YAML::Syck;
use Proc::Daemon;
use lib '.','../..'; #Êto keep EPIC happy
use Web::Terminal::Settings;
use Web::Terminal::Msg;
use Web::Terminal::Server::Session;
our @ISA       = qw( Exporter );
our @EXPORT    = qw( run );
our @EXPORT_OK = qw( run );
our %EXPORT_TAGS = (
					 ALL     => [qw( run )],
					 DEFAULT => [],
);

#$|=1;
$SIG{CHLD} = 'IGNORE';

# The messages contain the session id and the app.
# If the ID does not exist in %active_sessions, create a new session
# Otherwise, write to the terminal and send back the result, again
# with the session id as first line.


#my %sessions = ();    # session_number (from stack) =>  actual session object
#Datastructures per app, so $session{$app}{...}, $inactive[$app][...] or @{$inactive[$app]}
my @active_sessions             = (); # id => session_number for active sessions
my @sessions              = (); # session_number (from stack) =>  actual session object
my @session_numbers_stack = ();  # stack for session numbers, i.e. those not active or inactive!
my @inactive_sessions     = ();  # => stack of session numbers for inactive sessions
#my @unused       = ();  # => hash keyed by session number. value is id.

# Counters.
# initialised via init_sessions()
my @n_sessions          = ();    # total number of sessions
my @n_active_sessions   = ();
my @n_inactive_sessions = ();

#Êlimit number of session from a single IP.
# Problem for folks behind a gateway, but otherwise too easy for DoS 
my %n_sessions_ip = ();    # ip -> nsessions 

# for use by child to know how many new sessions to create
my @n_new_sessions = ();

# Limits
my @n_inactive_min    = @Web::Terminal::Settings::n_inactive_min;
my @n_inactive_max    = @Web::Terminal::Settings::n_inactive_max;
my @n_max             = @Web::Terminal::Settings::n_max;
my $n_sessions_ip_max = $Web::Terminal::Settings::nsessions_ip;
# verbose
my $v                 = 1 - $Web::Terminal::Settings::daemon;
# Child pid
my $childpid;

#-------------------------------------------------------------------------------
# The main method to be called on a server object. Does fork&exec and init + open log
sub run {
	my $host = $Web::Terminal::Settings::host;
	my $port = $Web::Terminal::Settings::port;
	$SIG{USR1} = \&clean_up_timed_out_sessions;
	$SIG{USR2} = \&init_create;
	if ($Web::Terminal::Settings::daemon) {
		Proc::Daemon::Init;
	}

	# fork/exec by the book:
	use Errno qw(EAGAIN);
  FORK: {
		if ( $childpid = fork ) {

			#parent here
			my $log =
"$Web::Terminal::Settings::log_path/$Web::Terminal::Settings::appname.log";
			if ( -e $log ) {
				rename $log, $log . '.' . join( "", localtime );
			}
			open( LOG2, ">$log" );
			print "Parent: init sessions ...\n " if $v;
			&init_sessions();
			print "Parent: create new server..." if $v;
			Web::Terminal::Msg->new_server( $host, $port, \&login_proc );
			print "OK\n" if $v;
			Web::Terminal::Msg->event_loop();
		} elsif ( defined $childpid ) {

			# child here
			&init_child();
			
			while ( getppid() > 10 ) { # a bit ad-hoc. to see if parent is alive
				sleep $Web::Terminal::Settings::check_interval;

				#				print "Child: ", getppid(), "\n" if $v;				
				&call_create(); # can consume a lot of time!
				kill 'USR1', getppid();
			}

			# normally the child restarts the parent if it dies
			if ($Web::Terminal::Settings::test==1) {
			die "No restarting, test phase\n";
			} elsif ($Web::Terminal::Settings::restart_parent==1) {
			print "Restarting server\n" if $v;
			chdir $Web::Terminal::Settings::lib_path;
			exec("$Web::Terminal::Settings::perl $Web::Terminal::Settings::server"
			);
			}
		} elsif ( $! == EAGAIN ) {

			# Maybe ulimit might take us here
			sleep 30;
			redo FORK;
		} else {

			# Or ulimit could take us here
			print "Couldn't fork" if $v;
			die "Can't fork: $!\n";
		}
	}    # FORK
}    # END of run()

#-------------------------------------------------------------------------------
# If you wonder why this is here, read
# Advanced Perl Programming
sub login_proc {

	# Unconditionally accept.
	\&rcvd_msg_from_client;
}

#-------------------------------------------------------------------------------
# General request handler
# Valid messages are either session create request or requests from the web client
# Both types should be sent by Dispatcher::run()
# Or a compatible API :-)
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
			# Next bit is purely for logging.
			my $tpid          = 0;
			my $n_active_sess = scalar keys %{ $active_sessions[$app] };
			my $n_sess        = scalar keys %{ $sessions[$app] };
			if ( exists $active_sessions[$app]{$id} ) {
				my $term = $sessions[$app]{ $active_sessions[$app]{$id} };
				$tpid = $term->{pid};
				print LOG2 scalar(localtime),
				  " : $n_sess/$n_active_sess : $ip : $id : $tpid > ", $cmd,
				  "\n";
				print scalar(localtime),
				  " : $n_sess/$n_active_sess : $ip : $id : $tpid > ", $cmd, "\n"
				  if $v;
			}
			my $replyref;
			if (     $cmd eq 'Web::Terminal::Server::Session.create'
				 and $ip eq '127.0.0.1' )
			{
				print "#### Received session create request\n" if $v;
				my $ret = &create_session($app);
				$replyref = YAML::Syck::Dump(
											  {
												id     => 0,
												msg    => $ret,
												recent => [],
												prompt => ''
											  }
				);
			} else {
				print "    Received ordinary request\n" if $v;
				my $lines   = &termhandler( $id, $ip, $app, $ia, $cmd );
				my @history = ('');
				my $prompt  = $Web::Terminal::Settings::prompt;
				if ( exists $active_sessions[$app]{$id} ) {
					my $term = $sessions[$app]{ $active_sessions[$app]{$id} };
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
		} else {

			# received 0-length message. Ignore.
		}
	}
}    # END of rcvd_msg_from_client

#-------------------------------------------------------------------------------
# The new termhandler
# This routine deals with all 'ordinary' requests
# These are:
#	- new session (from launching run.pugscode.org)
#	- swap between release and devel (toggle button on UI)
#	- actual usage (but not quit commands)
#	- quit command or close browser window
sub termhandler {
	my $id  = shift;
	my $ip  = shift;
	my $app = shift;
	my $ia  = shift;
	my $cmd = shift;
	if ( $ia == 1 ) {    # interactive session
		if ( $n_sessions_ip{$ip} < $n_sessions_ip_max ) {

			# OK, max number of sessions/ip not exceeded
			if ( exists $active_sessions[$app]{$id} ) {

				# session is active
				print "    Connecting session $id to $active_sessions[$app]{$id}\n"
				  if $v;
				my $term = $sessions[$app]{ $active_sessions[$app]{$id} };
				if ( $term->{pid} ) {

					# check for timeout
					$term->{called} = time;

				#if swap to other app
				# swap to other app is like new session except we recycle the id
				# I guess in this case a disconnect/connect should be ok
					if ( $app != $term->{'app'} ) {
						print "*** Swapping from ",$term->{'app'} ," to $app.\n" if $v;
						&deactivate_session( $term->{'app'}, $id, $ip );

				 # then we should check if there is a free session for this $app
						if ( $n_inactive_sessions[$app] > 0
							 and scalar @{ $inactive_sessions[$app] } > 0 )
						{

					 # OK, inactive session available.
					 # There should be 1 at least because of the disconnect call
							my $session_number =
							  &activate_session( $app, $id, $ip );
							my $term = $sessions[$app]{$session_number};
							return $term->{output};
						} else { # if not, create one. but this should not happen as we disconnected
							my $session_number = &create_session($app);
							if ( $session_number != -1 ) {
								my $session_number =
								  &activate_session( $app, $id, $ip );
								my $term = $sessions[$app]{$session_number};
								return $term->{output};
							} else {
								return "Failed to create a new terminal.";
							}
						}
					}    # end of swap app

					# handle history
					push @{ $term->{recent} }, $cmd unless $cmd =~ /^\s*$/;
					if (
						 scalar @{ $term->{recent} } >
						 $Web::Terminal::Settings::nrecent )
					{
						shift @{ $term->{recent} };
					}

					# processes command
					my $lines = $Web::Terminal::Settings::prompt;
					if ( $cmd ne $Web::Terminal::Settings::quit_command ) {
						print "Sending $cmd to the application.\n" if $v;
						$lines = $term->write($cmd);
print "Application returned '$lines'.\n" if $v;
						if ( $term->{error} == 1 ) {
							&kill_session( $app, $id, $ip )
							  ; # Well, really. If the session returns an error, we should kill it.
							$n_sessions_ip{$ip}--;
						}
					} else {    # it's a quit
						print "*** Received a QUIT from $id: $cmd\n" if $v;
						if (
							 scalar @{ $inactive_sessions[$app] } >
							 $n_inactive_min[$app] )
						{       #Êif there are enough inactive sessions
							&kill_session( $app, $id, $ip );
						} else {
							print "Not enough inactive sessions to kill $id: ",scalar @{ $inactive_sessions[$app] },"<>", $n_inactive_sessions[$app],">",$n_inactive_min[$app] ,"?\n" if $v;  
							&deactivate_session( $app, $id, $ip );
						}
						$n_sessions_ip{$ip}--;
						$lines=$Web::Terminal::Settings::quit_message;
					}
					return $lines;
				} else {    # TROUBLE! no pid, so no process.
					return $Web::Terminal::Settings::prompt;
				}
			} else {

				# new session
				if ( $n_inactive_sessions[$app] > 0
					 and scalar @{ $inactive_sessions[$app] } > 0 )
				{

					# activate
					&activate_session( $app, $id, $ip );
					$n_sessions_ip{$ip}++;

					# Check if there are enough inactive sessions left
					# launch new sessions if n_inactive<n_min_inactive
					# and n_sessions < n_max
					if (     $n_inactive_sessions[$app] < $n_inactive_min[$app]
						 and $n_sessions[$app] < $n_max[$app] )
					{

						# asynchronously create new sessions
						&async_init_create();
					}
				} else {    # no inactive sessions, create one if possible
					if ( $n_sessions[$app] < $n_max[$app] )
					{       # a session can be created
						    # synchronously create a new session
						my $ret = &create_session($app);

						# asynchronously create new sessions
						&async_init_create();
						return $ret;
					} else {
						print
"Sorry, I can't run any more sessions.\nPlease try again later.\n"
						  if $v;
						return
"Sorry, I can't run any more sessions.\nPlease try again later.";
					}
				}
			}
		} else {    # max for ip reached
			print LOG2 "MAX nsessions for $ip reached\n";
			print "MAX nsessions for $ip reached\n" if $v;
			return
"Sorry, you can't run more than $n_sessions_ip_max sessions from one IP address.\n";
		}
	} else {    # non-interactive
		print "Non-interactive $id\n" if $v;
		print "$app $ia $id $cmd\n"   if $v;
		my $term = new Web::Terminal::Server::Session(
													   app  => $app,
													   ia   => $ia,
													   id   => $id,
													   cmds => $cmd
		);
		$term->{called} = time;
		$term->{ip}     = $ip;
		my $output = $term->{'output'};
		$term->DESTROY();
		return $output;
	}
}    # END of termhandler()

#-------------------------------------------------------------------------------
# Create a new session
sub create_session {
	my $app = shift;
my $session_number=-3;
	if ( $n_sessions[$app] < $n_max[$app] ) {
		# Yes, there is room to create a new session
		$session_number = pop @{ $session_numbers_stack[$app] };
		my $new_session = new Web::Terminal::Server::Session(
														  app => $app,
														  ia  => 1,
														  id => $session_number,
														  cmds => "42"
		);
		
		if ( $new_session->{'error'} != 1 ) { # all's well!
			$sessions[$app]{$session_number} = $new_session;
			$n_sessions[$app]++;
			push @{ $inactive_sessions[$app] }, $session_number;
			$n_inactive_sessions[$app]++;
		} else {
			# Something went wrong, failed to create a new session
			# push session number back onto stack
			push @{ $session_numbers_stack[$app] }, $session_number;
			$session_number= -1;    # creating session failed
		}
	} else {
		# Alas, no room for a new session.
		$session_number= -2;        # max nsessions reached
	}
	print "+++ Created session  $session_number ($app): tot:$n_sessions[$app]; active: $n_active_sessions[$app];inactive: $n_inactive_sessions[$app]\n" if $v;
	print "+++ Actual occupancy $session_number ($app): tot:",scalar keys %{$sessions[$app]},"; active: ", scalar keys %{$active_sessions[$app]},";inactive: ",scalar @{$inactive_sessions[$app]}," ;free: ",scalar @{$session_numbers_stack[$app]},"\n" if $v;
	assert($n_sessions[$app]==$n_inactive_sessions[$app]+$n_active_sessions[$app]);
	assert(scalar(keys %{$sessions[$app]})==scalar( keys %{$active_sessions[$app]})+scalar( @{$inactive_sessions[$app]}));
	assert(scalar(@{$session_numbers_stack[$app]})+scalar(keys %{$sessions[$app]})==$n_max[$app]);
	
	return $session_number;
}    # end of create_session

#-------------------------------------------------------------------------------
# move a session from inactive to active;
# add it to %active_sessions
sub activate_session {
	my $app                   = shift;
	my $id                    = shift;
	my $ip                    = shift;
	my $active_session_number = pop @{ $inactive_sessions[$app] };
	$n_inactive_sessions[$app]--;
	print "Connecting $id to new session $active_session_number\n" if $v;
#	$unused[$app]{$active_session_number} = $id;
	$n_active_sessions[$app]++;
	$active_sessions[$app]{$id} = $active_session_number;
	my $term = $sessions[$app]{$active_session_number};
	$term->{called} = time;
	$term->{app}    = $app;
	$term->{ip}     = $ip;
	$term->{id}     = $id;
	$n_sessions_ip{$ip}++;
	
	print ">>> Connected session $active_session_number ($app,$id,$ip): tot:$n_sessions[$app]; active: $n_active_sessions[$app];inactive: $n_inactive_sessions[$app] ;per ip: $n_sessions_ip{$ip}\n" if $v;
	print ">>> Actual occupancy  $active_session_number ($app,$id,$ip): tot:",scalar keys %{$sessions[$app]},"; active: ", scalar keys %{$active_sessions[$app]},";inactive: ",scalar @{$inactive_sessions[$app]}," ;free: ",scalar @{$session_numbers_stack[$app]},"\n" if $v;
	assert($n_sessions[$app]==$n_inactive_sessions[$app]+$n_active_sessions[$app]);
	assert(scalar(keys %{$sessions[$app]})==scalar( keys %{$active_sessions[$app]})+scalar( @{$inactive_sessions[$app]}));
	assert(scalar(@{$session_numbers_stack[$app]})+scalar(keys %{$sessions[$app]})==$n_max[$app]);
	
	return $active_session_number;
}    # end of activate_session()

#-------------------------------------------------------------------------------
sub deactivate_session {
	my $app   = shift;
	my $id    = shift;
	my $ip    = shift;
	my $term  = $sessions[$app]{ $active_sessions[$app]{$id} };
	my $ncmd  = $Web::Terminal::Settings::reset_command;
	my $lines = $term->write($ncmd);
	print "Disconnecting $id from session $active_sessions[$app]{$id}\n" if $v;
	$term->{recent} = [];
	$term->{called} = 0;

	#	my $ip=$term->{ip} ;
	$term->{ip} = '127.0.0.1';

	#	my $app                     = $term->{app};
	$term->{app} = -1;
	$term->{id}  = '';
	my $inactive_session_number = $active_sessions[$app]{$id};
	delete $active_sessions[$app]{$id};
	$n_active_sessions[$app]--;
	push @{ $inactive_sessions[$app] }, $inactive_session_number;
	$n_inactive_sessions[$app]++;
	$n_sessions_ip{$ip}--;
	print "<<< Disconnected session $inactive_session_number ($app,$id,$ip): tot:$n_sessions[$app]; active: $n_active_sessions[$app];inactive: $n_inactive_sessions[$app]; per ip: $n_sessions_ip{$ip}\n" if $v;
	print "<<< Actual occupancy     $inactive_session_number ($app,$id,$ip): tot:",scalar keys %{$sessions[$app]},"; active: ", scalar keys %{$active_sessions[$app]},";inactive: ",scalar @{$inactive_sessions[$app]}," ;free: ",scalar @{$session_numbers_stack[$app]},"\n" if $v;
	assert($n_sessions[$app]==$n_inactive_sessions[$app]+$n_active_sessions[$app]);
	assert(scalar(keys %{$sessions[$app]})==scalar( keys %{$active_sessions[$app]})+scalar( @{$inactive_sessions[$app]}));
	assert(scalar(@{$session_numbers_stack[$app]})+scalar(keys %{$sessions[$app]})==$n_max[$app]);

	return $inactive_session_number;
}    # end of deactivate_session()

#-------------------------------------------------------------------------------
sub kill_session {
	my $app            = shift;
	my $id             = shift;
	my $ip             = shift;
	my $session_number = $active_sessions[$app]{$id};

	#	my $app = $sessions[$app]{ $session_number}->{app};
	#	my $ip = $sessions[$app]{ $session_number}->{ip};
#	my $spid = $sessions[$app]{$session_number}->{pid};
	$sessions[$app]{$session_number}->DESTROY();
	delete $sessions[$app]{$session_number};
	$n_sessions[$app]--;
	# if it has an id, then it must be an active session, isn't it?
	delete $active_sessions[$app]{$id};
	$n_active_sessions[$app]--;

	push @{ $session_numbers_stack[$app] }, $session_number;

	# kill 9,$spid;
	$n_sessions_ip{$ip}--;
	print "--- Killed session   $session_number ($app,$id,$ip): tot:$n_sessions[$app]; active: $n_active_sessions[$app];inactive: $n_inactive_sessions[$app] ;per ip: $n_sessions_ip{$ip}\n" if $v;
	print "--- Actual occupancy $session_number ($app,$id,$ip): tot:",scalar keys %{$sessions[$app]},"; active: ", scalar keys %{$active_sessions[$app]},";inactive: ",scalar @{$inactive_sessions[$app]}," ;free: ",scalar @{$session_numbers_stack[$app]},"\n" if $v;
	assert($n_sessions[$app]==$n_inactive_sessions[$app]+$n_active_sessions[$app]);
	assert(scalar(keys %{$sessions[$app]})==scalar( keys %{$active_sessions[$app]})+scalar( @{$inactive_sessions[$app]}));
	assert(scalar(@{$session_numbers_stack[$app]})+scalar(keys %{$sessions[$app]})==$n_max[$app]);

	return $session_number;
}    # END of kill_session()

#-------------------------------------------------------------------------------
# The child wakes up every $timeout_idle seconds and sends a SIGUSR1 to the parent to activate clean_up_timed_out_sessions()
# This routine checks for sessions that have been "active" for too long without activity from the client.
  
sub clean_up_timed_out_sessions() {

	# Cleaning up timed-out sessions
	my $now = time();
	my @cleaned_up=(0,0);
	for my $app ( 0 .. @Web::Terminal::Settings::commands - 1 ) {
		for my $id ( keys %{ $active_sessions[$app] } ) {
			if ( exists $active_sessions[$app]{$id} ) {
				my $term = $sessions[$app]{ $active_sessions[$app]{$id} };
				my $then = $term->{called};
				if ( $now - $then > $Web::Terminal::Settings::timeout_idle ) {
					my $tpid = $term->{pid};
					my $ip   = $term->{ip};
					my $app  = $term->{app};
					$n_sessions_ip{$ip}--;
					$cleaned_up[$app]++;
					if ( $n_inactive_sessions[$app] <= $n_inactive_min[$app] ) {						
						&deactivate_session( $app, $id, $ip );
					} else {
						&kill_session( $app, $id, $ip );
					}
					print LOG2 "Cleaned up $ip : $id : $tpid\n";
					print "Cleaned up $ip : $id : $tpid\n" if $v;
				}
			}
		}
		if ($cleaned_up[$app]>0) { 
		print "/// Cleaned up $cleaned_up[$app] session(s) ($app): tot:$n_sessions[$app]; active: $n_active_sessions[$app];inactive: $n_inactive_sessions[$app]\n" if $v;
		print "/// Actual occupancy now ($app):    tot:",scalar keys %{$sessions[$app]},"; active: ", scalar keys %{$active_sessions[$app]},";inactive: ",scalar @{$inactive_sessions[$app]}," ;free: ",scalar @{$session_numbers_stack[$app]},"\n" if $v;
		assert($n_sessions[$app]==$n_inactive_sessions[$app]+$n_active_sessions[$app]);
		assert(scalar(keys %{$sessions[$app]})==scalar( keys %{$active_sessions[$app]})+scalar( @{$inactive_sessions[$app]}));
		assert(scalar(@{$session_numbers_stack[$app]})+scalar(keys %{$sessions[$app]})==$n_max[$app]);
		
		}
	}
}    # END of clean_up_timed_out_sessions()

#-------------------------------------------------------------------------------
sub init_sessions {
	for my $app ( 0 .. @Web::Terminal::Settings::commands - 1 ) {
		@{ $session_numbers_stack[$app] } = 1 .. $n_max[$app];
		$n_inactive_sessions[$app] = 0;
		$n_active_sessions[$app]   = 0;
		$n_sessions[$app]          = 0;
		 @{ $inactive_sessions[$app] }=();
		  %{ $sessions[$app]}=();
		  
		for my $i ( 1 .. $Web::Terminal::Settings::npreloaded_sessions[$app] ) {
			my $ret = &create_session($app);
			print $ret>0?'OK: $ret':'NOK',': #sesssions: ', scalar( @{ $inactive_sessions[$app] } ), ' for app ',
			  $app, "\n"
			  if $v;
		}
	}
}    # END of init_session()

#-------------------------------------------------------------------------------
# init_create gets called by SIGUSR2
# all it does is set the counter @n_new_sessions (count zero!)
sub init_create {

	#    sleep 3;
	for my $app ( 0 .. @Web::Terminal::Settings::commands - 1 ) {
		if ( $n_new_sessions[$app] == 0 ) {
			$n_new_sessions[$app] =
			  $n_inactive_max[$app] - $n_inactive_min[$app];
		}
	}
}    # END of init_create()

#-------------------------------------------------------------------------------
sub async_init_create {

	# we use a SIGUSR2 to the child for this
	kill 'USR2', $childpid;    # or die $!;
	print "Initiated create_session() call, now returning\n"
	  if $v;
}    # END of async_init_create()

#-------------------------------------------------------------------------------
sub call_create {
	for my $app ( 0 .. @Web::Terminal::Settings::commands - 1 ) {
		
	#	print "\tTime to create new sessions for $app? " if $v;
		while ( $n_new_sessions[$app] > 0 ) {
			 print "\tYes, initiating a create_session() call...\n" if $v;
			$n_new_sessions[$app]--;
			use Web::Terminal::Dispatcher3;
			( my $ret, my $p, my $h ) =
			  Web::Terminal::Dispatcher3::send( 0, '127.0.0.1', 1, 1,
									  'Web::Terminal::Server::Session.create' );
			print "create_session() call returned $ret:",
			  ( $ret < 1 ) ? "ERROR" : "OK", "\n" if $v;
			  sleep $Web::Terminal::Settings::create_interval;
		} 
		#else {
	#		 print "No.\n" if $v;
		#}
	}
}    # END of call_create()

#-------------------------------------------------------------------------------
sub init_child {
	for my $app ( 0 .. @Web::Terminal::Settings::commands - 1 ) {
		$n_new_sessions[$app] = 0;
	}
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

