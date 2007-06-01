package Web::Terminal::Server;

#The inactive sessions queue starts with n_inactive_max sessions.
#Ideally, once it drops to n_inactive_min, it should gradually create 
#n_inactive_max-n_inactive_min sessions
#To do this reallu asynchronously, we need to let the child handle this
#The problem is that the child can't access the counters of the parent.
#So I'd just have the child create n_inactive_max-n_inactive_min sessions, gradually e.g. one every 5 minutes
#But that might interfere with the cleanup:
#The child sleeps for some time, then cleans up
#So if we use the same time constant (makes sense),
#the we need a counter which is set by the signal, that's all
#So child gets a SIGUSR
#=> it sets n_new_sessions to max-min if n_new_sessions was 0

use vars qw( $VERSION );
$VERSION = '0.4.0';
# use utf8; # No UTF, sorry
use strict;
use Carp::Assert;
use YAML::Syck;
use Proc::Daemon;
use lib '.','../..'; #to keep EPIC happy
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

# verbose
my $v                 = 1;#(1 - $Web::Terminal::Settings::daemon)*(1-$Web::Terminal::Settings::test);

# Datastructures per app, so $session{$app}{...}, $inactive[$app][...] or @{$inactive[$app]}
my @active_sessions       = (); # id => session_number for active sessions
my @sessions              = (); # session_number (from stack) =>  actual session object
my @session_numbers_stack = ();  # stack for session numbers, i.e. those not active or inactive!
my @inactive_sessions     = ();  # => stack of session numbers for inactive sessions

# Counters.
# initialised via init_sessions()
my @n_sessions          = ();    # total number of sessions
my @n_active_sessions   = ();
my @n_inactive_sessions = ();

# Limit number of session from a single IP.
# Problem for folks behind a gateway, but otherwise too easy for DoS 
my %n_sessions_ip = ();    # ip -> nsessions 

# For use by child to know how many new sessions to create
my @n_new_sessions = ();

# Limits
my @n_inactive_min    = @Web::Terminal::Settings::n_inactive_min;
my @n_inactive_max    = @Web::Terminal::Settings::n_inactive_max;
my @n_max             = @Web::Terminal::Settings::n_max;
my $n_sessions_ip_max = $Web::Terminal::Settings::nsessions_ip;

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
				&call_clean_up();
				#kill 'USR1', getppid();
			}

			# The child can restart the parent
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
	my $check='';
	print "CONN:\n" if $v;
				for my $key (keys( %{$conn}) ) {
					print $key,':',$conn->{$key},"\n" if $v;
					if ($key eq 'queue') {
						print "len:",scalar @{$conn->{$key}},"\n" if $v;
						print "cont:",join(';',@{$conn->{$key}}),"\n" if $v;
					}
				}
					print "MSG:<",$msg,">;ERR:<",$err,">\n" if $v;
	my $success=1;
	if ( defined $msg ) {
#				for my $key (keys( %{$conn}) ) {
#					print $key,':',$conn->{$key},"\n";
#					if ($key eq 'queue') {
#						print scalar @{$conn->{$key}};
#						print join(';',@{$conn->{$key}});
#					}
#				}
#					print "CONN: ", ";MSG:",$msg,";ERR: ",$err,"\n" if $v;
		my $len = length($msg);
		if ( $len > 0 ) {
			my $mesgref = YAML::Syck::Load($msg);
			my $id      = $mesgref->{id};
			my $ip      = $mesgref->{ip};
			my $cmd     = $mesgref->{cmd};
			my $app     = $mesgref->{app};
			my $ia      = $mesgref->{ia};
print "MSG LEN $len: id:$id;cmd:$cmd;ip:$ip,app:$app,ia:$ia\n" if $v;
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
			if ( $cmd eq 'Web::Terminal::Server::Session.create'
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
			} elsif ( $cmd eq 'Web::Terminal::Server::Sessions.clean-up'
				 and $ip eq '127.0.0.1' ) { 
				 	print "#### Received sessions clean-up request\n" if $v;
				my $ret = &clean_up_timed_out_sessions();
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
				print "LINES: $lines\n" if $v;
				$check=$lines;
				$replyref = YAML::Syck::Dump(
											  {
												id     => $id,
												msg    => $lines,
												recent => \@history,
												prompt => $prompt
											  }
				);
				
			}
			print "REPLY: $id: $cmd=>$replyref\n" if ($id ne '0' and $v);
			$success=$conn->send_now($replyref);
			if ($id ne '0' and $v) {
			print "send_now() status: $success;\n";
				print "CONN after send_now():\n";
				for my $key (keys( %{$conn}) ) {
					print $key,':',$conn->{$key},"\n";
					if ($key eq 'queue') {
						print "len:",scalar @{$conn->{$key}},"\n";
						print "cont:",join(';',@{$conn->{$key}}),"\n";
					}
				}
				print "\n";
			assert(not($check eq '0' and $cmd ne '1'));
			}
			
		} 
		# It seems for every non-0-length there is a 0-length message. Don't know why, I guess that's down to how Msg does its job. 
		else {
#			$success=-1;
			print "0000 Received 0-length message. Ignoring.\n" if $v;
		}
		
	} else {
		print ".... Received undefined message. Ignoring.\n" if $v;
	}
	assert(defined $msg);
	assert($success!=0);
	return $success;
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

				# if swap to other app
				# swap to other app is like new session except we recycle the id
				# I guess in this case a disconnect/connect should be ok
					if ( $app != $term->{'app'} ) {
						print "*** Swapping from ",$term->{'app'} ," to $app.\n" if $v;
                        # deactivate is problematic, so we just kill.
                        # If there are no inactive sessions left one will be
                        # created
						#&deactivate_session( $term->{'app'}, $id, $ip );
						&kill_session( $term->{'app'}, $id, $ip );

				 # then we should check if there is a free session for this $app

                        if ( not ($n_inactive_sessions[$app] > 0
                              and scalar @{ $inactive_sessions[$app] } > 0 ))
                        { # No inactive session available. Create one.
							my $session_number = &create_session($app);
							if ( $session_number == -1 ) {
								return "Failed to create a new terminal.";
							}
                        }
						my $session_number = &activate_session( $app, $id, $ip );
						my $term = $sessions[$app]{$session_number};
						return $term->{output};
                        
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
if($Web::Terminal::Settings::test==1 and $cmd ne ':A') {
my $reply=$lines;
$reply=~s/^.*?called\ with\ //;
$reply=~s/\.\s*$//;

assert($reply eq $cmd);
} elsif ($Web::Terminal::Settings::test==1 and $cmd eq ':A') {
	print "Simulated Abort. Application returned '$lines'\n" if $v;
}
						if ( $term->{error} == 1 ) {
							&kill_session( $app, $id, $ip );
							   # Well, really. If the session returns an error, we should kill it.
							$n_sessions_ip{$ip}--;
						}
					} else {    # it's a quit
						print "*** Received a QUIT from $id: $cmd\n" if $v;
						if (
							 scalar @{ $inactive_sessions[$app] } <=
							 $n_inactive_min[$app] )
						{       # if there are enough inactive sessions
							print "Not enough inactive sessions, create one
                            before killing $id: ",scalar @{ $inactive_sessions[$app] },"<>", $n_inactive_sessions[$app],">",$n_inactive_min[$app] ,"?\n" if $v;  
							&create_session( $app );
						} 
							&kill_session( $app, $id, $ip );
						$n_sessions_ip{$ip}--;
						$lines=$Web::Terminal::Settings::quit_message;
					}
					return $lines;
				} else {    # TROUBLE! no pid, so no process.
					return $Web::Terminal::Settings::prompt;
				}
			} else {
print "    Session $id ($app,$cmd) is not active:", (defined $active_sessions[$app])?exists( $active_sessions[$app]{$id}):'(empty)',"\n" if $v;
				# new session
			   assert($n_inactive_sessions[$app]==scalar( @{ $inactive_sessions[$app] }));
				if ( $n_inactive_sessions[$app] > 0
					 and scalar @{ $inactive_sessions[$app] } > 0 )
				{

					# activate
					
					my $sessnum=&activate_session( $app, $id, $ip );
					print "Activated session $sessnum\n" if $v;
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
							print "Created a new session $ret\n" if $v;
							my $sessnum=&activate_session( $app, $id, $ip );
							print "Then activated a session $sessnum\n" if $v;
						# asynchronously create new sessions
						&async_init_create();
						my $term = $sessions[$app]{$ret};
						my $motd=$term->{'motd'};
						return $motd;
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
			print "Something went wrong, failed to create a new session:\n";
			print $new_session->{'error'};
			print $new_session->{'output'};
			
			# push session number back onto stack
			push @{ $session_numbers_stack[$app] }, $session_number;
			$session_number= -1;    # creating session failed
		}
	} else {
		# Alas, no room for a new session.
		$session_number= -2;        # max nsessions reached
	}
	print "\n+++ Created session $session_number ($app): tot:$n_sessions[$app]; active: $n_active_sessions[$app];inactive: $n_inactive_sessions[$app]\n" if $v;
	print "+++ Occupancy for $session_number ($app): tot:",scalar keys %{$sessions[$app]},"; active: ", scalar keys %{$active_sessions[$app]},";inactive: ",scalar @{$inactive_sessions[$app]}," ;free: ",scalar @{$session_numbers_stack[$app]},"\n" if $v;
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
# deactivate_session() was meant to take a running session and recycle it
# using a reset_command.
# Unfortunately, if the user leaves the session on an unfinished multi-line,
# the reset_command doesn't get through. 
# So we should check that based on the value of prompt.
# If it's multi-line we need kill & create
# In fact, to ensure we have the latest devel version, kill & create is
# prefered anyway.
# So this routine is obsolete.
sub deactivate_session {
	my $app   = shift;
	my $id    = shift;
	my $ip    = shift;
	my $term  = $sessions[$app]{ $active_sessions[$app]{$id} };
    my $unfinished_multi_line=0;
    my $tprompt=$term->{prompt};
    if ($tprompt=~/$Web::Terminal::Settings::prompt_pattern/ and $tprompt ne
    $Web::Terminal::Settings::prompt) {
    my $unfinished_multi_line=1;
    }
    my $always_kill=1;
    my $recycle=(1-$always_kill) * $unfinished_multi_line; 
	my $inactive_session_number = -3;
    if ($recycle==1) {
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
	$inactive_session_number = $active_sessions[$app]{$id};
	delete $active_sessions[$app]{$id};
	$n_active_sessions[$app]--;
	push @{ $inactive_sessions[$app] }, $inactive_session_number;
	$n_inactive_sessions[$app]++;
	$n_sessions_ip{$ip}--;
    } else {
        # kill the session
        $inactive_session_number=&kill_session($app,$id,$ip);
        # launch a new session. Or is this too direct?
        # Should I use a signal to do this async?
        my $new_session=&create_session($app);
    }
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
	my @cleaned_up_sessions=();
	for my $app ( 0 .. @Web::Terminal::Settings::commands - 1 ) {
		for my $id ( keys %{ $active_sessions[$app] } ) {
			if ( exists $active_sessions[$app]{$id} ) {
				my $term = $sessions[$app]{ $active_sessions[$app]{$id} };
				my $then = $term->{called};
				if ( $now - $then > $Web::Terminal::Settings::timeout_idle ) {
					push @cleaned_up_sessions,$id;
					my $tpid = $term->{pid};
					my $ip   = $term->{ip};
					my $app  = $term->{app};
					$n_sessions_ip{$ip}--;
					$cleaned_up[$app]++;
					if ( $n_inactive_sessions[$app] <= $n_inactive_min[$app] ) {	
						# recycling sessions is problematic
                        # So we just create a new session (sync, so slow) and then kill the
                        # old one.
                        #&deactivate_session( $app, $id, $ip );
					} #else {
						&kill_session( $app, $id, $ip );
					#}
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
	return scalar @cleaned_up_sessions;
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
		  
if (&sane($app)) {
			print "Creating sessions for $app\n" if $v;
		for my $i ( 1 .. $Web::Terminal::Settings::npreloaded_sessions[$app] ) {
            my $ret = &create_session($app);
			print $ret>0?'OK: $ret':"NOK: $ret",': #sesssions: ', scalar( @{ $inactive_sessions[$app] } ), ' for app ',
			  $app, "\n"
			  if $v;
		}
	assert(scalar(@{ $inactive_sessions[$app] })==$Web::Terminal::Settings::npreloaded_sessions[$app]);
	} else {
		print "App $app is not sane. Skipped.\n" if $v; 
}
}
}    # END of init_sessions()
#------------------------------------------------------------------------------
# Check if command is sane (FSDO sane)
# This should be part of settings. 
sub sane {
    my $app=shift;
    my $cmd=$Web::Terminal::Settings::commands[$app];
    $cmd=~s/^.*nice\s+//; # de-nice
    if ($cmd !~/pugs/) {
    	return 1;
    } else {
    	print "PUGS_SAFEMODE=1 $cmd -e \"print 42\" 2>/dev/null\n";
    my $reply=`PUGS_SAFEMODE=1 $cmd -e \"print 42\" 2>/dev/null`;
    if ($reply==42) {
        return 1;
    } else {
        return 0;
    }}
}
#-------------------------------------------------------------------------------
# init_create gets called by SIGUSR2, as raised by async_init_create()
# all it does is set the counter @n_new_sessions (count zero!)
# if counter is 0, it gets reset to max-min
# this avoids potential race hazards 
sub init_create {

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
	kill 'USR2', $childpid;   
	print "Triggered init_create() call with SIGUSR2, now returning\n"
	  if $v;
}    # END of async_init_create()

# Alternatively, we could do this using threads:
# Have a thread instead of the child process.
# Instead of using the SIGUSR, we call init_create directly.
# If $n_new_sessions[$app] is shared, this should work.
# But then we also need a thread for cleaning up etc.
# And once we have these, there's no need to use the socket calls.
# 

#-------------------------------------------------------------------------------
sub call_create {
	for my $app ( 0 .. @Web::Terminal::Settings::commands - 1 ) {
		
	#	print "\tTime to create new sessions for $app? " if $v;
		while ( $n_new_sessions[$app] > 0 ) {
			 print "\tYes, initiating a create_session() call...\n" if $v;
			$n_new_sessions[$app]--;
			use Web::Terminal::Dispatcher;
			( my $ret, my $p, my $h ) =
			  Web::Terminal::Dispatcher::send( 0, '127.0.0.1', 1, 1,
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
# Cleaning up timed-out sessions used to be triggered via a SIGUSR1 from the child to the parent.
# Now we use a socket call, guaranteed no race conditions.
sub call_clean_up {
		
			use Web::Terminal::Dispatcher;
			( my $ret, my $p, my $h ) =
			  Web::Terminal::Dispatcher::send( 0, '127.0.0.1', 1, 1,
									  'Web::Terminal::Server::Sessions.clean-up' );
								  if ($v) {
									  if($ret=~/[a-z]/) {
			print "call_clean_up() call returned: $ret\n";
									  } else {
			print "call_clean_up() call returned: cleaned up <$ret> sessions\n";
		}
		}
			  #( $ret < 1 ) ? "Nothing to clean up" : "OK", "\n" if $v;
		
}    # END of call_clean_up()
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

