package WebTerminal::Server;

use vars qw( $VERSION );
$VERSION = '0.1.0';
use utf8;

use strict;
use lib '.';

use Proc::Daemon;
use WebTerminal::Msg;
use WebTerminal::Server::Terminal;

our @ISA         = qw( Exporter );
our @EXPORT   = qw( run );
our @EXPORT_OK   = qw( run );
our %EXPORT_TAGS = (
        ALL     => [qw( run )],
        DEFAULT => [],
        );

$SIG{CHLD} = 'IGNORE';

=pod
The messages should have the session id as first line.
If the ID does not exist in %terminals, create a new session
Otherwise, write to the terminal and send back the result, again
with the session id as first line.
=cut

our %terminals=();
our %sessions_per_ip=();

sub termhandler {
	my $id  = shift;
    my $ip = shift;
	my $cmd = shift;
    if(scalar(keys %terminals)>50){ # each pugs takes 1% of feather's MEM!
        return "Sorry, I can't run any more sessions.\nPlease try again later.";
    } else {
    	if ( exists $terminals{$id} ) {
        	$terminals{$id}->{called}=time;
		    my $term  = $terminals{$id};
    		my $lines = $term->write($cmd);
	    	if ( $cmd eq ':q' ) {
		    	delete $terminals{$id};
                $sessions_per_ip{$ip}--;
    		}
            if ($lines=~/Aborted/s) {
                 delete $terminals{$id};
                $sessions_per_ip{$ip}--;
            }
	    	return $lines;
    	} else {
            if ($sessions_per_ip{$ip}>10) {
                 return "Sorry, you can't run more than 10 sessions from one IP address.\n";   
            } else {
                $sessions_per_ip{$ip}++;
	        	$terminals{$id} = new WebTerminal::Server::Terminal();
	            $terminals{$id}->{called}=time;
        	    $terminals{$id}->{ip}=$ip;
        		my $term = $terminals{$id};
    	    	return $term->{'init'};
            }
	    }
    }   
}

sub rcvd_msg_from_client {
	my ( $conn, $msg, $err ) = @_;
	if ( defined $msg ) {
		my $len = length($msg);
		if ( $len > 0 ) {
			( my $id, my $ip, my $cmd ) = split( "\n", $msg, 3 );
            $cmd=pack("U0C*", unpack("C*",$cmd));
#            print "$id($ip): ",$cmd,"\n";
			my $lines = &termhandler( $id, $ip, $cmd );
			$conn->send_now("$id\n$lines");

		}
	}
}

sub login_proc {
	# Unconditionally accept
	\&rcvd_msg_from_client;
}

sub run {
    my $host=shift;
    my $port=shift;
    $SIG{USR1}=\&timeout;
    Proc::Daemon::Init;
    # fork/exec by the book:
    use Errno qw(EAGAIN);
    my $pid;
    FORK: {
        if ($pid=fork) {
            #parent here
            WebTerminal::Msg->new_server( $host, $port, \&login_proc );
            WebTerminal::Msg->event_loop();
        } elsif (defined $pid) {
           # child here
           while (getppid()>10) { # a bit ad-hoc.
               sleep 300;
                #print getppid(),"\n";
                kill 'USR1',getppid();
            }
        } elsif ($! == EAGAIN) {
            sleep 5;
            redo FORK;
        } else {
            die "Can't fork: $!\n";
        } 
    } # FORK
}

sub timeout() {
    my $now=time();
    for my $id (keys %terminals) {
        my $then=$terminals{$id}->{called};
        if ($now-$then>600) {
        if(exists $terminals{$id}) {
            my $ip=$terminals{$id}->{ip};
            $sessions_per_ip{$ip}--;
            $terminals{$id}->write(':q');
            delete $terminals{$id};
            }
        }
    }
}

1;
