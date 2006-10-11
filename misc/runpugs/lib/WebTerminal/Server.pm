package WebTerminal::Server;

use vars qw( $VERSION );
$VERSION = '0.1.0';

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
our %lastcalled=();

sub termhandler {
	my $id  = shift;
	my $cmd = shift;
	$lastcalled{$id}=time;
	if ( exists $terminals{$id} ) {
		my $term  = $terminals{$id};
		my $lines = $term->write($cmd);
		if ( $cmd eq ':q' ) {
			delete $terminals{$id};
            delete $lastcalled{$id};
		}
        if ($lines=~/Aborted/s) {
             delete $terminals{$id};
             delete $lastcalled{$id};
        }
		return $lines;
	} else {
		$terminals{$id} = new WebTerminal::Server::Terminal();
		my $term = $terminals{$id};
		return $term->{'init'};
	}
}

sub rcvd_msg_from_client {
	my ( $conn, $msg, $err ) = @_;
	if ( defined $msg ) {

		my $len = length($msg);
		if ( $len > 0 ) {
			( my $id, my $cmd ) = split( "\n", $msg, 2 );

			my $lines = &termhandler( $id, $cmd );
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
   while (1) {
       sleep 600;
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
    for my $id (keys %lastcalled) {
        my $then=$lastcalled{$id};
        if ($now-$then>600) {
            $terminals{$id}->write(':q');
            delete $terminals{$id};
            delete $lastcalled{$id};
        }
    }
}

1;
