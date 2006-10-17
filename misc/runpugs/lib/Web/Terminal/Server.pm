package Web::Terminal::Server;

use vars qw( $VERSION );
$VERSION = '0.2.0';
use utf8;
use strict;

use YAML::Syck;
#use lib '.';

use Proc::Daemon;
use Web::Terminal::Settings;
use Web::Terminal::Msg;
use Web::Terminal::Server::Session;

our @ISA         = qw( Exporter );
our @EXPORT   = qw( run );
our @EXPORT_OK   = qw( run );
our %EXPORT_TAGS = (
        ALL     => [qw( run )],
        DEFAULT => [],
        );

$SIG{CHLD} = 'IGNORE';

=pod
The messages contain the session id.
If the ID does not exist in %terminals, create a new session
Otherwise, write to the terminal and send back the result, again
with the session id as first line.
=cut

our %terminals=();
our %lastcalled=();
our %sessions_per_ip=();

sub termhandler {
	my $id  = shift;
    my $ip=shift;
	my $cmd = shift;
if(scalar(keys %lastcalled)>$Web::Terminal::Settings::nsessions){ # each pugs takes 1% of feather's MEM!
    return "Sorry, I can't run any more sessions.\nPlease try again later.";
} else {
	$lastcalled{$id}=time;
	if ( exists $terminals{$id} ) {
		my $term  = $terminals{$id};
		my $lines = $term->write($cmd);
		if ( $cmd eq $Web::Terminal::Settings::quit_command ) {
			delete $terminals{$id};
            delete $lastcalled{$id};
            $sessions_per_ip{$ip}--;
		}
        if ($lines=~/Aborted/s) {
             delete $terminals{$id};
             delete $lastcalled{$id};
            $sessions_per_ip{$ip}--;
        }
		return $lines;
	} else {
        if ($sessions_per_ip{$ip}>$Web::Terminal::Settings::nsessions_ip) {
         return "Sorry, you can't run more than
         ${Web::Terminal::Settings::nsessions_ip} sessions from one IP
         address.\n";   
        } else {
            $sessions_per_ip{$ip}++;
		$terminals{$id} = new Web::Terminal::Server:Session();
		my $term = $terminals{$id};
		return $term->{'init'};
        }
	}
}
} # of termhandler

sub rcvd_msg_from_client {
	my ( $conn, $msg, $err ) = @_;
	if ( defined $msg ) {
		my $len = length($msg);
		if ( $len > 0 ) {
#			( my $id, my $ip, my $cmd ) = split( "\n", $msg, 3 );
print "MSG:", $msg;			
			my $mesgref=YAML::Syck::Load($msg);
			 my $id=$mesgref->{id};
             my $ip=$mesgref->{ip};
             my $cmd=$mesgref->{cmd};
            $cmd=pack("U0C*", unpack("C*",$cmd));
#            print "$id($ip): ",$cmd,"\n";
			my $lines = &termhandler( $id, $ip, $cmd );
            my $replyref=YAML::Syck::Dump({id=>$id,msg=>$lines});
#			$conn->send_now("$id\n$lines");
 			$conn->send_now($replyref);

		}
	}
}

sub login_proc {
	# Unconditionally accept
	\&rcvd_msg_from_client;
}

sub run {
my $host=$Web::Terminal::Settings::host;#shift;
my $port=$Web::Terminal::Settings::port;#shift;

$SIG{USR1}=\&timeout;
#Proc::Daemon::Init;

# fork/exec by the book:
use Errno qw(EAGAIN);
my $pid;
FORK: {
if ($pid=fork) {
    #parent here
    Web::Terminal::Msg->new_server( $host, $port, \&login_proc );
    Web::Terminal::Msg->event_loop();
} elsif (defined $pid) {
   # child here
   while (getppid()>10) { # a bit ad-hoc.
       sleep $Web::Terminal::Settings::check_interval;
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
    for my $id (keys %lastcalled) {
        my $then=$lastcalled{$id};
        if ($now-$then>$Web::Terminal::Settings::timeout_idle) {
        if(exists $terminals{$id}) {
            $terminals{$id}->write(':q');
            delete $terminals{$id};
            }
            if (exists $lastcalled{$id}) {
            delete $lastcalled{$id};
            }
        }
    }
}

1;
